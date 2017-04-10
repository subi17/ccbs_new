/* ----------------------------------------------------------------------
  MODULE .......: migration_oper_data_status.p
  TASK .........: Program informs Migration tool when oper data is updated
                  for subscriptions.
                  Timing plan:
                  The program runs periodically and checks time period after
                  the pervious run.
                  This program has locking functionality that prevents
                  running multiple instances.
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 17.3.2017
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Migration/migrationfunc.i}

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR ldCollPeriodStartTS AS DEC NO-UNDO.
DEF VAR ldCollPeriodEndTS AS DEC NO-UNDO. /*now - 1 minute*/
DEF VAR ldaReadDate AS DATETIME.
DEF VAR lcTimePart AS CHAR. /*For log file name*/
DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcMQMessage AS CHAR NO-UNDO.

DEF VAR lcErr AS CHAR NO-UNDO.



DEF TEMP-TABLE ttNotifyList
   FIELD msseq as INT
   INDEX msseq msseq.

FUNCTION fStatusComment RETURNS CHAR
   (iiIn AS INT):
   IF      iiIN EQ 2 THEN RETURN "Completed".
   ELSE IF iiIn EQ 9 THEN RETURN "Completed".
   ELSE IF iiIn EQ 3 THEN RETURN "Failed".

   RETURN "Not completed".

END.

/*Function provides total situation of mentioned subscription for request based
  settings.
  Options that can ve used:
  -Providing status of all Migration requests to output value (usage: reporting
   status in failure case)
  -Providing information if all requests are handled already successfully
   to parameter plgOK (usage: ensuring that that migration is ready)
  */
FUNCTION fTotalSituation RETURNS CHAR
   (iiMsSeq AS INT,
    OUTPUT olgOK AS LOGICAL):
   DEF VAR lcCheckTypes AS CHAR NO-UNDO. /*req types to be checked*/
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcSummary AS CHAR NO-UNDO.

   lcCheckTypes = "8,35".
   olgOK = TRUE.
   DO i = 1 TO NUM-ENTRIES(lcCheckTypes):
      FOR EACH msrequest NO-LOCK where
               msrequest.msseq EQ iiMsSeq AND
               msrequest.reqtype EQ INT(ENTRY(i,lcChecktypes)) AND
               msrequest.reqsource EQ {&REQUEST_SOURCE_MIGRATION}:
         IF msrequest.reqtype EQ {&REQTYPE_BARRING_REQUEST} THEN DO:
         IF NOT (msrequest.reqstatus EQ 2 OR
                 msrequest.reqstatus EQ 9) THEN olgOK = FALSE.

            lcSummary = "Barring: " + msrequest.reqcparam1 + " status " +
                        fStatusComment(msrequest.reqstatus).
         END.

         ELSE IF msrequest.reqtype EQ {&REQTYPE_CONTRACT_ACTIVATION} THEN DO:
           IF NOT (msrequest.reqstatus EQ 2 OR
                   msrequest.reqstatus EQ 9) THEN olgOK = FALSE.
 
            lcSummary = "Contract: " + msrequest.reqcparam3 + " status " +
                        fStatusComment(msrequest.reqstatus).
         END.

      END. /*for each*/
   END. /*type loop*/
END.

/**/
FUNCTION fFailedOperDataSettings RETURNS CHAR
   (idStartTime AS DECIMAL,
    idEndTime AS DECIMAL):
   DEF VAR llgAllOK AS LOGICAL NO-UNDO. /*Not used here*/

   FOR EACH msrequest NO-LOCK where
            msrequest.brand eq "1" and
            msrequest.reqstatus eq 3 and
            msrequest.updatestamp > idStartTime AND
            msrequest.updatestamp <= idEndTime AND
            msrequest.reqsource EQ "35": /*migration*/

      /*if found, report total situation including pending and ok cases to
        help persons to start solving the problems.*/
      FIND FIRST ttNotifyList WHERE
                 ttNotifylist.msseq eq msrequest.msseq NO-ERROR.
      IF NOT AVAIL ttNotifyList THEN DO:
         CREATE ttNotifyList.
         ttNotifyList.msseq = msrequest.msseq.
      END.
   END.
END.


FUNCTION fOKOperDataSettings RETURNS CHAR
   (idStartTime AS DECIMAL,
    idEndTime AS DECIMAL):
   DEF VAR llgAllOK AS LOGICAL NO-UNDO.

   FOR EACH msrequest NO-LOCK where
            msrequest.brand eq "1" and
            msrequest.reqstatus eq 2 and
            msrequest.updatestamp > idStartTime AND
            msrequest.updatestamp <= idEndTime AND
            msrequest.reqsource EQ {&REQUEST_SOURCE_MIGRATION} :
      FIND FIRST ttNotifyList WHERE
                 ttNotifylist.msseq eq msrequest.msseq NO-ERROR.
      IF NOT AVAIL ttNotifyList THEN DO:
         fTotalSituation(msrequest.msseq, llgAllOK).
         IF llgAllOK EQ TRUE THEN DO:
            CREATE ttNotifyList.
            ttNotifyList.msseq = msrequest.msseq.
         END.
      END.
   END.
END.


/*Function sends notifications for operational data updates that have been
  done during collection period.
  Program uses temp table for avoiding multiple messages to a single entry*/
FUNCTION fNotifyWEB RETURNS CHAR
   (iiMsSeq AS INT):
   DEF VAR liOrderID AS INT NO-UNDO.
   DEF VAR lcStatusCode AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.

   FOR EACH ttNotifyList:
      ASSIGN 
         liOrderID = 0
         lcMSISDN = ""
         lcStatusCode = "".
      FIND FIRST Order NO-LOCK WHERE
                 Order.MsSeq EQ iiMsSeq AND
                 Order.Orderchannel BEGINS "migration" NO-ERROR.
      IF AVAIL Order THEN liOrderID = Order.Orderid.

      lcStatusCode = fTotalSituation(iiMsSeq, llgOK).
      IF llgOK EQ TRUE THEN lcStatusCode = "OK".

      lcMQMessage = fGenerateOrderInfo(liOrderID,
                                       lcMSISDN,
                                       lcStatusCode).

      IF lcMQMessage EQ "" THEN
         lcStatusCode = lcStatusCode + ";Message creation failed".
      ELSE DO:
         lcStatusCode = ";" + fWriteToMQ(lcMQMessage).
      END.
      PUT STREAM sLog UNFORMATTED lcStatusCode SKIP.
      PUT STREAM sLog UNFORMATTED lcMQMessage SKIP.

   END.   

   RETURN "".
END.




ASSIGN
   lcTableName = "MB_Migration"
   lcActionID = "migration_oper_data_status"
   ldCurrentTimeTS = fMakeTS()
   lcLogDir = fCParam("MB_Migration", "MigrationLogDir").

IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".

/*Set output and log files*/
ldaReadDate = TODAY.
lcTimePart = STRING(YEAR(ldaReadDate)) +
             STRING(MONTH(ldaReadDate),"99") +
             STRING(DAY(ldaReadDate),"99") +
             REPLACE(STRING(TIME,"HH:MM:SS"),":","").
lcLogFile = lcLogDir + "MM_MIGRATION_OPER_DATA_READY_" + lcTimePart + ".log".

OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   "Migration file reading starts " + fTS2HMS(fMakeTS()) SKIP.

/*Ensure that multiple instances of the program are not running*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sLog UNFORMATTED
         "File processing alrady ongoing " + fTS2HMS(fMakeTS()) SKIP.
      OUTPUT STREAM sLog CLOSE.
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      /*store previous starting time before setting new value to db*/
      ldCollPeriodStartTS = INT(fMakeTS()). 

      QUIT. /*No reporting in first time.*/
   END.
END.

/*Execution part*/
lcErr = fInitMigrationMQ("oper_status"). 
IF lcErr NE "" THEN DO:
   PUT STREAM sLog UNFORMATTED
      "MQ error. Notification sending not possible " + lcErr +
      fTS2HMS(fMakeTS()) SKIP.

END.
ELSE DO: /*Initialization OK, ready to send data to Migration tool*/
   /*Actual processing*/
   ldCollPeriodStartTS = ActionLog.ActionTS.
   ldCollPeriodEndTS = fSecOffSet(ldCurrentTimeTS, -60). /*now - 1 minute */

   PUT STREAM sLog UNFORMATTED
      "Failed case collection starts " + fTS2HMS(fMakeTS()) SKIP.
   fFailedOperDataSettings(ldCollPeriodStartTS, ldCollPeriodEndTS).
   
   PUT STREAM sLog UNFORMATTED
      "Completed case collection starts" + fTS2HMS(fMakeTS()) SKIP.
   fOKOperDataSettings(ldCollPeriodStartTS, ldCollPeriodEndTS).

   PUT STREAM sLog UNFORMATTED
      "Reporting part starts" + fTS2HMS(fMakeTS()) SKIP.
   FOR EACH ttNotifyList:
      fNotifyWEB(ttNotifyList.msseq).
   END.

END.

/*Release ActionLog lock*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
      /*Collection time start for next run*/
      /*Stamp is moved if sending operation has been successful*/
      IF lcErr EQ "" THEN ActionLog.ActionTS = ldCollPeriodEndTS.

   END.
   RELEASE ActionLog.

END.

PUT STREAM sLog UNFORMATTED
   "Operational data check ends" + fTS2HMS(fMakeTS()) SKIP.
OUTPUT STREAM sLog CLOSE.

