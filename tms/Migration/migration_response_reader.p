/* ----------------------------------------------------------------------
  MODULE .......: migration_response_reader.p
  TASK .........: Program reads Migration response files.
                  It also sets corresponding order status according to
                  returned status and notifies WEB MigrationTool about 
                  changed status.
                  Timing plan:
                  The program runs periodically some time after TMS has
                  sent migration request to NC.
                  This program has locking functionality that prevents
                  running multiple instances.
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 25.1.2017
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Migration/migrationfunc.i}
{Func/ftransdir.i}
{Func/orderfunc.i}
Syst.CUICommon:gcBrand = "1".

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcProcDir AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcInDir AS CHAR NO-UNDO.
DEF VAR ldaReadDate AS DATETIME.
DEF VAR lcTimePart AS CHAR. /*For log file name*/
DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcRowStatus AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR No-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcMQMessage AS CHAR NO-UNDO.

DEF VAR lcErr AS CHAR NO-UNDO.

ASSIGN
   lcTableName = "MB_Migration"
   lcActionID = "migration_response_reader"
   ldCurrentTimeTS = Func.Common:mMakeTS()
   lcInDir = fCParam("MB_Migration", "MigrationInDir").

IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".
IF lcInDir EQ "" OR lcInDir EQ ? THEN DO:
   ASSIGN
      lcInDir = "/tmp/"
      lcLogDir = "/tmp/"
      lcProcDir = "/tmp/".
END.
ELSE DO:
   ASSIGN
      lcProcDir = lcInDir + "/processed/"
      lcLogDir = lcInDir + "/logs/"
      lcInDir = lcInDir + "/incoming/".
END.   


/*Set output and log files*/
ldaReadDate = TODAY.
lcTimePart = STRING(YEAR(ldaReadDate)) +
             STRING(MONTH(ldaReadDate),"99") +
             STRING(DAY(ldaReadDate),"99") +
             REPLACE(STRING(TIME,"HH:MM:SS"),":","").
lcLogFile = lcLogDir + "MM_MIGRATION_RESPONSE_" + lcTimePart + ".log".

OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   "Migration file reading starts " + Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.

/*Ensure that multiple instances of the program are not running*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.CUICommon:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sLog UNFORMATTED
         "File processing alrady ongoing " + Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.
      OUTPUT STREAM sLog CLOSE.
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.CUICommon:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      QUIT. /*No reporting in first time.*/
   END.
END.

/*Execution part*/
lcErr = fInitMigrationMQ("response").
IF lcErr NE "" THEN DO:
   PUT STREAM sLog UNFORMATTED
      "MQ error. Migration file will be skipped: " + lcErr +
      Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.

END.
ELSE DO:
   /*MQ ready, it is possible to handle data*/
   INPUT STREAM sFile THROUGH VALUE("ls -1tr MIGRATION_RESPONSE*" + lcInDir ).
   REPEAT:
      IMPORT STREAM sFile UNFORMATTED lcFileName.
      lcInputFile = lcInDir + lcFileName.
      IF SEARCH(lcInputFile) NE ? THEN INPUT STREAM sIn FROM VALUE(lcInputFile).
      ELSE NEXT.
    
      RUN pReadFile.
      fMove2TransDir(lcInputFile,"",lcProcDir).
   END.
END.   
/*Release ActionLog lock*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.CUICommon:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.


PUT STREAM sLog UNFORMATTED
   "Migration file handling done " + Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.
OUTPUT STREAM sLog CLOSE.


/*File handling logic*/
/*Program separates succesful and unsuccesful rows to different lists.
  Data is provided to Migration Tool in a single function call accoring to
  temp tables that this procedure has collected*/
PROCEDURE pReadFile:
   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcNCStatus AS CHAR NO-UNDO.
   DEF VAR lcNCComment AS CHAR NO-UNDO.
   DEF VAR lcFileName AS CHAR NO-UNDO.
   DEF VAR liLineNumber AS INT NO-UNDO.
   DEF VAR liOrderID AS INT NO-UNDO.

   PUT STREAM sLog UNFORMATTED
      "List collection starts " + Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.
   
   FILE_LINE:
   REPEAT TRANS:

      IMPORT STREAM sIn UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.
      liLineNumber = liLineNumber + 1.

      IF NOT SESSION:BATCH AND liLineNumber MOD 10 = 0 THEN DO:
         disp "Reading data: " lcFilename liLineNumber with frame a.
         pause 0.
      END.
      IF NUM-ENTRIES (lcLine,";") NE 3 THEN DO:
         PUT STREAM sLog UNFORMATTED
         lcLine + ";" + "ERROR:Incorrect input format"  SKIP.
         NEXT.

      END.
      assign
         lcMSISDN = STRING(ENTRY(1,lcline,";"))
         lcNCStatus = STRING(ENTRY(2,lcline,";"))
         lcNCComment = STRING(ENTRY(3,lcline,";")) 
         liOrderID = 0.

      FIND FIRST Order NO-LOCK /*EL?)*/ WHERE
                 Order.brand EQ Syst.CUICommon:gcBrand AND
                 Order.CLI EQ lcMSISDN AND
                 Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING} 
                 NO-ERROR.
      IF AVAIL Order THEN liOrderID = Order.OrderID.

      /*Nodo data contains a number for Order that is
        already handled
        in incorrect status
        does not exist at all
        ->TMS writes own error notification list for this*/
      IF liOrderID EQ 0 THEN DO:
         lcRowStatus = "TMS Error, order not found".
      END.
      /*NC response is OK and TMS has order for the operation*/
      ELSE IF lcNCStatus EQ "0000 00000" THEN DO:
         /*Succesful case handling*/
         /*Set Order status*/
         /*Add entry to Migration Tool response list (ok)*/
         /*Send SMS*/
         /*It is important to check that we do not create double requests*/
         lcRowStatus = fCreateMigrationSub(liOrderID).
         
         IF lcRowStatus EQ "" THEN lcRowStatus = "OK".
         ELSE DO:
            lcNcStatus = "CreSub failed in TMS".
         END.

      END.
      ELSE DO:
         /*Error case handling*/
         /*Set Order Status*/
         lcRowStatus = "NC Failure".
         IF liOrderID NE 0 THEN fSetOrderStatus(liOrderID,"7").

      END.
      /*Send message to WEB*/
      lcMQMessage = fGenerateNCResponseInfo(liOrderID,
                                            lcMSISDN,
                                            lcNCStatus,
                                            lcNCComment).
      IF lcMQMessage EQ "" THEN 
         lcRowStatus = lcRowStatus + ";Message creation failed". 
      ELSE DO:
        lcRowStatus = ";" + fWriteToMQ(lcMQMessage).
      END.

      PUT STREAM sLog UNFORMATTED
         lcLine + ";" + lcRowStatus SKIP.
      /*at least in testing phase we write message to log */
      PUT STREAM sLog UNFORMATTED lcMQMessage SKIP.
                  
   END. /* Line handling loop END */

   PUT STREAM sLog UNFORMATTED
      "Read " + STRING(liLineNumber) + " lines. " 
      "List collection done " + Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.
END.

