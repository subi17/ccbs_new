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
      ldCollPeriodStartTS = 20170317. /*fixed TS for the 1st run*/

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

   /*TODO logic for sending the notifications*/



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

