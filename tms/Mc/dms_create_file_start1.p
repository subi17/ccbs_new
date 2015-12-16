/* ----------------------------------------------------------------------
  MODULE .......: dms_create_file_start1.p
  TASK .........: Create DMS casefile.
                  Module reads events from previous run time time to
                  module start time -1 minute.             
  APPLICATION ..: tms
  AUTHOR .......: ilkkasav
  CREATED ......: 31.08.15
  Version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
Katun = "Cron".
{tmsconst.i}
{timestamp.i}
{cparam2.i}
{ftransdir.i}
{dms.i}

DEF VAR ldCollPeriodStartTS   AS DEC  NO-UNDO.
DEF VAR ldCollPeriodEndTS AS DEC  NO-UNDO.
DEF VAR ldCurrentTimeTS   AS DEC  NO-UNDO.
DEF VAR lcActionID        AS CHAR NO-UNDO.
DEF VAR lcTableName       AS CHAR NO-UNDO.
DEF VAR lcCaseFile        AS CHAR NO-UNDO.
DEF VAR lcSpoolDir        AS CHAR NO-UNDO.
DEF VAR lcOutDir          AS CHAR NO-UNDO.
DEF VAR ldaReadDate       AS DATE NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile1        AS CHAR NO-UNDO.


lcTableName = "DMS".
lcActionID = {&DMS_HIGH_FREQ_FILE_CREATOR}.
ldCurrentTimeTS = fMakeTS().

/*Is feature active:*/
IF fDMSOnOff() NE TRUE THEN RETURN.

/*Dir and file definition*/
ASSIGN
       ldaReadDate  = TODAY        
       lcSpoolDir   = fCParam("DMS","TMS_to_DMS_SpoolDir")
       lcLogDir     = fCParam("DMS","TMS_to_DMS_LogDir")
       lcOutDir     = fCParam("DMS","TMS_to_DMS_OutDir")


       lcCaseFile   = lcSpoolDir + "dms_casefile_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") +
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

       lcLogFile1    = lcLogDir + "tms_to_dms_1_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") + ".log".

DO TRANS:

   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
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
      RETURN. /*No reporting in first time.*/
   END.
   ELSE DO:
      /*store previous starting time before setting new value to db*/
      ldCollPeriodStartTS = ActionLog.ActionTS.
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun.
      
      RELEASE Actionlog.
   END.
END.

/*Execute read operation and assign new period end time to actionlog.*/
ldCollPeriodEndTS = fSecOffSet(ldCurrentTimeTS, -60).

RUN dms_create_docfile.p(SUBST("&1,&2,&3,&4,&5,&6",
                          {&DMS_CASE_TYPE_ID_ORDER_ACT},
                          {&DMS_CASE_TYPE_ID_ORDER_RESTUDY},
                          {&DMS_CASE_TYPE_ID_COMPANY},
                          {&DMS_CASE_TYPE_ID_ORDER_VFR},
                          {&DMS_CASE_TYPE_ID_DIRECT_CH},
                          {&DMS_CASE_TYPE_ID_CANCEL}),
                       ldCollPeriodStartTS, 
                       ldCollPeriodEndTS, lcCaseFile, lcLogFile1).
/* Move the file to Transfer directory */
fMove2TransDir(lcCaseFile, ".txt", lcOutDir).

/*Update cunrent collection period end time to actionlog*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionTS = ldCollPeriodEndTS.
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.   
   RELEASE ActionLog.
END.





