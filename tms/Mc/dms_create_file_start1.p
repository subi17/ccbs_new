/* ----------------------------------------------------------------------
  MODULE .......: dms_create_file_start1.p
  TASK .........: Create DMS file with short time period.
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
DEF VAR ldPreviousEndTS   AS DEC  NO-UNDO.
DEF VAR lcActionID        AS CHAR NO-UNDO.
DEF VAR lcTableName       AS CHAR NO-UNDO.
DEF VAR lcCaseFile        AS CHAR NO-UNDO.
DEF VAR lcSpoolDir        AS CHAR NO-UNDO.
DEF VAR lcOutDir          AS CHAR NO-UNDO.
DEF VAR ldaReadDate       AS DATE NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile         AS CHAR NO-UNDO.


lcTableName = "DMS".
lcActionID = "DMS_HIGH_FREQ".
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

       lcLogFile    = lcLogDir + "tms_to_dms_1_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") + ".log".

FIND FIRST ActionLog WHERE
           ActionLog.Brand     EQ  gcBrand        AND
           ActionLog.ActionID  EQ  lcActionID     AND
           ActionLog.TableName EQ  lcTableName NO-LOCK NO-ERROR.
IF NOT AVAIL ActionLog THEN DO TRANS:
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

/*Execute read operation and assign new period end time to actionlog.*/
ldPreviousEndTS = ActionLog.ActionTS.
ldCollPeriodStartTS = ldPreviousEndTS.
ldCollPeriodEndTS = fSecOffSet(ldCurrentTimeTS, -60).
/*make actual file creation operations*/
/*DMS_CASE_TYPE_ID_ORDER_RESTUDY "2"*/
/*DMS_CASE_TYPE_ID_COMPANY "3"*/
RUN dms_create_docfile.p(SUBST("&1,&2", 
                       {&DMS_CASE_TYPE_ID_ORDER_RESTUDY},
                       {&DMS_CASE_TYPE_ID_COMPANY}),
                       ldCollPeriodStartTS, 
                       ldCollPeriodEndTS, lcCaseFile, lcLogFile).
/* Move the file to Transfer directory */
fMove2TransDir(lcCaseFile, ".txt", lcOutDir).

/*Update cunrent collection period end time to actionlog*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN ActionLog.ActionTS = ldCollPeriodEndTS.
   RELEASE ActionLog.
END.





