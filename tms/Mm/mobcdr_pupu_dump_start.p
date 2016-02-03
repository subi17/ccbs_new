/* ----------------------------------------------------------------------
  MODULE .......: mobcdr_pupu_dump_start.p
  TASK .........: Create a dump file for cdrs for High performance data
  APPLICATION ..: tms
  AUTHOR .......: Vikas 
  CREATED ......: 10.06.13
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcBrand = "1".
Katun = "Cron".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/direct_dbconnect.i}

DEF VAR lcDel         AS CHAR NO-UNDO INIT "|".
DEF VAR ldaReadDate   AS DATE NO-UNDO.
DEF VAR ldeReadInTS   AS DEC  NO-UNDO.
DEF VAR ldeCDRStamp   AS DEC  NO-UNDO.
DEF VAR ldeCurrStamp  AS DEC  NO-UNDO.
DEF VAR liEvents      AS INT  NO-UNDO.
DEF VAR liReadTime    AS INT  NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO.
DEF VAR lcSpoolDir    AS CHAR NO-UNDO.
DEF VAR lcOutDir      AS CHAR NO-UNDO.
DEF VAR lcKeyValue    AS CHAR NO-UNDO.
DEF VAR lcDel2        AS CHAR NO-UNDO.
DEF VAR ldaConnectDate AS DATE NO-UNDO.

ASSIGN ldaReadDate  = TODAY
       ldeReadInTS  = fMake2Dt(ldaReadDate,0)
       ldeCurrStamp = fMakeTS()
       lcSpoolDir   = fCParam("HPD","DumpSpoolDir")
       lcOutDir     = fCParam("HPD","DumpOutDir")
       lcLogFile    = lcSpoolDir + "/mobcdr_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") + "_" +
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

FIND FIRST ActionLog WHERE
           ActionLog.Brand     = gcBrand        AND
           ActionLog.ActionID  = "MobCDR_HPD"   AND
           ActionLog.TableName = "MobCDR" NO-LOCK NO-ERROR.
IF AVAIL ActionLog THEN DO:
   ldeReadInTS = ActionLog.ActionTS.
   fSplitTS(ldeReadInTS,ldaReadDate,liReadTime).
END.
ELSE DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand
      ActionLog.TableName    = "MobCDR"
      ActionLog.KeyValue     = "HPD"
      ActionLog.ActionID     = "MobCDR_HPD"
      ActionLog.ActionPeriod = YEAR(ldaReadDate) * 100 + MONTH(ldaReadDate)
      ActionLog.ActionStatus = 2
      ActionLog.UserCode     = katun
      ActionLog.ActionTS     = ldeReadInTS.

END. /* ELSE DO: */

RELEASE ActionLog.

ldaConnectDate = ldaReadDate.

RUN pDBConnect(ldaConnectDate).
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN.

RUN pStartDump(FALSE).

/* do two runs if db has been renewed on the event day -> some tickets have
   been saved to old db on the 1. day */
ldaConnectDate = ?.
FOR FIRST ttDB WHERE
          ttDb.ConnName = "" AND
          ttDb.TableName = "MobCDR",
    FIRST DBConfig NO-LOCK WHERE
          DBConfig.DBConfigId = ttDb.DbConfigId:
  IF DBConfig.FromDate = ldaReadDate THEN
     ldaConnectDate = DbConfig.FromDate - 1.
END.

IF ldaConnectDate NE ? THEN DO:
   RUN pDBConnect(ldaConnectDate).
   IF NOT RETURN-VALUE BEGINS "ERROR" THEN
      RUN pStartDump(TRUE).
END.

IF ldeCDRStamp = 0 OR ldeCDRStamp = ? THEN
   ldeCDRStamp = ldeCurrStamp.

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     = gcBrand        AND
              ActionLog.ActionID  = "MobCDR_HPD"   AND
              ActionLog.TableName = "MobCDR" EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN
      ActionLog.ActionTS = ldeCDRStamp.
END.

RELEASE ActionLog.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir). 

PROCEDURE pDBConnect:
 
   DEF INPUT PARAMETER idaConnectDate AS DATE NO-UNDO.
   
   /* connect to correct cdr dbs */
   fInitializeConnectTables("MobCDR,McdrDtl2","").

   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          idaConnectDate,
                          idaConnectDate).

   IF RETURN-VALUE BEGINS "ERROR" THEN 
      RETURN RETURN-VALUE.

END PROCEDURE.

PROCEDURE pStartDump:

   DEF INPUT PARAMETER ilAppend  AS LOG  NO-UNDO.

   RUN mobcdr_pupu_dump.p(ldaReadDate,ldeReadInTS,fTimeStamp2DateTime(ldeCurrStamp),
                          lcLogFile,ilAppend,INPUT-OUTPUT ldeCDRStamp).

END PROCEDURE.

