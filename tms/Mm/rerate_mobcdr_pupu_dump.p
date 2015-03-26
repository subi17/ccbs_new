/* ----------------------------------------------------------------------
  MODULE .......: rerate_mobcdr_pupu_dump.p
  TASK .........: Create a dump for rerated cdrs for High performance data
  APPLICATION ..: tms
  AUTHOR .......: Vikas 
  CREATED ......: 10.06.13
  Version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
Katun = "Cron".
{tmsconst.i}
{timestamp.i}
{cparam2.i}
{ftransdir.i}

DEF VAR lcDel         AS CHAR NO-UNDO INIT "|".
DEF VAR ldaReadDate   AS DATE NO-UNDO.
DEF VAR liReadTime    AS INT  NO-UNDO.
DEF VAR ldeReadInTS   AS DEC  NO-UNDO.
DEF VAR ldeCurrStamp  AS DEC  NO-UNDO.
DEF VAR ldtCurrStamp  AS DATETIME NO-UNDO.
DEF VAR liEvents      AS INT  NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO.
DEF VAR lcSpoolDir    AS CHAR NO-UNDO.
DEF VAR lcOutDir      AS CHAR NO-UNDO.
DEF VAR lcKeyValue    AS CHAR NO-UNDO.
DEF VAR lcDel2        AS CHAR NO-UNDO.
DEF VAR llFound       AS LOG  NO-UNDO INIT TRUE.
DEF VAR ldeLastEDRStamp   AS DEC  NO-UNDO.

DEF STREAM sFile.

ASSIGN ldaReadDate  = TODAY
       liReadTime   = 0
       ldeReadInTS  = fMake2Dt(ldaReadDate,liReadTime)
       ldeCurrStamp = fMakeTS()
       ldtCurrStamp = fTimeStamp2DateTime(ldeCurrStamp)
       lcDel2       = CHR(255)
       lcSpoolDir   = fCParam("HPD","DumpSpoolDir")
       lcOutDir     = fCParam("HPD","DumpOutDir")
       lcLogFile    = lcSpoolDir + "/rerate_mobcdr_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") + "_" +
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

FIND FIRST ActionLog WHERE
           ActionLog.Brand     = gcBrand             AND
           ActionLog.ActionID  = "Rerate_MobCDR_HPD" AND
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
      ActionLog.ActionID     = "Rerate_MobCDR_HPD"
      ActionLog.ActionPeriod = YEAR(ldaReadDate) * 100 + MONTH(ldaReadDate)
      ActionLog.ActionStatus = 2
      ActionLog.UserCode     = katun
      ActionLog.ActionTS     = ldeReadInTS.

END. /* ELSE DO: */

RELEASE ActionLog.

FOR EACH EDRHistory NO-LOCK USE-INDEX UpdateDate WHERE
         EDRHistory.Brand       = gcBrand      AND
         EDRHistory.UpdateDate >= ldaReadDate
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN LEAVE.
         
   IF EDRHistory.UpdateDate EQ ldaReadDate AND
      EDRHistory.UpdateTime <= liReadTime THEN NEXT.

   IF llFound THEN DO:
      OUTPUT STREAM sFile TO VALUE(lcLogFile).
      ldeLastEDRStamp = fMake2Dt(EDRHistory.UpdateDate,EDRHistory.UpdateTime).
      llFound = FALSE.
   END. /* IF llFound THEN DO: */

   RUN pWrite2File.
END.

IF ldeLastEDRStamp = 0 OR ldeLastEDRStamp = ? THEN
   ldeLastEDRStamp = ldeCurrStamp.

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     = gcBrand             AND
              ActionLog.ActionID  = "Rerate_MobCDR_HPD" AND
              ActionLog.TableName = "MobCDR" EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN
      ActionLog.ActionTS = ldeLastEDRStamp.
END.

RELEASE ActionLog.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

IF NOT llFound THEN DO:
   OUTPUT STREAM sFile CLOSE.

   UNIX SILENT VALUE("gzip " + lcLogFile).
   lcLogFile = lcLogFile + ".gz".

   /* Move the report to Transfer directory */
   fMove2TransDir(lcLogFile, ".txt", lcOutDir).
END. /* IF NOT llFound THEN DO: */

PROCEDURE pWrite2File:

   FIND FIRST MobCDR WHERE
              MobCDR.CLI       = EDRHistory.CLI       AND
              MobCDR.DateSt    = EDRHistory.DateSt    AND
              MobCDR.TimeStart = EDRHistory.TimeStart AND
              MobCDR.DtlSeq    = EDRHistory.DtlSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobCDR THEN RETURN.

   ASSIGN
      liEvents   = liEvents + 1
      lcKeyValue = MobCDR.CLI + lcDel2 + STRING(MobCDR.DtlSeq) + lcDel2 +
                   STRING(MobCDR.DateSt).
  
   PUT STREAM sFile UNFORMATTED
      "MobCDR"                            lcDel
      (IF MobCDR.ErrorCode = 0 THEN "MODIFY" ELSE "DELETE") lcDel
      STRING(RECID(MobCDR))               lcDel
      lcKeyValue                          lcDel
      STRING(ldtCurrStamp)                lcDel
      MobCDR.MsSeq                        lcDel
      MobCDR.CLI                          lcDel
      MobCDR.CLIType                      lcDel
      MobCDR.InvCust                      lcDel
      STRING(MobCdr.DateSt,"99.99.9999")  lcDel
      STRING(MobCdr.TimeSt,"HH:MM:SS")    lcDel
      MobCDR.ReadInTS                     lcDel
      MobCDR.EventType                    lcDel
      MobCDR.GsmBnr                       lcDel
      MobCDR.BillCode                     lcDel
      MobCDR.CCN                          lcDel
      MobCDR.BillDur                      lcDel
      MobCDR.DataIn + MobCDR.DataOut      lcDel
      TRIM(STRING(MobCDR.Amount,"->>>>>>>>>>>9.9<<<<<")) lcDel
      MobCDR.DCEvent                      lcDel
      MobCDR.BDest                        SKIP.
   
   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "MobCDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END PROCEDURE.               


