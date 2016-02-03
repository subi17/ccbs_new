/* ----------------------------------------------------------------------
  MODULE .......: mobcdr.p
  TASK .........: Create a full dump file for current month cdrs
                  for High performance data
  APPLICATION ..: tms
  AUTHOR .......: Vikas 
  CREATED ......: 10.06.13
  Version ......: yoigo
---------------------------------------------------------------------- */

DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.
DEF INPUT PARAMETER iiSeq       AS INT  NO-UNDO.

DEF VAR lcDel         AS CHAR NO-UNDO INIT "|".
DEF VAR ldaFromDate   AS DATE NO-UNDO.
DEF VAR ldaToDate     AS DATE NO-UNDO.
DEF VAR liEvents      AS INT  NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO.
DEF VAR lcSpoolDir    AS CHAR NO-UNDO.
DEF VAR lcOutDir      AS CHAR NO-UNDO.
DEF VAR lcKeyValue    AS CHAR NO-UNDO.
DEF VAR lcDel2        AS CHAR NO-UNDO.
DEF VAR gcBrand       AS CHAR NO-UNDO INIT "1".
DEF VAR ldtCurrStamp  AS DATETIME NO-UNDO.

{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/date.i}
{Func/cparam2.i}
{Func/ftransdir.i}

DEF STREAM sFile.

ASSIGN ldaFromDate  = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldaToDate    = fLastDayOfMonth(TODAY)
       ldtCurrStamp = DATETIME(TODAY,MTIME)
       lcDel2       = CHR(255)
       lcSpoolDir   = fCParam("HPD","DumpSpoolDir")
       lcOutDir     = fCParam("HPD","DumpOutDir")
       lcLogFile    = lcSpoolDir + "/mobcdr_full_" + STRING(iiSeq) + "_" +
                      STRING(YEAR(TODAY)) +
                      STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + "_" +
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM sFile TO VALUE(lcLogFile).

FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE
         MobCDR.ReadDate >= idaFromDate AND
         MobCDR.ReadDate <= idaToDate   AND
         MobCDR.ErrorCode = 0  
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN LEAVE.

   RUN pWrite2File.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

PROCEDURE pWrite2File:

   ASSIGN lcKeyValue = MobCDR.CLI + lcDel2 + STRING(MobCDR.DtlSeq) +
                       lcDel2 + STRING(MobCDR.DateSt)
          liEvents   = liEvents + 1.
  
   PUT STREAM sFile UNFORMATTED
      "MobCDR"                                              lcDel
      "CREATE"                                              lcDel
      fNotNull(STRING(RECID(MobCDR)))                       lcDel
      fNotNull(lcKeyValue)                                  lcDel
      fNotNull(STRING(ldtCurrStamp) )                       lcDel
      fNotNull(STRING(MobCDR.MsSeq))                        lcDel
      fNotNull(STRING(MobCDR.CLI))                          lcDel
      fNotNull(STRING(MobCDR.CLIType))                      lcDel
      fNotNull(STRING(MobCDR.InvCust))                      lcDel
      fNotNull(STRING(MobCdr.DateSt,"99.99.9999"))          lcDel
      fNotNull(STRING(MobCdr.TimeSt,"HH:MM:SS"))            lcDel
      fNotNull(STRING(MobCDR.ReadInTS))                     lcDel
      fNotNull(STRING(MobCDR.EventType))                    lcDel
      fNotNull(STRING(MobCDR.GsmBnr))                       lcDel
      fNotNull(STRING(MobCDR.BillCode))                     lcDel
      fNotNull(STRING(MobCDR.CCN))                          lcDel
      fNotNull(STRING(MobCDR.BillDur))                      lcDel
      fNotNull(STRING(MobCDR.DataIn + MobCDR.DataOut))      lcDel
      fNotNull(TRIM(STRING(MobCDR.Amount,"->>>>>>>>>>>9.9<<<<<"))) lcDel
      fNotNull(STRING(MobCDR.DCEvent))                      lcDel
      fNotNull(STRING(MobCDR.BDest))                        SKIP.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "MobCDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END PROCEDURE.               


