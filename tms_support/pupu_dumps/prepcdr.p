/* ----------------------------------------------------------------------
  MODULE .......: prepcdr.p
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
DEF VAR lcDCEvent     AS CHAR NO-UNDO.

{tmsconst.i}
{timestamp.i}
{date.i}
{cparam2.i}
{ftransdir.i}

DEF STREAM sFile.

ASSIGN ldaFromDate  = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldaToDate    = fLastDayOfMonth(TODAY)
       ldtCurrStamp = DATETIME(TODAY,MTIME)
       lcDel2       = CHR(255)
       lcSpoolDir   = fCParam("HPD","DumpSpoolDir")
       lcOutDir     = fCParam("HPD","DumpOutDir")
       lcLogFile    = lcSpoolDir + "/prepcdr_full_" + STRING(iiSeq) + "_" +
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

FOR EACH PrepCDR NO-LOCK USE-INDEX ReadDate WHERE
         PrepCDR.ReadDate >= idaFromDate AND
         PrepCDR.ReadDate <= idaToDate   AND
         PrepCDR.ErrorCode = 0  
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

   ASSIGN lcKeyValue = PrepCDR.CLI + lcDel2 + STRING(PrepCDR.DtlSeq) +
                       lcDel2 + STRING(PrepCDR.DateSt)
          liEvents   = liEvents + 1.

   IF PrepCDR.DCEvent > "" THEN
      lcDCEvent = PrepCDR.DCEvent.
   ELSE IF PrepCDR.BillCode = "PREMDUB" THEN
      lcDCEvent = "PMDUB".
   ELSE IF PrepCDR.CLIType = "TARJ7" AND PrepCDR.Charge = 0 THEN
      lcDCEvent = "TARJ7".
   ELSE lcDCEvent = "".
  
   PUT STREAM sFile UNFORMATTED
      "PrepCDR"                                              lcDel
      "CREATE"                                               lcDel
      fNotNull(STRING(RECID(PrepCDR)))                       lcDel
      fNotNull(STRING(lcKeyValue))                           lcDel
      fNotNull(STRING(ldtCurrStamp))                         lcDel
      fNotNull(STRING(PrepCDR.MsSeq))                        lcDel
      fNotNull(STRING(PrepCDR.CLI))                          lcDel
      fNotNull(STRING(PrepCDR.CLIType))                      lcDel
      fNotNull(STRING(PrepCDR.InvCust))                      lcDel
      fNotNull(STRING(PrepCDR.DateSt,"99.99.9999"))          lcDel
      fNotNull(STRING(PrepCDR.TimeSt,"HH:MM:SS"))            lcDel
      fNotNull(STRING(PrepCDR.ReadInTS))                     lcDel
      fNotNull(STRING(PrepCDR.EventType))                    lcDel
      fNotNull(STRING(PrepCDR.GsmBnr))                       lcDel
      fNotNull(STRING(PrepCDR.BillCode))                     lcDel
      fNotNull(STRING(PrepCDR.CCN))                          lcDel
      fNotNull(STRING(PrepCDR.BillDur))                      lcDel
      fNotNull(STRING(PrepCDR.DataIn + PrepCDR.DataOut))     lcDel
      fNotNull(TRIM(STRING(PrepCDR.Charge,"->>>>>>>>>>>9.9<<<<<"))) lcDel
      fNotNull(STRING(lcDCEvent))                            lcDel
      fNotNull(STRING(PrepCDR.BDest))                        SKIP.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "PrepCDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END PROCEDURE.               


