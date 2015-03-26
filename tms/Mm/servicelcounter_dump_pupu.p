/* -----------------------------------------------------------------
   MODULE .......: servicelcounter_dump_pupu.p
   TASK .........: Dump ServiceLCounter records
   APPLICATION ..: TMS
   AUTHOR .......: Vikas
   CREATED ......: 18.06.13
   Version ......: YOIGO
   -------------------------------------------------------------- */

{commali.i}
{date.i}
{timestamp.i}
{coinv.i}
{dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcDelim                    AS CHAR NO-UNDO.
DEF VAR lcDelim2                   AS CHAR NO-UNDO.
DEF VAR lcNumeric                  AS CHAR NO-UNDO.
DEF VAR liPeriod                   AS INT  NO-UNDO.
DEF VAR ldFromDate                 AS DATE NO-UNDO.
DEF VAR ldeFromStamp               AS DEC  NO-UNDO.
DEF VAR ldtCurrStamp               AS DATETIME NO-UNDO.
DEF VAR lcBundles                  AS CHAR NO-UNDO.
DEF VAR lcBundle                   AS CHAR NO-UNDO.
DEF VAR lcKeyValue                 AS CHAR NO-UNDO.
DEF VAR liCount                    AS INT  NO-UNDO.
DEF VAR liNumEntries               AS INT  NO-UNDO.

DEF STREAM sCounter.

ASSIGN lcNumeric       = SESSION:NUMERIC-FORMAT
       icFile          = REPLACE(icFile,".gz","")
       ldtCurrStamp    = DATETIME(TODAY,MTIME)
       lcDelim2        = CHR(255).

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   ASSIGN lcDelim   = fInitDelimiter(DumpFile.DumpDelimiter)
          lcBundles = DumpFile.ConfigParam.

   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END. /* IF AVAILABLE DumpFile THEN DO: */
ELSE RETURN "ERROR:Dump configuration missing".

OUTPUT STREAM sCounter TO VALUE(icFile).

ASSIGN liPeriod     = YEAR(TODAY) * 100 + MONTH(TODAY)
       ldFromDate   = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldeFromStamp = fMake2Dt(ldFromDate,0)
       liNumEntries = NUM-ENTRIES(lcBundles).

/* Dump ServiceLCounter for mentioned bundles */
DO liCount = 1 TO liNumEntries:
   lcBundle = ENTRY(liCount,lcBundles).

   FOR FIRST ServiceLimit WHERE
             ServiceLimit.GroupCode = lcBundle AND
             ServiceLimit.DialType  = 7 NO-LOCK,
        EACH MServiceLimit WHERE
             MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.EndTS   >= ldeFromStamp NO-LOCK:

      IF MServiceLimit.CustNum > 0 THEN
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.CustNum = MServiceLimit.CustNum AND
                    ServiceLCounter.Period  = liPeriod AND
                    ServiceLCounter.SlSeq   = MServiceLimit.SlSeq
              NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.MsSeq  = MServiceLimit.MsSeq AND
                    ServiceLCounter.Period = liPeriod AND
                    ServiceLCounter.SlSeq  = MServiceLimit.SlSeq
              NO-LOCK NO-ERROR.

      IF NOT AVAIL ServiceLCounter THEN NEXT.

      oiEvents = oiEvents + 1.

      IF NOT SESSION:BATCH AND oiEvents mod 100 = 0 THEN DO:
         DISP oiEvents LABEL "Consumption Counter" FORMAT ">>>>>>>>>9" 
         WITH SIDE-LABELS OVERLAY ROW 10 CENTERED
         TITLE " Collecting " FRAME fQty.
         PAUSE 0.
      END. /* IF NOT SESSION:BATCH AND oiInvCount mod 100 = 0 THEN DO: */

      RUN local-Dump-Counter.

   END. /* FOR FIRST ServiceLimit WHERE */
END. /* DO liCount = 1 TO liNumEntries: */

OUTPUT STREAM sCounter CLOSE.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

UNIX SILENT VALUE("gzip " + icFile).
icFile = icFile + ".gz".

SESSION:NUMERIC-FORMAT = lcNumeric.

/* ------------------------------- END MAIN ------------------------------- */

PROCEDURE local-Dump-Counter:

   IF ServiceLCounter.CustNum > 0 THEN
      lcKeyValue = STRING(ServiceLCounter.CustNum) + lcDelim2 +
                   STRING(ServiceLCounter.SlSeq)   + lcDelim2 +
                   STRING(ServiceLCounter.Period).
   ELSE
      lcKeyValue = STRING(ServiceLCounter.MsSeq)   + lcDelim2 +
                   STRING(ServiceLCounter.SlSeq)   + lcDelim2 +
                   STRING(ServiceLCounter.Period).

   PUT STREAM sCounter UNFORMATTED
       "ServiceLCounter"               lcDelim
       "MODIFY"                        lcDelim
       STRING(RECID(ServiceLCounter))  lcDelim
       lcKeyValue                      lcDelim
       STRING(ldtCurrStamp)            lcDelim
       ServiceLCounter.MSID            lcDelim
       ServiceLCounter.MsSeq           lcDelim
       ServiceLCounter.CustNum         lcDelim
       ServiceLCounter.SlSeq           lcDelim
       ServiceLCounter.Period          lcDelim
       ServiceLCounter.Amt             SKIP.

END PROCEDURE.

