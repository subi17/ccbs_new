/* -----------------------------------------------------------------
   MODULE .......: payment_dump.p
   TASK .........: Dump Payment records
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
DEF VAR lcNumeric                  AS CHAR NO-UNDO.
DEF VAR lcOffsetMonths             AS CHAR NO-UNDO.
DEF VAR liOffsetMonths             AS INT  NO-UNDO.
DEF VAR liFromPeriod               AS INT  NO-UNDO.
DEF VAR liTime                     AS INT  NO-UNDO.
DEF VAR ldFromDate                 AS DATE NO-UNDO.
DEF VAR ldtCurrStamp               AS DATETIME NO-UNDO.

DEF STREAM sPayment.

ASSIGN lcNumeric       = SESSION:NUMERIC-FORMAT
       icFile          = REPLACE(icFile,".gz","")
       ldtCurrStamp    = DATETIME(TODAY,MTIME).

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   ASSIGN lcDelim  = fInitDelimiter(DumpFile.DumpDelimiter)
          lcOffsetMonths = DumpFile.ConfigParam.

   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END. /* IF AVAILABLE DumpFile THEN DO: */
ELSE RETURN "ERROR:Dump configuration missing".

OUTPUT STREAM sPayment TO VALUE(icFile).

/* FULL DUMP */
IF icDumpMode = "Full" THEN DO:
   IF lcOffsetMonths = "" OR lcOffsetMonths = ? THEN
      liOffsetMonths = 0.
   ELSE
      liOffsetMonths = INT(lcOffsetMonths).

   liFromPeriod = fOffsetMonthsToPeriod(INPUT liOffsetMonths).
   IF liFromPeriod = ? OR liFromPeriod = 0 THEN
      liFromPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).

   ldFromDate = fInt2Date(INT(liFromPeriod),1).

   /* Dump ALL payment */
   FOR EACH Payment NO-LOCK USE-INDEX AccDate WHERE
            Payment.Brand    = gcBrand AND
            Payment.AccDate >= ldFromDate
       ON QUIT UNDO, RETRY
       ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END. /* IF RETRY THEN DO: */

      /* Only Pick Payment Type (1 and 8) and newest than 06/01/2013 */
      IF LOOKUP(STRING(Payment.PaymType),"1,8") = 0 OR
         Payment.InvDate < 06/01/2013 THEN NEXT.
      
      oiEvents = oiEvents + 1.

      IF NOT SESSION:BATCH AND oiEvents mod 100 = 0 THEN DO:
         DISP oiEvents LABEL "Payments" FORMAT ">>>>>>>>>9" 
         WITH SIDE-LABELS OVERLAY ROW 10 CENTERED
         TITLE " Collecting " FRAME fQty.
         PAUSE 0.
      END. /* IF NOT SESSION:BATCH AND oiInvCount mod 100 = 0 THEN DO: */

      RUN local-Dump-Payment.
        
   END.
END. /* IF icDumpMode = "Full" THEN DO: */
ELSE DO:

   IF idLastDump > 0 THEN DO:
      fSplitTS(idLastDump,ldFromDate,liTime).
      ldFromDate = (ldFromDate - 4).
   END.
   ELSE
      ldFromDate = (TODAY - 5).

   FOR EACH Payment NO-LOCK USE-INDEX AccDate WHERE
            Payment.Brand    = gcBrand AND
            Payment.AccDate >= ldFromDate
       ON QUIT UNDO, RETRY
       ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END. /* IF RETRY THEN DO: */

      /* Only Pick Payment Type (1 and 8) and newest than 06/01/2013 */
      IF LOOKUP(STRING(Payment.PaymType),"1,8") = 0 OR
         Payment.InvDate < 06/01/2013 OR
         Payment.ImportStamp < idLastDump THEN NEXT.

      oiEvents = oiEvents + 1.
     
      RUN local-Dump-Payment.

   END.
END.

OUTPUT STREAM sPayment CLOSE.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

UNIX SILENT VALUE("gzip " + icFile).
icFile = icFile + ".gz".

SESSION:NUMERIC-FORMAT = lcNumeric.

/* ------------------------------- END MAIN ------------------------------- */

PROCEDURE local-Dump-Payment:

   PUT STREAM sPayment UNFORMATTED
       "Payment"               lcDelim
       "CREATE"                lcDelim
       STRING(RECID(Payment))  lcDelim
       STRING(Payment.Voucher) lcDelim
       STRING(ldtCurrStamp)    lcDelim
       Payment.Voucher         lcDelim
       Payment.InvNum          lcDelim
       Payment.ExtInvId        lcDelim
       Payment.CustNum         lcDelim
       Payment.PaymAmt         lcDelim
       Payment.PaymType        lcDelim
       Payment.PaymSrc         lcDelim
       Payment.PaymDate        lcDelim
       Payment.ImportStamp     SKIP.

END PROCEDURE.

