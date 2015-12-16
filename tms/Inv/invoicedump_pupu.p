/* -----------------------------------------------------------------
   MODULE .......: invoicedump_pupu.p
   TASK .........: dump service invoice information and corresponding
                   invoice rows
   APPLICATION ..: TMS
   AUTHOR .......: Vikas
   CREATED ......: 11.06.13
   Version ......: YOIGO
   --------------------------------------------------- */               

{commali.i}
{date.i}
{timestamp.i}
{coinv.i}
{ftransdir.i}
{dumpfile_run.i}
{istc.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcOutDir                   AS CHAR NO-UNDO.
DEF VAR lcInvRowLogFile            AS CHAR NO-UNDO.
DEF VAR lcSubInvLogFile            AS CHAR NO-UNDO.
DEF VAR lcDelim                    AS CHAR NO-UNDO.
DEF VAR lcDelim2                   AS CHAR NO-UNDO.
DEF VAR lcNumeric                  AS CHAR NO-UNDO.
DEF VAR lcOffsetMonths             AS CHAR NO-UNDO.
DEF VAR liOffsetMonths             AS INT  NO-UNDO.
DEF VAR liFromPeriod               AS INT  NO-UNDO.
DEF VAR ldFromDate                 AS DATE NO-UNDO.
DEF VAR ldeVATAmt                  AS DEC  NO-UNDO.
DEF VAR ldeTotalAmt                AS DEC  NO-UNDO.
DEF VAR ldtCurrStamp               AS DATETIME NO-UNDO.

DEF VAR ldInstallmentFee           AS DEC  NO-UNDO.
DEF VAR lcSubInvKey                AS CHAR NO-UNDO.
DEF VAR lcInvRowKey                AS CHAR NO-UNDO.

DEF STREAM sInv.
DEF STREAM sInvRow.
DEF STREAM sSubInv.

ASSIGN lcNumeric       = SESSION:NUMERIC-FORMAT
       icFile          = REPLACE(icFile,".gz","")
       ldtCurrStamp    = DATETIME(TODAY,MTIME)
       lcInvRowLogFile = REPLACE(icFile,"invoice","invrow")
       lcSubInvLogFile = REPLACE(icFile,"invoice","subinvoice")
       lcDelim2        = CHR(255).

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   ASSIGN lcDelim  = fInitDelimiter(DumpFile.DumpDelimiter)
          lcOffsetMonths = DumpFile.ConfigParam
          lcOutDir = DumpFile.TransDir.

   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END. /* IF AVAILABLE DumpFile THEN DO: */
ELSE RETURN "ERROR:Dump configuration missing".

OUTPUT STREAM sInv TO VALUE(icFile).
OUTPUT STREAM sInvRow TO VALUE(lcInvRowLogFile).
OUTPUT STREAM sSubInv TO VALUE(lcSubInvLogFile).

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

   /* Dump ALL invoices */
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand    = gcBrand    AND
            Invoice.InvDate >= ldFromDate AND
            Invoice.InvType  = 1          AND
            Invoice.DeliveryState > 0
       ON QUIT UNDO, RETRY
       ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END. /* IF RETRY THEN DO: */
      
      FIND FIRST Customer WHERE
                 Customer.CustNum = Invoice.CustNum
      NO-LOCK NO-ERROR.
      IF NOT AVAIL Customer THEN NEXT.

      oiEvents = oiEvents + 1.

      IF NOT SESSION:BATCH AND oiEvents mod 100 = 0 THEN DO:
         DISP oiEvents LABEL "Invoices" FORMAT ">>>>>>>>>9" 
         WITH SIDE-LABELS OVERLAY ROW 10 CENTERED TITLE " Collecting " FRAME fQty.
         PAUSE 0.
      END. /* IF NOT SESSION:BATCH AND oiInvCount mod 100 = 0 THEN DO: */

      RUN local-Dump-Invoice.

      RUN local-Dump-InvRow.
        
   END.
END. /* IF icDumpMode = "Full" THEN DO: */
ELSE DO:
   ldFromDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).

   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand    = gcBrand    AND
            Invoice.InvDate >= ldFromDate AND
            Invoice.InvType  = 1          AND
            Invoice.DeliveryState > 0
       ON QUIT UNDO, RETRY
       ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END. /* IF RETRY THEN DO: */

      IF Invoice.ChgStamp < idLastDump THEN NEXT.

      FIND FIRST Customer WHERE
                 Customer.CustNum = Invoice.CustNum
      NO-LOCK NO-ERROR.
      IF NOT AVAIL Customer THEN NEXT.

      oiEvents = oiEvents + 1.
     
      RUN local-Dump-Invoice.

      RUN local-Dump-InvRow.

   END.
END.

OUTPUT STREAM sInv CLOSE.
OUTPUT STREAM sInvRow CLOSE.
OUTPUT STREAM sSubInv CLOSE.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

UNIX SILENT VALUE("gzip " + icFile).
icFile = icFile + ".gz".

UNIX SILENT VALUE("gzip " + lcInvRowLogFile).
lcInvRowLogFile = lcInvRowLogFile + ".gz".

UNIX SILENT VALUE("gzip " + lcSubInvLogFile).
lcSubInvLogFile = lcSubInvLogFile + ".gz".

/* Move the report to Transfer directory */
fMove2TransDir(lcInvRowLogFile, ".txt", lcOutDir).
fMove2TransDir(lcSubInvLogFile, ".txt", lcOutDir).

SESSION:NUMERIC-FORMAT = lcNumeric.

/* ------------------------------- END MAIN ------------------------------- */
                          
PROCEDURE local-Dump-Invoice:

   PUT STREAM sInv UNFORMATTED
       "Invoice"               lcDelim
       "CREATE"                lcDelim
       STRING(RECID(Invoice))  lcDelim
       STRING(Invoice.InvNum)  lcDelim
       STRING(ldtCurrStamp)    lcDelim
       Invoice.InvNum          lcDelim
       Invoice.ExtInvId        lcDelim
       Invoice.CustNum         lcDelim
       Invoice.FromDate        lcDelim
       Invoice.ToDate          lcDelim
       Invoice.InvDate         lcDelim
       Invoice.DueDate         lcDelim
       Invoice.AmtExclVAT      lcDelim
       Invoice.VATAmt          lcDelim
       Invoice.InvAmt          lcDelim
       Invoice.DelType         SKIP.

END PROCEDURE.


PROCEDURE local-Dump-InvRow:

   DEF VAR ldaISTCDate AS DATE NO-UNDO. 
   DEF VAR ldaMaxInvRowDate AS DATE NO-UNDO. 
   DEF VAR lcCLIType                  AS CHAR NO-UNDO.
   DEF VAR lcTariffBundle             AS CHAR NO-UNDO.

   FOR EACH SubInvoice OF Invoice NO-LOCK:

       ASSIGN lcCLIType        = ""
              lcTariffBundle   = ""
              ldInstallmentFee = 0
              lcSubInvKey      = STRING(Invoice.InvNum) + lcDelim2 +
                                 STRING(SubInvoice.SubInvNum)
              ldaMaxInvRowDate = 1/1/2000.
       
       ldaISTCDate = fGetISTCDate(SubInvoice.MsSeq,
                                  Invoice.Custnum,
                                  Invoice.Todate).

       FOR EACH InvRow NO-LOCK WHERE
                InvRow.InvNum = Invoice.InvNum AND 
                InvRow.SubInvNum = SubInvoice.SubInvNum:

          lcInvRowKey = STRING(Invoice.InvNum)       + lcDelim2 +
                        STRING(SubInvoice.SubInvNum) + lcDelim2 +
                        STRING(InvRow.InvRowNum).

          ldeVATAmt = ROUND(((InvRow.Amt * InvRow.VATPerc) / 100),2).
          IF ldeVATAmt = ? THEN ldeVATAmt = 0.

          ldeTotalAmt = InvRow.Amt + ldeVATAmt.
          
          FIND FIRST BillItem WHERE
                     BillItem.Brand    = gcBrand AND
                     BillItem.BillCode = InvRow.BillCode
                     NO-LOCK NO-ERROR.
          IF AVAIL BillItem AND 
                   BillItem.BIGroup = "33" THEN
            ldInstallmentFee = ldInstallmentFee + InvRow.Amt.

          /* Dump Invoice Rows */
          PUT STREAM sInvRow UNFORMATTED
           "InvRow"                                 lcDelim
           "CREATE"                                 lcDelim
           STRING(RECID(InvRow))                    lcDelim
           lcInvRowKey                              lcDelim
           STRING(ldtCurrStamp)                     lcDelim
           InvRow.InvNum                            lcDelim
           Invoice.ExtInvId                         lcDelim
           Invoice.CustNum                          lcDelim
           InvRow.FromDate                          lcDelim
           InvRow.ToDate                            lcDelim
           Invoice.InvDate                          lcDelim
           SubInvoice.SubInvNum                     lcDelim
           SubInvoice.MsSeq                         lcDelim
           SubInvoice.CLI                           lcDelim
           InvRow.InvRowNum                         lcDelim
           InvRow.BillCode                          lcDelim
           InvRow.Qty                               lcDelim
           ROUND(InvRow.Amt,2)                      lcDelim
           ldeVATAmt                                lcDelim
           ldeTotalAmt                              SKIP.

           IF ldaISTCDate NE ? AND
              ldaMaxInvRowDate < InvRow.ToDate THEN
              ldaMaxInvRowDate = InvRow.Todate.

       END. /* FOR EACH InvRow NO-LOCK WHERE */

       IF ldaISTCDate NE ? AND 
          ldaMaxInvRowDate < ldaISTCDate THEN DO:
       
          FIND FIRST MsOwner USE-INDEX MsSeq WHERE 
                     MsOwner.MsSeq   = SubInvoice.MsSeq AND
                     MsOwner.TsBeg   < fMake2Dt(ldaISTCDate,0)  AND
                     MsOwner.TsEnd  >= fMake2Dt(Invoice.FromDate,0) AND
                     MsOwner.PayType = FALSE AND
                     MsOwner.Custnum = Invoice.Custnum
                     NO-LOCK NO-ERROR.
          IF AVAIL MsOwner THEN
             ASSIGN lcCLIType      = MsOwner.Clitype
                    lcTariffBundle = MsOwner.TariffBundle.
                   
       END.
      
       IF lcCLIType EQ "" THEN DO:

          FIND FIRST MsOwner USE-INDEX MsSeq WHERE 
                     MsOwner.MsSeq   = SubInvoice.MsSeq AND
                     MsOwner.TsBeg  <= fMake2Dt(Invoice.ToDate,0)  AND
                     MsOwner.TsEnd  >= fMake2Dt(Invoice.FromDate,0) AND
                     MsOwner.PayType = FALSE AND
                     MsOwner.Custnum = Invoice.Custnum 
                     NO-LOCK NO-ERROR.
          IF AVAIL MsOwner THEN 
               ASSIGN lcCLIType      = MsOwner.Clitype
                      lcTariffBundle = MsOwner.TariffBundle.
          ELSE
            FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                      MSOwner.MsSeq = SubInvoice.MsSeq AND
                      MSOwner.InvCust = Invoice.CustNum:
               ASSIGN lcCLIType      = MsOwner.Clitype
                      lcTariffBundle = MsOwner.TariffBundle.
           END.

       END.
       
      PUT STREAM sSubInv UNFORMATTED
       "SubInvoice"                  lcDelim
       "CREATE"                      lcDelim
       STRING(RECID(SubInvoice))     lcDelim
       lcSubInvKey                   lcDelim
       STRING(ldtCurrStamp)          lcDelim
       SubInvoice.InvNum             lcDelim
       Invoice.ExtInvId              lcDelim
       SubInvoice.CustNum            lcDelim
       SubInvoice.SubInvNum          lcDelim
       SubInvoice.MsSeq              lcDelim
       SubInvoice.CLI                lcDelim
       SubInvoice.AmtExclVAT         lcDelim
       SubInvoice.VATAmt             lcDelim
       SubInvoice.InvAmt             lcDelim
       lcCLIType                     lcDelim
       lcTariffBundle                lcDelim
       STRING(ldInstallmentFee)      lcDelim
       Invoice.FromDate              lcDelim
       Invoice.ToDate                SKIP.
       
   END.

END PROCEDURE.


