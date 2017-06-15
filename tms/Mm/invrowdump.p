/* ----------------------------------------------------------------------
  MODULE .......: invrowdump.p
  TASK .........: Dump Invoice Rows
  APPLICATION ..: tms
  AUTHOR .......: vikas 
  CREATED ......: 18.03.11
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/coinv.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric                  AS CHAR NO-UNDO.
DEF VAR lcDelimiter                AS CHAR NO-UNDO.
DEF VAR lcInvType                  AS CHAR NO-UNDO.
DEF VAR liPeriod                   AS INT  NO-UNDO.
DEF VAR ldaStart                   AS DATE NO-UNDO.
DEF VAR ldaEND                     AS DATE NO-UNDO.
DEF VAR lcCLIType                  AS CHAR NO-UNDO.
DEF VAR lcBundle                   AS CHAR NO-UNDO.
DEF VAR liBIGroupType              AS INT  NO-UNDO.

DEF STREAM sFile.

DEF TEMP-TABLE ttMsOwner NO-UNDO
    FIELD MsSeq         AS INT
    FIELD Custnum       AS INT
    FIELD CLIType       AS CHAR
    FIELD TariffBundle  AS CHAR
    FIELD PeriodDate    AS DATE
    INDEX MsSeqType  MsSeq Custnum CLIType TariffBundle
    INDEX MsSeqDate  MsSeq Custnum PeriodDate
    INDEX PeriodDate MsSeq Custnum PeriodDate DESC.

ASSIGN lcNumeric      = SESSION:NUMERIC-FORMAT
       liPeriod       = YEAR(Today) * 100 + MONTH(Today)
       ldaStart       = fInt2Date(liPeriod,1)
       ldaEnd         = fInt2Date(liPeriod,2)
       icFile         = REPLACE(icFile, ".gz", "").

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDelimiter = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END. /* IF AVAILABLE DumpFile THEN DO: */
ELSE DO:
   ASSIGN 
      lcDelimiter = ";"
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END. /* ELSE DO: */

FUNCTION fGetCLIType RETURNS LOGICAL (INPUT piMsSeq  AS INT):

    DEF VAR ldaDate AS DATE NO-UNDO.
    DEF VAR liTime  AS INT  NO-UNDO.
    DEF VAR ldFromPer AS DECIMAL NO-UNDO.
    DEF VAR ldToPer AS DECIMAL NO-UNDO.
    DEF VAR ldInvoiceFrom AS DECIMAL NO-UNDO.
    DEF VAR liFoundOwner  AS INT NO-UNDO.

    ASSIGN
       ldFromPer   = fMake2Dt(IF Invoice.FirstCall NE ?
                              THEN Invoice.FirstCall
                              ELSE Invoice.FromDate,0)
       ldToPer     = fMake2DT(Invoice.ToDate,86399)
       ldInvoiceFrom = fMake2DT(DATE(MONTH(Invoice.Todate),
                                     1,
                                     YEAR(Invoice.ToDate)),0)
       liFoundOwner = 0.

    DEF BUFFER bMsOwner   FOR MsOwner.

    FIND FIRST bMsOwner WHERE
               bMsOwner.MsSeq    = piMsSeq         AND
               bMsOwner.TsBeg   <= ldToPer  AND
               bMsOwner.TsBeg   >= ldInvoiceFrom AND
               bMsOwner.PayType  = FALSE           AND
               bMsOwner.InvCust  = Invoice.Custnum AND
               bMsOwner.CLIEvent BEGINS "iS" NO-LOCK NO-ERROR.
    IF AVAIL bMsOwner THEN DO:

       fSplitTS(bMsOwner.TSBeg,OUTPUT ldaDate,OUTPUT liTime).

       CREATE ttMsOwner.
       ASSIGN ttMsOwner.MsSeq        = bMsOwner.MsSeq
              ttMsOwner.Custnum      = bMsOwner.InvCust
              ttMsOwner.CLIType      = bMsOwner.CLIType
              ttMsOwner.TariffBundle = bMsOwner.TariffBundle
              ttMsOwner.PeriodDate   = Invoice.Todate.
    END.

    FOR EACH MsOwner WHERE
             MsOwner.MsSeq    = piMsSeq         AND
             MsOwner.TsBeg   <= ldToPer  AND
             MsOwner.TsEnd   >= ldFromPer AND
             MsOwner.InvCust  = Invoice.Custnum AND
             MsOwner.PayType  = FALSE NO-LOCK USE-INDEX MsSeq:

       IF AVAIL bMsOwner AND
                bMsOwner.TsEnd <= MsOwner.TsEnd THEN NEXT.

       IF NOT CAN-FIND(FIRST ttMsOwner NO-LOCK WHERE
                             ttMsOwner.MsSeq   = MsOwner.MsSeq   AND
                             ttMsOwner.CLIType = MsOwner.CLIType AND
                             ttMsOwner.TariffBundle = MsOwner.TariffBundle)
       THEN DO:

          liFoundOwner = liFoundOwner + 1.

          CREATE ttMsOwner.
          ASSIGN ttMsOwner.MsSeq        = MsOwner.MsSeq
                 ttMsOwner.Custnum      = MsOwner.InvCust
                 ttMsOwner.CLIType      = MsOwner.CLIType
                 ttMsOwner.TariffBundle = MsOwner.TariffBundle.

          IF ldaDate <> ? THEN
             ttMsOwner.PeriodDate = (ldaDate - 1).
          ELSE
             ttMsOwner.PeriodDate = Invoice.Todate.

          LEAVE.
       END.
    END.

    IF liFoundOwner = 0 THEN
      FOR FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
                MsOwner.MsSeq   = piMsSeq AND
                MsOwner.InvCust = Invoice.CustNum AND
                MsOwner.PayType = FALSE:

         CREATE ttMsOwner.
         ASSIGN ttMsOwner.MsSeq        = MsOwner.MsSeq
                ttMsOwner.Custnum      = MsOwner.InvCust
                ttMsOwner.CLIType      = MsOwner.CLIType
                ttMsOwner.TariffBundle = MsOwner.TariffBundle.

         ttMsOwner.PeriodDate = Invoice.Todate.
      END.

    RETURN TRUE.

END FUNCTION. /* FUNCTION fGetCLIType RETURNS CHARACTER */

lcInvType = fCParamC("InvoiceType").
IF lcInvType = "" OR lcInvType = ? THEN lcInvType = "1".

OUTPUT STREAM sFile TO VALUE(icFile).

/* Add the header in the output file */
PUT STREAM sFile UNFORMATTED
    "PeriodBegin"          lcDelimiter
    "PeriodEnd"            lcDelimiter
    "InvNum"               lcDelimiter
    "InvDate"              lcDelimiter
    "ExtInvId"             lcDelimiter
    "InvCustNum"           lcDelimiter
    "SubInvNum"            lcDelimiter
    "SubInvCustNum"        lcDelimiter
    "MsSeq"                lcDelimiter
    "CLI"                  lcDelimiter
    "CLIType"              lcDelimiter
    "CCN"                  lcDelimiter
    "BillCode"             lcDelimiter
    "Minutes"              lcDelimiter
    "DataAmt(MB)"          lcDelimiter
    "Quantity"             lcDelimiter
    "AmountEUR"            lcDelimiter
    "TariffBundle"         lcDelimiter
    "FixedNumber"          lcDelimiter
    "BillingItemType"      SKIP.


Invoices:
FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
         Invoice.Brand    = gcBrand   AND
         Invoice.InvDate >= ldaStart  AND
         Invoice.InvDate <= ldaEnd    AND
         LOOKUP(STRING(Invoice.InvType), lcInvType) > 0,
    EACH SubInvoice OF Invoice NO-LOCK
    ON QUIT UNDO, RETRY
    ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE Invoices.
   END. /* IF RETRY THEN DO: */

   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents mod 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "Invoices" FORMAT ">>>>>>>>>9" 
      WITH SIDE-LABELS OVERLAY ROW 10 CENTERED TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND oiInvCount mod 100 = 0 THEN DO: */

   EMPTY TEMP-TABLE ttMsOwner NO-ERROR.

   /* Pick all the subs types */
   fGetCLIType(SubInvoice.MsSeq).

   FOR EACH InvRowCounter WHERE
            InvRowCounter.InvNum  = Invoice.InvNum AND
            InvRowCounter.SubInvNum = SubInvoice.SubInvNum NO-LOCK:

      FIND FIRST ttMsOwner NO-LOCK WHERE
                 ttMsOwner.MsSeq = SubInvoice.MsSeq AND
                 ttMsOwner.Custnum = Invoice.Custnum AND
                 ttMsOwner.PeriodDate >= InvRowCounter.Todate
           USE-INDEX MsSeqDate NO-ERROR.
      IF NOT AVAIL ttMsOwner THEN
         FIND FIRST ttMsOwner NO-LOCK WHERE
                    ttMsOwner.MsSeq = SubInvoice.MsSeq AND
                    ttMsOwner.Custnum = Invoice.Custnum
              USE-INDEX PeriodDate NO-ERROR.

      IF AVAIL ttMsOwner THEN
         ASSIGN lcCLIType = ttMsOwner.CLIType
                lcBundle  = ttMsOwner.TariffBundle.
      ELSE
         ASSIGN lcCLIType = ""
                lcBundle  = "".

      ASSIGN liBIGroupType = -1.
      FOR FIRST BillItem NO-LOCK WHERE
                BillItem.Brand    = Invoice.Brand AND
                BillItem.BillCode = InvRowCounter.BillCode,
         FIRST BitemGroup NO-LOCK WHERE
               BitemGroup.Brand   = BillItem.Brand AND
               BitemGroup.BIGroup = BillItem.BIGroup:
         ASSIGN liBIGroupType = BitemGroup.GroupType.
      END.
      /* Dump Invoice Rows */
      PUT STREAM sFile UNFORMATTED
                 Invoice.FromDate                         lcDelimiter
                 Invoice.Todate                           lcDelimiter
                 Invoice.InvNum                           lcDelimiter
                 Invoice.InvDate                          lcDelimiter
                 Invoice.ExtInvId                         lcDelimiter
                 Invoice.CustNum                          lcDelimiter
                 SubInvoice.SubInvNum                     lcDelimiter
                 SubInvoice.CustNum                       lcDelimiter
                 SubInvoice.MsSeq                         lcDelimiter
                 SubInvoice.CLI                           lcDelimiter
                 lcCLIType                                lcDelimiter
                 InvRowCounter.CCN                        lcDelimiter
                 InvRowCounter.BillCode                   lcDelimiter
                 ROUND((InvRowCounter.Duration / 60),4)   lcDelimiter
                 ROUND(InvRowCounter.DataAmt / (1024 * 1024),4) lcDelimiter
                 InvRowCounter.Quantity                   lcDelimiter
                 ROUND(InvRowCounter.Amount,4)            lcDelimiter
                 lcBundle                                 lcDelimiter
                 SubInvoice.FixedNumber                   lcDelimiter
                 liBIGroupType                            SKIP.

   END. /* FOR EACH InvRowCounter */

   FOR EACH InvRow NO-LOCK WHERE
            InvRow.InvNum = Invoice.InvNum AND 
            InvRow.SubInvNum = SubInvoice.SubInvNum AND
            LOOKUP(STRING(InvRow.RowType),"2,6") = 0:

      FIND FIRST ttMsOwner NO-LOCK WHERE
                 ttMsOwner.MsSeq = SubInvoice.MsSeq AND
                 ttMsOwner.Custnum = Invoice.Custnum AND
                 ttMsOwner.PeriodDate >= InvRow.Todate
           USE-INDEX MsSeqDate NO-ERROR.
      IF NOT AVAIL ttMsOwner THEN
         FIND FIRST ttMsOwner NO-LOCK WHERE
                    ttMsOwner.MsSeq = SubInvoice.MsSeq AND
                 ttMsOwner.Custnum = Invoice.Custnum
              USE-INDEX PeriodDate NO-ERROR.

      IF AVAIL ttMsOwner THEN
         ASSIGN lcCLIType = ttMsOwner.CLIType
                lcBundle  = ttMsOwner.TariffBundle.
      ELSE
         ASSIGN lcCLIType = ""
                lcBundle  = "".

      ASSIGN liBIGroupType = -1.
      FOR FIRST BillItem NO-LOCK WHERE
                BillItem.Brand    = Invoice.Brand AND
                BillItem.BillCode = InvRow.BillCode,
         FIRST BitemGroup NO-LOCK WHERE
               BitemGroup.Brand   = BillItem.Brand AND
               BitemGroup.BIGroup = BillItem.BIGroup:
         ASSIGN liBIGroupType = BitemGroup.GroupType.
      END.

         /* Dump Invoice Rows */ 
         PUT STREAM sFile UNFORMATTED
                    Invoice.FromDate                 lcDelimiter
                    Invoice.ToDate                   lcDelimiter
                    Invoice.InvNum                   lcDelimiter
                    Invoice.InvDate                  lcDelimiter
                    Invoice.ExtInvId                 lcDelimiter
                    Invoice.CustNum                  lcDelimiter
                    SubInvoice.SubInvNum             lcDelimiter
                    SubInvoice.CustNum               lcDelimiter
                    SubInvoice.MsSeq                 lcDelimiter
                    SubInvoice.CLI                   lcDelimiter
                    lcCLIType                        lcDelimiter
                    ""                               lcDelimiter
                    InvRow.BillCode                  lcDelimiter
                    0                                lcDelimiter
                    0                                lcDelimiter
                    InvRow.Qty                       lcDelimiter
                    ROUND(InvRow.Amt,4)              lcDelimiter
                    lcBundle                         lcDelimiter
                    SubInvoice.FixedNumber           lcDelimiter
                    liBIGroupType                    SKIP.

   END. /* FOR EACH InvRow NO-LOCK WHERE */   
END. /* for EACH Invoice NO-LOCK USE-INDEX InvDate WHERE */

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sFile CLOSE.

UNIX SILENT VALUE("gzip " + icFile).
icFile = icFile + ".gz".

SESSION:NUMERIC-FORMAT = lcNumeric.
