{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".

{Func/cparam2.i}
{Func/ftransdir.i}
{Func/timestamp.i}
{Syst/funcrunprocess_update.i}

DEF INPUT  PARAMETER idaStart         AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaEnd           AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType        AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFileName       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiInvCount       AS INT  NO-UNDO.

DEF VAR lcTrans         AS CHAR NO-UNDO.
DEF VAR lcPenBITotals   AS CHAR NO-UNDO. 
DEF VAR lcPenSpool      AS CHAR NO-UNDO.
DEF VAR ldaPeriodStart  AS DATE NO-UNDO.
DEF VAR ldaPeriodEnd    AS DATE NO-UNDO.
DEF VAR lcCLIType       AS CHAR NO-UNDO. 
DEF VAR ldPeriodEndTS   AS DEC  NO-UNDO.
DEF VAR ldPeriodFromTS  AS DEC  NO-UNDO.
DEF VAR lcFinalFile     AS CHAR NO-UNDO.
DEF VAR lcPenFileName   AS CHAR NO-UNDO. 
DEF VAR lcPenInvGrain   AS CHAR NO-UNDO.
DEF VAR lcInvGrainFile  AS CHAR NO-UNDO.
DEF VAR lcBundle        AS CHAR NO-UNDO.
DEF VAR ldaFromDate     AS DATE NO-UNDO. 
DEF VAR ldaToDate       AS DATE NO-UNDO.

DEF STREAM sLogInvGrain.

DEF STREAM sLog.

DEF TEMP-TABLE ttCCN NO-UNDO
   FIELD CC       LIKE Mobcdr.SpoCMT
   FIELD RepCCN   LIKE InvRowCounter.CCN
   FIELD BillCode LIKE BillItem.BillCode
   FIELD CLIType  LIKE Mobcdr.CLIType
   FIELD Seconds  AS   DEC
   FIELD DataAmt  AS   DEC
   FIELD Qty      AS   INT
   FIELD Amount   AS   DEC
   FIELD RowType  AS   INT
   FIELD TariffBundle  AS CHAR
INDEX CCN RepCCN BillCode CLIType TariffBundle
INDEX BillCode BillCode RowType CLIType TariffBundle
INDEX RowType RowType ASC.

DEF TEMP-TABLE ttMsOwner NO-UNDO
    FIELD MsSeq         AS INT
    FIELD Custnum       AS INT
    FIELD CLIType       AS CHAR
    FIELD TariffBundle  AS CHAR
    FIELD PeriodDate    AS DATE
    INDEX MsSeqType  MsSeq Custnum CLIType TariffBundle
    INDEX MsSeqDate  MsSeq Custnum PeriodDate
    INDEX PeriodDate MsSeq Custnum PeriodDate DESC.

FUNCTION fGetCLIType RETURNS LOGICAL (INPUT piMsSeq  AS INT):

    DEF VAR ldaDate AS DATE NO-UNDO.
    DEF VAR liTime  AS INT  NO-UNDO.
    DEF VAR ldFromPer AS DECIMAL NO-UNDO.
    DEF VAR ldToPer AS DECIMAL NO-UNDO.
    DEF VAR ldInvoiceFrom AS DECIMAL NO-UNDO.

    ASSIGN
       ldFromPer   = fMake2Dt(IF Invoice.FirstCall NE ?
                              THEN Invoice.FirstCall
                              ELSE Invoice.FromDate,0)
       ldToPer     = fMake2DT(Invoice.ToDate,86399)
       ldInvoiceFrom = fMake2DT(DATE(MONTH(Invoice.Todate),
                                     1,
                                     YEAR(Invoice.ToDate)),0).

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

    RETURN TRUE.

END FUNCTION. /* FUNCTION fGetCLIType RETURNS CHARACTER */

ldaPeriodEnd =  DATE(MONTH(idaStart),1,YEAR(idaStart)) - 1.
ldaPeriodStart = DATE(MONTH(ldaPeriodEnd),1,YEAR(ldaPeriodEnd)).
ldPeriodFromTS = fHMS2TS(ldaPeriodStart,"00:00:00").
ldPeriodEndTS = fHMS2TS(ldaPeriodEnd,"23:59:59").

/* Invoice Grain file */
lcInvGrainFile = "/store/riftp/tmp/invoice_row_dump_test_#DATE.txt".
lcInvGrainFile = REPLACE(lcInvGrainFile,
                           "#DATE",STRING(YEAR(ldaPeriodEnd),"9999") +
                           STRING(MONTH(ldaPeriodEnd),"99") +
                           STRING(DAY(ldaPeriodEnd),"99")).

icFileName = "/store/riftp/tmp/billing_item_totals_#DATE.log".
IF icFileName = "" OR icFileName = ? THEN
   RETURN "ERROR:File name not defined".
icFileName = REPLACE(icFileName,"#DATE",STRING(YEAR(ldaPeriodEnd),"9999") +
                                        STRING(MONTH(ldaPeriodEnd),"99") +
                                        STRING(DAY(ldaPeriodEnd),"99")).

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   DISP oiInvCount LABEL "Invoices" FORMAT ">>>>>>>>>9" 
      WITH SIDE-LABELS OVERLAY ROW 10 CENTERED TITLE " Collecting " FRAME fQty.
END.

OUTPUT STREAM sLogInvGrain TO VALUE(lcInvGrainFile).
PUT STREAM sLogInvGrain UNFORMATTED SKIP
   "PeriodBegin;PeriodEnd;InvNum;InvDate;ExtInvId;InvCustNum;SubInvNum;SubInvCustNum;MsSeq;CLI;"
   "CLIType;CCN;BillCode;Minutes;DataAmt(MB);Quantity;AmountEUR;TariffBundle;FromDate;ToDate".


Invoices:
for EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
         Invoice.Brand    = gcBrand   AND
         Invoice.InvDate >= idaStart  AND
         Invoice.InvDate <= idaEnd    AND
         Invoice.InvType  = iiInvType,
    EACH SubInvoice OF Invoice NO-LOCK:

   oiInvCount = oiInvCount + 1.
   IF oiInvCount MOD 100 = 0 THEN DO:
      DISP oiInvCount WITH FRAME a.
      PAUSE 0.
   END.

   IF NOT SESSION:BATCH AND oiInvCount mod 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiInvCount WITH FRAME fQty.
   END.

   EMPTY TEMP-TABLE ttMsOwner NO-ERROR.

   /* Pick all the subs types */
   fGetCLIType(SubInvoice.MsSeq).
  
   FOR EACH InvRowCounter WHERE
            InvRowCounter.InvNum = Invoice.InvNum AND
            InvRowCounter.SubInvNum = SubInvoice.SubInvNum NO-LOCK:

      IF InvRowCounter.Quantity <= 0 AND InvRowCounter.Amount <= 0 THEN
         NEXT.

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

      FIND FIRST ttCCN WHERE
                 ttCCN.RepCCN   = InvRowCounter.CCN AND
                 ttCCN.BillCode = InvRowCounter.BillCode AND 
                 ttCCN.CLIType  = lcCLIType AND
                 ttCCN.TariffBundle = lcBundle NO-ERROR.

      IF NOT AVAIL ttCCN THEN DO:

         FIND FIRST Mobcdr WHERE
                    Mobcdr.InvCust  = Invoice.CustNum AND
                    Mobcdr.InvSeq   = SubInvoice.InvSeq  AND
                    Mobcdr.BillCode = InvRowCounter.BillCode AND
                    Mobcdr.CCN      = InvRowCounter.CCN NO-LOCK NO-ERROR.

         CREATE ttCCN.
         ASSIGN
            ttCCN.CC       = Mobcdr.Spocmt WHEN AVAIL Mobcdr
            ttCCN.RepCCN   = InvRowCounter.CCN
            ttCCN.BillCode = InvRowCounter.BillCode
            ttCCN.CLIType  = lcCLIType
            ttCCN.RowType  = 1
            ttCCN.TariffBundle = lcBundle.
      END.

      ASSIGN
         ttCCN.Seconds = ttCCN.Seconds + InvRowCounter.Duration
         ttCCN.Amount  = ttCCN.Amount  + InvRowCounter.Amount
         ttCCN.DataAmt = ttCCN.DataAmt + InvRowCounter.DataAmt
         ttCCN.Qty     = ttCCN.Qty + InvRowCounter.Quantity.

      /* update InvGrain */
      PUT STREAM sLogInvGrain UNFORMATTED SKIP
                 ldaPeriodStart                   ";"
                 ldaPeriodEnd                     ";"
                 Invoice.InvNum                   ";"
                 Invoice.InvDate                  ";"
                 Invoice.ExtInvId                 ";"
                 Invoice.CustNum                  ";"
                 SubInvoice.SubInvNum             ";"
                 SubInvoice.CustNum               ";"
                 SubInvoice.MsSeq                 ";"
                 SubInvoice.CLI                   ";"
                 lcCLIType                        ";"
                 InvRowCounter.CCN                ";"
                 InvRowCounter.BillCode           ";"
                 ROUND((InvRowCounter.Duration / 60),4) ";"
                 ROUND(InvRowCounter.DataAmt / (1024 * 1024),4) ";"
                 InvRowCounter.Quantity           ";"
                 ROUND(InvRowCounter.Amount,4)    ";"
                 lcBundle                         ";"
                 InvRowCounter.FromDate           ";"
                 InvRowCounter.ToDate             .

   END.

   FOR EACH InvRow NO-LOCK WHERE
            InvRow.InvNum = Invoice.InvNum AND 
            InvRow.SubInvNum = SubInvoice.SubInvNum AND
            LOOKUP(STRING(InvRow.RowType),"2,6") = 0:

         IF InvRow.FromDate = ? THEN
            ldaFromDate = ldaPeriodStart.
         ELSE
            ldaFromDate = InvRow.FromDate.

         IF InvRow.ToDate = ? THEN
            ldaToDate = ldaPeriodEnd.
         ELSE
            ldaToDate = InvRow.ToDate.

         FIND FIRST ttMsOwner NO-LOCK WHERE
                    ttMsOwner.MsSeq = SubInvoice.MsSeq AND
                    ttMsOwner.Custnum = Invoice.Custnum AND
                    ttMsOwner.PeriodDate >= ldaTodate
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

         FIND FIRST ttCCN WHERE
                    ttCCN.BillCode = InvRow.BillCode AND
                    ttCCN.RowType  = 2 AND 
                    ttCCN.CLIType  = lcCLIType AND
                    ttCCN.TariffBundle = lcBundle NO-ERROR.

         IF NOT AVAIL ttCCN THEN DO:

            CREATE ttCCN.
            ASSIGN
               ttCCN.BillCode = InvRow.BillCode
               ttCCN.RowType  = 2
               ttCCN.CLIType  = lcCLIType
               ttCCN.TariffBundle = lcBundle.
         END.

         ASSIGN
            ttCCN.Amount  = ttCCN.Amount  + InvRow.Amt
            ttCCN.Qty     = ttCCN.Qty + InvRow.Qty.

         /* update InvGrain */ 
         PUT STREAM sLogInvGrain UNFORMATTED SKIP
                    ldaPeriodStart                   ";"
                    ldaPeriodEnd                     ";"
                    Invoice.InvNum                   ";"
                    Invoice.InvDate                  ";"
                    Invoice.ExtInvId                 ";"
                    Invoice.CustNum                  ";"
                    SubInvoice.SubInvNum             ";"
                    SubInvoice.CustNum               ";"
                    SubInvoice.MsSeq                 ";"
                    SubInvoice.CLI                   ";"
                    lcCLIType                        ";"
                    ""                               ";"
                    InvRow.BillCode                  ";"
                    0                                ";"
                    0                                ";"
                    InvRow.Qty                       ";"
                    ROUND(InvRow.Amt,4)              ";"
                    lcBundle                         ";"
                    ldaFromDate                      ";"
                    ldaToDate                        .

   END.
   
   /*
   IF iiUpdateInterval > 0 AND oiInvCount MOD iiUpdateInterval = 0 THEN DO:
      IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiInvCount) THEN
         RETURN "ERROR:Stopped".
   END.   
   */ 
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

PUT STREAM sLogInvGrain UNFORMATTED SKIP.
OUTPUT STREAM sLogInvGrain CLOSE.
/*
UNIX SILENT VALUE("gzip " + lcInvGrainFile).
lcInvGrainFile = lcInvGrainFile + ".gz".
*/

OUTPUT STREAM sLog TO VALUE(icFileName).

PUT STREAM sLog UNFORMATTED
   SKIP
   "Start;End;CallCase;RepCCN Code;RepCCN Name;Product Code;Product Name;" 
   "CLIType;Minutes;DataAmt(MB);Quantity;Amount EUR;TariffBundle" SKIP.

FOR EACH ttCCN NO-LOCK USE-INDEX RowType.

   FIND FIRST CCN WHERE
              CCN.Brand = gcBrand AND  
              CCN.CCN = ttCCN.RepCCN NO-LOCK NO-ERROR.

   FIND FIRST BillItem WHERE
              BillItem.Brand = gcBrand AND
              BillItem.BillCode = ttCCN.BillCode NO-LOCK NO-ERROR.


   IF ttCCN.RowType = 1 THEN DO:
      PUT STREAM sLog UNFORMATTED
      ldaPeriodStart                   ";"
      ldaPeriodEnd                     ";"
      ttCCN.CC                         ";"
      ttCCN.RepCCN                     ";"
      CCN.CCNName                      ";"
      ttCCN.BillCode                   ";"
      BillItem.BIName                  ";"
      ttCCN.CLIType                    ";"
      (IF INT64(ttCCN.Seconds / 60) = ? THEN 0
       ELSE INT64(ttCCN.Seconds / 60))   ";"
      (IF INT64(ttCCN.DataAmt / (1024 * 1024)) = ? THEN 0
       ELSE INT64(ttCCN.DataAmt / (1024 * 1024))) ";"
      ttCCN.Qty                        ";"
      ROUND(ttCCN.Amount,4)             ";"
      ttCCN.TariffBundle                SKIP.
   END.
   ELSE DO:
      PUT STREAM sLog UNFORMATTED
      ldaPeriodStart                   ";"
      ldaPeriodEnd                     ";"
      ";;;"
      ttCCN.BillCode                   ";"
      BillItem.BIName                  ";"
      ttCCN.CLIType                    ";"
      "0;0;"
      ttCCN.Qty                        ";"
      ROUND(ttCCN.Amount,4)            ";"
      ttCCN.TariffBundle               SKIP.
   END.
END.

OUTPUT STREAM sLog CLOSE.

/*
IF iiFRProcessID > 0 THEN DO TRANS:
   CREATE FuncRunResult.
   ASSIGN 
      FuncRunResult.FRProcessID = iiFRProcessID
      FuncRunResult.FRResultSeq = 1
      FuncRunResult.CharParam   = lcFinalFile.
END.
*/
RETURN "".


