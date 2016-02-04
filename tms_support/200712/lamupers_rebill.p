/* logic routines FOR billing       

            21.03.2002/kl  FIRST Version
            25.03.2002/aam latest changes FOR JG added (bitems, VATAmt handling
                           FOR mobile fees etc.),
                           CREATE optrans-log when overpayment used,
                           invoice NOT created IF only BillCode is the one
                           defined in parameter "ProdNoBill" (FINKEH),
                           Customer.AdvPaym used LIKE Customer.Deposit[2]
            27.03.2002/ht  in pCancel Customer exclusive-lock               
            10.04.02 jp    dont UPDATE INPUT pdate1            
            15.04.2002/aam pGetAmt RETURNS also the VALUE of invoices,
                           when creating the first "newinv" FOR InvGroup FIELD
                           "invno" was set from invgroup.ig-invno -> LAST 
                           invoice from previous billing RUN was included 
                           in "newinv",
                           get exact dates FOR invoice lines older than 
                           CURRENT invoicing Period 
            17.04.2002/aam amount without adv.payment must exceed MINIMUM
                           invoicing Limit,
                           use AdvPaymAcc when Customer.AdvPaym used
            16.04.2002/kl  updating bufseq.InvNum was missing
                           pUpdInvSeq changed into pUpdInvGroup
            17.05.2002/aam mark VATIncl TO Invoice
            28.05.2002/aam CustIntEvent.asno replaced WITH CustIntEvent.CustNum
            07.06.2002/aam use Invoice.OverPaym AND OPAccNum FOR overpayment,
                           VAT handling FOR advance payment 
            13.06.2002/jp  RowType = 5 FOR GSMJFREE minimuminvoice
            26.07.2002/tk  eventlog functions persistent
            31.07.2002/aam get VATCode from BillItem,
                           invoice can include lines with several VAT%
            14.08.2002/jp  FATIME
            14.08.2002/jp  RowType = 7 FOR Free Air Time
            20.08.2002/tk  BDestHist     
            04.09.2002/aam billrund.i,
                           volume discounts 
            26.09.2002/aam customer balances in table CustBal and CustCount
            02.10.2002/jp  create FFItems automatically if last 
            10.10.2002/jp  Call Counter balances into callcount
            10.10.2002/jp  NEW InvSeq TO previously uninvoiced CallCounters
            14.10.2002/aam fToHomeCurr() & fCurrRate()
            23.10.2002/aam pCleanMem
            31.10.2002/jp  fatime.fatclass added
            01.11.2002/ jp fatime for fixed fees
            21.11.02 lp    fcustacc.i added   
            03.12.2002/aam check direct debit and mark Invoice.ddebit
            10.12.2002/aam use del_inv in pCancel (all were not cancelled),
                           procedure pCleanRun,
                           credit loss posting (makepaym)
            31.12.2002/aam eventlog row for cancel event,
                           delete db-eventlog when run cancelled,
                           katun instead of lUser (lUser was not assigned)
            11.02.2003/aam ChargeType and DelType from customer to invoice 
            21.02.2003/aam BillInterest determines interest billing,
                           BillDblVat allows different vat methods,
                           TimeBands and TariffNum into summary tables,
                           time period into InvAsub,
                           mark prefix to invrow (PrefixBillItem),
                           use of PriceList.CurrUnit
            15.04.2003/aam deposit invoice (invtype = 3),
                           makepaym returns voucher nbr
            15.04.2003/jp  Reset CallCounter Mobile limit %
            22.04.2003/aam find invgroup for xCustomer-buffer in pCreateInv
            24.04.2003/jp  new mobile limit always 0
            08.05.2003/jp  callcounter
            19.05.2003/aam update invoice qty and amount to InvRunLog
            28.05.2003/aam payment term for deposit invoices from TMSParam
            04.06.2003/aam prefix for invrow also from mobcdr
            06.06.2003/aam combine contract and single fee rows by product
            17.06.2003/aam don't add "46" to mobcdr.cli
            18.06.2003/aam FatType instead of FatClass,
                           new FATime type: 2 (all),
                           fMakeFATimeRow()
            24.06.2003/aam pVATIncluded corrected (retrieval of vat%)
            01.07.2003/aam pVASCDR (RowType = 8)            
            15.07.2003/jp  next-value(cinvseq) instead next-value(invseq).
            22.08.2003/aam set claimperm = false for deposit invoices,
                           set WInvDisp = FALSE as default
            11.09.2003/aam brand 
            01.10.2003 jp  fmakecontractmore, new rules
            02.10.2003/aam SpecDel from customer,
                           take ArAccNum from CustCat (if defined)
            14.10.2003/aam gather fees according to InvCustCombine
            24.11.2003/aam SingleFee.Active
            28.11.2003/jp  Find  billitem with brand (fatime)
            28.11.2003/jp  when fatime, dont create invoice automatically
            01.12.2003/ jp brand code for singlefee
            04.12.2003/aam new VAT logic (pInvRowVat etc.),
                           reject bill if product for row not found,
                           IDel* from customer,
                           negative nbrs for ttcli&ttccn.lanro
            18.12.2003/jp  use mobcdr.grossamount
            18.03.2004/aam make InvRows by CLI/BillItem
            30.03.2004 kl  fixes after index changes
            23.04.2004/aam also adv.payment invoices from pDepositInvoice  
            19.04.2004/aam use of callamtwvat with fatime corrected
            20.04.2004/aam create fee for call spec (creasfee)
            22.04.2004/aam use ttCli.From/ToDate with fatime
            28.04.2004/aam update DataAmt,
                           fChkDueDate
            05.05.2004/aam BillRun
            17.05.2004/aam amount to eventlog for low value
            09.06.2004/aam also normal fee invoices through pDepositInvoice
            18.06.2004/aam new logic for fatime of calls 
            23.08.2004/aam orderid to pDepositInvoice
            28.09.2004/aam BillItem.AltAccNum for own use (OwnUseCategory)
            03.01.2005/aam RepCode from SubSer (MobSub)
            20.01.2005/aam SaldoLimit from SubSer (not MobSub.Balance)
            03.02.2005/aam check RowType in ttIR-find for CDRs,
                           round DiscAmt to 2 decimals in fMakeFatimeRow,
                           use fCallSpecDuring()
            31.03.2005/aam set Fatime.Used back to 0 if invoice is cancelled 
            12.04.2005/aam use InvAsub.GenPrice to store MPM,
                           parameter InvCCNColl determines whether InvCCN
                           is collected or not
            29.04.2005/aam correct amount to fatime.amt when transferring,
                           make ttInvoiceItem 5 for transferred fatimes
            04.05.2005/aam convert DataAmt in ttCLI to Kbs in a separate loop,
                           delete created singlefees when invoice rejected 
            06.05.2005/aam InvRow.Amt with 3 decimals,
                           don't run eventlog.i as persistent,
                           log-messages modified,
                           use-index CustNum with CustIntEvent
            04.08.2005/aam use MobCDR.MPMAmt for mpm
            14.09.2005/aam check also ciperiod when Fatime.FatType=2
            20.09.2005/aam invoice number with fGetInvNum(),
                           proper index for NewInv
            21.09.2005/jp&aam run dblcalls and rerate (persistent)
            26.09.2005/aam MPMRid and ServRid to InvASub (ttCLI)
            29.09.2005/aam customer counter in pCreateInv,
                           disp starttime and duration,
                           customer nbr order in pCreateInv
            08.11.2005/aam error-handling to invoice creation (same nbr used),
                           check fee periods
            18.11.2005/aam set FromDate and ToDate in fInvRowVat when combining
            23.11.2005/aam new parameters to pCreateInv from pDepositInv
            30.11.2005/aam vat handling for adv.payment type fat rows  
            02.12.2005/aam check fatime.period before using it 
            18.01.2006/aam qty for fat rows always 1,
                           fPrintCustName()
            06.02.2006/jp  ttServiceCounter
            28.04.2006/aam new fat logic (priority,fatperc),
                           ttEventCust
            23.05.2006/aam cli based sums to fattype=0 (all)
            20.06.2006/aam pEndInvoice,
                           skip customers whose EndInvType>0
            07.07.2006/aam EndInvType removed from foreach               
            26.09.2006/aam check 'concerns' for fixedfees when endinvoice
            28.11.2006/aam pCashInvoice,
                           new vat handling
            29.11.2006/aam ExtInvID
            21.12.2006/aam minimum invoice amount
            04.01.2007/aam receivable account from clitype
            08.01.2007/aam cli based invoices i.e. one cli per invoice,
                           interests to an invoice row (rowtype 9)
            29.01.2007/aam take seconds into invrow.minutes               
            02.02.2007/aam minimum consumption before fatimes
            22.02.2007/aam changes to base amount for fatime type 2
            08.03.2007/mvi added pCreateTestInv, and invtype 99 handled like 1
            13.04.2007/aam pCreateODInvoice,
                           new parameters to pCreateInv,
                           mark/check minimum consumption in ActionLog
            18.04.2007/aam use MobSub.RepCodes temporarily as 'billing denied'
            30.04.2007/aam nul current month's SaldoCounter
            10.05.2007/aam ErrorLog
            29.06.2007/aam better checking of ExtInvID for doubles 
            05.07.2007 kl  Minimum Consumption for Leaving Customer
            17.07.2007 kl  TERMPERIOD From & ToDate for SingleFees
            22.08.2007/aam minimum consumption as msseq based (ActionLog)
            23.08.2007/aam ttIr and whole invoice as msseq based
*/

{Syst/commali.i}                                                                   
{Syst/country.i}
{Func/fixedfee.i}
{Func/cparam.i2}
{Func/timestamp.i}
{Func/fapvat.i}
{Inv/billrund.i {1}}
{Func/fcustbal.i}
{Func/fcustcnt.i}
{Func/nncoit2.i}
{Func/fcurrency.i}
{Syst/eventval.i}
{Func/fvasinv.i}
{Func/fduedate.i}
{Func/fsubser.i}
{Func/lib/eventlog.i}
{Func/finvnum.i}
{Func/ftaxdata.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

END.


&IF "{&InitPersistent}" NE "NO" 
&THEN 
DEF VAR fhVDHandle AS HANDLE  NO-UNDO. 
DEF VAR fhDCHandle AS HANDLE  NO-UNDO.
DEF VAR fhRRHandle AS HANDLE  NO-UNDO.

/* volume discount functions */
FUNCTION fVolDiscFixed RETURNS LOG (BUFFER VFixCDR FOR FixCDR) IN fhVDHandle. 
FUNCTION fVolDiscMob   RETURNS LOG (BUFFER VMobCDR FOR MobCDR) IN fhVDHandle. 

/* load volume discount procedures */
RUN Inv/voldisc    PERSISTENT SET fhVDHandle. 
RUN Mm/domcopers  PERSISTENT SET fhDCHandle.
RUN Rate/cust_ratep PERSISTENT SET fhRRHandle.
&ENDIF

DEF VAR lUpdacc       AS LOG  NO-UNDO.
DEF VAR lCustVat      AS LOG  NO-UNDO.
DEF VAR CallAmt       AS DE   NO-UNDO.
DEF VAR CallAmtWVat   AS DEC  NO-UNDO. 
DEF VAR CallQty       AS i    NO-UNDO.
DEF VAR llFatVat      AS LOG  NO-UNDO. 
DEF VAR DiscAmt       AS DE   NO-UNDO.
DEF VAR DiscQty       AS I    NO-UNDO.
DEF VAR liseq         AS I    no-undo.
DEF VAR lD2D          AS DE   NO-UNDO.
DEF VAR lDBLVat       AS LO   NO-UNDO.
DEF VAR lPrevVAT      AS LOG  NO-UNDO.
DEF VAR llAllowDbl    AS LOG  NO-UNDO.   
DEF VAR ldCustOP      AS DEC  NO-UNDO.
DEF VAR ldCustAP      AS DEC  NO-UNDO.
DEF VAR ldCredLoss    AS DEC  NO-UNDO.
DEF VAR liMinDays     AS INT  NO-UNDO. 
DEF VAR liCLossCust   AS INT  NO-UNDO. 
DEF VAR lcMarkPref    AS CHAR NO-UNDO. 
DEF VAR ldNet         AS DEC  NO-UNDO. 
DEF VAR ldGross       AS DEC  NO-UNDO.
DEF VAR lcCustPL      AS CHAR NO-UNDO. 
DEF VAR liDepoAcc     AS INT  NO-UNDO. 
DEF VAR lcDepoItem    AS CHAR NO-UNDO. 
DEF VAR liDepoPTerm   AS INT  NO-UNDO. 
DEF VAR lcMSSeqLst    AS CHAR NO-UNDO. 
DEF VAR liIRLQty      AS INT  NO-UNDO.
DEF VAR ldIRLAmt      AS DEC  NO-UNDO. 
DEF VAR liSilent      AS LOG  NO-UNDO INIT FALSE. 
DEF VAR lcCLI         AS CHAR NO-UNDO. 
DEF VAR liMsSeq       AS INT  NO-UNDO. 
DEF VAR liOrderID     AS INT  NO-UNDO. 
DEF VAR lcOwnUse      AS CHAR NO-UNDO.
DEF VAR liOwnUsePA    AS INT  NO-UNDO.
DEF VAR liInvCCN      AS INT  NO-UNDO.
DEF VAR ldtBSDate     AS DATE NO-UNDO.
DEF VAR liBSTime      AS INT  NO-UNDO. 
DEF VAR lRejBill      AS LOG  NO-UNDO. 
DEF VAR lcRejReason   AS CHAR NO-UNDO. 
DEF VAR lcRejCLI      AS CHAR NO-UNDO. 
DEF VAR liArAccNum    AS INT  NO-UNDO. 
DEF VAR lAmt          AS INT  NO-UNDO.
DEF VAR lSum2         AS INT  NO-UNDO.
DEF VAR lSum3         AS DEC  NO-UNDO.
DEF VAR pODate        AS DATE NO-UNDO.
DEF VAR ReceivAcc     AS INT  NO-UNDO.
DEF VAR RoundAcc      AS INT  NO-UNDO.
DEF VAR OTIntAcc      AS INT  NO-UNDO.
DEF VAR OverPayAcc    AS INT  NO-UNDO.
DEF VAR AdvPaymAcc    AS INT  NO-UNDO.
DEF VAR liCLossAcc    AS INT  NO-UNDO. 
DEF VAR lNotBilled    AS CHAR NO-UNDO. 
DEF VAR llBillInt     AS LOG  NO-UNDO.
DEF VAR llInvComb     AS LOG  NO-UNDO INIT FALSE. 
DEF VAR liVASAcc      AS INT  NO-UNDO. 
DEF VAR ldVatFactor   AS DEC  NO-UNDO. 
DEF VAR liFatPer      AS INT  NO-UNDO.
DEF VAR lcBillRun     AS CHAR NO-UNDO.
DEF VAR liLastCreated AS INT  NO-UNDO.
DEF VAR liEndInvType  AS INT  NO-UNDO. 
DEF VAR lcIntItem     AS CHAR NO-UNDO.
DEF VAR lcBillDeny    AS CHAR NO-UNDO.

DEF BUFFER xFATime FOR FATime.
DEF BUFFER bFATime FOR FATime.

DEF TEMP-TABLE irow NO-UNDO
   FIELD dt1  LIKE InvRow.FromDate
   FIELD dt2  LIKE InvRow.FromDate
   FIELD BillCode LIKE InvRow.BillCode
   FIELD InvRowId LIKE InvRow.InvRowNum

   INDEX dt1 AS primary
      dt1
      dt2
      BillCode.

DEF TEMP-TABLE newinv NO-UNDO
   FIELD InvNum     AS i
   FIELD InvSeq     AS i
   FIELD Qty        AS dec
   FIELD vatamt     AS dec 
   FIELD InvGroup   AS c
   FIELD InvType    AS INT
   FIELD SeqPrefix  AS CHAR 
   FIELD ExtInvID   AS CHAR 
   FIELD UpdCustBal AS lo
   INDEX InvGroup InvGroup InvType ExtInvID.

DEF TEMP-TABLE ttIR NO-UNDO LIKE InvRow 
   FIELD MsSeq  AS INT 
   FIELD bOld   AS lo
   FIELD bvat   AS LO
   /* dnet & dgross are used to calculate sums first without rounding */
   FIELD dnet   AS DEC
   FIELD dgross AS DEC.

DEF TEMP-TABLE ttInv NO-UNDO LIKE Invoice
   FIELD Vat0Amt AS DEC. 

DEF TEMP-TABLE ttCCN NO-UNDO LIKE InvCCN 
   FIELD bvat  AS LO
   FIELD CLI   AS CHAR 
   FIELD MsSeq AS INT
   INDEX iprod MsSeq InvSeq CCN BillCode  .

DEF TEMP-TABLE ttCLI NO-UNDO LIKE InvASub 
   FIELD bvat    AS LO
   FIELD AmtWVat AS DEC 
   FIELD MsSeq   AS INT
   INDEX iccn  MsSeq InvSeq CLI CCN BillCode .

DEF TEMP-TABLE ttRowVat NO-UNDO
   FIELD VatPerc    AS DEC 
   FIELD VatCode    AS INT 
   FIELD VatAcc     AS INT
   FIELD Amount     AS DEC
   FIELD CLI        AS CHAR
   FIELD MsSeq      AS INT
   INDEX CLI CLI MsSeq VatPerc.

DEF BUFFER xxIR  FOR ttIR.
DEF BUFFER xxcli FOR ttCLI.
DEF BUFFER xxccn FOR ttCCN.

DEF TEMP-TABLE ttInvoiceItem
   FIELD tType  AS INT
   FIELD MsSeq  AS INT
   FIELD tRecId AS RECID
   INDEX tType AS PRIMARY tType MsSeq.

DEF TEMP-TABLE ttCallFat NO-UNDO
   FIELD CLI      AS CHAR
   FIELD MsSeq    AS INT 
   FIELD BillCode AS CHAR
   FIELD CCN      AS INT
   FIELD Qty      AS INT
   FIELD Amt      AS DEC
   FIELD AmtWVat  AS DEC
   FIELD Period   AS INT
   FIELD VatIncl  AS LOG 
   INDEX BillCode CLI BillCode CCN Period
   INDEX CCN      CLI CCN. 

DEF TEMP-TABLE ttEventCust NO-UNDO
   FIELD CustNum AS INT.

DEF TEMP-TABLE ttCLIDisc NO-UNDO
   FIELD CLI      AS CHAR
   FIELD MsSeq    AS INT 
   FIELD Amt      AS DEC
   FIELD AmtWVat  AS DEC
   FIELD FromDate AS DATE
   INDEX CLI CLI MsSeq.
   
DEFINE TEMP-TABLE ttServiceCounter NO-UNDO
   FIELD MSSeq    LIKe Mobsub.MSSeq  
   FIELD CLI      AS   CHAR
   FIELD Period   AS   INT 
   FIELD SLSeq    AS   INT
   FIELD Amount   AS   DEC 
   INDEX MSSeq MSSeq Period SLSeq.

DEF TEMP-TABLE ttInvSeq NO-UNDO
   FIELD InvSeq AS INT
   FIELD MsSeq  AS INT  
   INDEX MsSeq MsSeq.
   

form
   lAmt            column-label "Amt"
   Invoice.InvNum   column-label "Invoice"
   Invoice.CustNum  column-label "Cust.nr"
   Invoice.CustName column-label "Customer name"  format "x(20)"
   Invoice.PostOffice  column-label "Address"       format "x(10)"
   Invoice.AmtExclVAT column-label "Value ex. VAT"
with row 3 centered overlay 
   title " HANDLING  Started " + 
         STRING(ldtBSDate,"99.99.99") + " " +
         STRING(liBSTime,"hh:mm:ss") + " " 
   12 DOWN FRAME LOG.


{Func/tmsparam.i ReceivAcc      RETURN} ReceivAcc  = TMSParam.IntVal.
{Func/tmsparam.i RoundAcc       RETURN} RoundAcc   = TMSParam.IntVal.
{Func/tmsparam.i OTIntAcc       RETURN} OTIntAcc   = TMSParam.IntVal.
{Func/tmsparam.i OverPayAcc     RETURN} OverPayAcc = TMSParam.IntVal.
{Func/tmsparam.i CreditLossAcc  RETURN} liCLossAcc = TMSParam.IntVal.
/* NOT mandatory */
{Func/tmsparam.i AdvPaymAcc}   IF AVAILABLE TMSParam THEN 
                                       AdvPaymAcc = tmsparam.intVal.
{Func/tmsparam.i ProdNoBill     RETURN} lNotBilled = TMSParam.CharVal.
{Func/tmsparam.i BillInterest}   IF AVAILABLE TMSParam THEN
                                       llBillInt = (TMSParam.IntVal = 1). 
{Func/tmsparam.i BillDblVat}   IF AVAILABLE TMSParam THEN
                                       llAllowDbl = (TMSParam.IntVal = 1). 
{Func/tmsparam.i CLossCustomer  RETURN} liCLossCust = TMSParam.IntVal.
{Func/tmsparam.i PrefixBillItem RETURN} lcMarkPref  = TMSParam.CharVal.
{Func/tmsparam.i InvCustCombine}   IF AVAILABLE TMSParam THEN
                                       llInvComb = (TMSParam.IntVal = 1). 
ASSIGN lcOwnUse   = fCParamC("OwnUseCategory")
       liOwnUsePA = fCParamI("OwnUsePaymAcc")
       liInvCCN   = fCParamI("InvCCNColl")
       lcIntItem  = fCParamC("InterestItem").
       
IF liOwnUsePA = ? THEN liOwnUsePA = 0.
       
IF AdvPaymAcc = 0 THEN AdvPaymAcc = OverPayAcc. 
     
&IF "{&InitPersistent}" NE "NO" 
&THEN 
RUN pInitializeRerate  IN  fhRRHandle.
&ENDIF

FUNCTION fInvoiceItem RETURNS LOGICAL
  (INPUT piType  AS INTEGER,
   INPUT iiMsSeq AS INT,
   INPUT prRecId AS RECID):

   CREATE ttInvoiceItem.
   ASSIGN
      ttInvoiceItem.tType  = piType
      ttInvoiceItem.MsSeq  = iiMsSeq
      ttInvoiceItem.tRecId = prRecId.
   
   RETURN TRUE.

END.
                          
FUNCTION fMarkInvoiceItems RETURNS DECIMAL
  (INPUT iiMsSeq AS INT,
   INPUT pInvNo  AS INTEGER):
  
   DEF VAR lValue AS DECIMAL NO-UNDO.
   
   /* TYPE 1: mark ALL contract fee rows */
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 1 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
       EACH FFItem EXCLUSIVE-LOCK WHERE
            RECID(FFItem) = ttInvoiceItem.tRecId:
  
      ASSIGN
         FFItem.InvNum = pInvNo WHEN pInvNo NE 0
         FFItem.BILLED = FALSE  WHEN pInvNo  = 0
         lValue        = lValue + FFItem.Amt.
  
   END.
  
   /* TYPE 2: mark ALL single fee rows */  
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 2 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
            EACH SingleFee EXCLUSIVE-LOCK WHERE
            RECID(SingleFee) = ttInvoiceItem.tRecId:
  
      ASSIGN
         SingleFee.InvNum = pInvNo WHEN pInvNo NE 0
         SingleFee.BILLED = FALSE  WHEN pInvNo  = 0
         lValue           = lValue + SingleFee.Amt.

      /* if created within this run -> delete */
      IF pInvno = 0 AND SingleFee.CalcObj > "" AND
         SingleFee.CalcObj = lcBillRun
      THEN DELETE SingleFee.
                             
   END.
  
   /* TYPE 3: mark ALL FATIME fee rows */
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 3 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
       EACH FATime EXCLUSIVE-LOCK WHERE
            RECID(FATime) = ttInvoiceitem.tRecId:
  
      ASSIGN
         FATime.InvNum   = pInvNo WHEN pInvNo NE 0
         FATime.Used     = 0      WHEN pInvno = 0
         FATime.TransQty = 0      WHEN pInvno = 0
         lValue          = lValue + FATime.Amt.
  
   END.
  
   /* TYPE 4: mark all interest records */
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 4 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
       EACH CustIntEvent EXCLUSIVE-LOCK WHERE
            RECID(CustIntEvent) = ttInvoiceitem.tRecId:
  
      ASSIGN
         CustIntEvent.BilledInvNum = pInvNo WHEN pInvNo NE 0
         lValue                    = lValue + CustIntEvent.Amt.
     
   END.

   /* TYPE 5: delete transferred FAT rows if invoice was rejected */
   IF pInvno = 0 THEN 
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 5 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
      FIRST FATime EXCLUSIVE-LOCK WHERE
            RECID(FATime) = ttInvoiceitem.tRecId:

      DELETE FATime.
   END. 
   
   RETURN lValue.

END.

FUNCTION fErrorLog RETURNS LOGIC
  (iiCustNum AS INT,
   icCLI     AS CHAR,
   icError   AS CHAR):
 
   /* save to db for reporting */
   CREATE ErrorLog.
   ASSIGN ErrorLog.Brand     = gcBrand
          ErrorLog.ActionID  = "BRUN"
          ErrorLog.TableName = "Customer"
          ErrorLog.KeyValue  = STRING(iiCustnum)
          ErrorLog.ErrorChar = icCLI
          ErrorLog.ErrorMsg  = icError.
          ErrorLog.ActionTS  = fMakeTS().
    
END FUNCTION.

FUNCTION fMakeFATimeRow RETURNS LOGICAL
   (INPUT-OUTPUT idAmt   AS DEC,
    INPUT-OUTPUT iiQty   AS INT,
    INPUT        iiMsSeq AS INT).

   DEF VAR ldtPerDate AS DATE NO-UNDO.
   
   IF idAmt <= 0 OR
      (FATime.Amt > 0 AND FATime.amt - FATime.used - FATime.TransQty <= 0) OR
      (FATime.FatPerc > 0 AND FATime.Used > 0)
   THEN RETURN FALSE.

   ASSIGN DiscAmt = 0
          DiscQty = 0.

   IF NOT CAN-FIND(BillItem WHERE 
                   BillItem.BillCode = FatGroup.Billcode  AND 
                   BillItem.Brand    = FatGroup.Brand) 
   THEN RETURN FALSE.

   FIND bFatime WHERE RECID(bFatime) = RECID(Fatime) EXCLUSIVE-LOCK.

   /* Calculate amount of discount */
   IF FATime.QtyUnit = "Amt" THEN DO:

      /* fixed amount */  
      IF FATime.Amt > 0 THEN DO:
       
         IF idAmt >= FATime.amt - FATime.used - FATime.TransQty 
         THEN ASSIGN discamt = FATime.amt - FATime.used - FATime.TransQty
                     idAmt   = idAmt - discAmt.

         ELSE ASSIGN discamt = ROUND(idAmt,2)
                     idAmt   = 0.
      END.
      
      /* percentage */  
      ELSE DO:
         ASSIGN DiscAmt = ROUND(FATime.FatPerc * idAmt / 100,2)
                idAmt   = idAmt - DiscAmt.
      END.
      
      ASSIGN DiscQty      = 1
             bFATime.used = bFATime.used + DiscAmt.
                
   END.

   ELSE IF FATime.QtyUnit = "Qty" AND
           FATime.FATType NE 2     
   THEN DO:

      IF iiQty >= FATime.amt - FATime.used - FATime.TransQty
      THEN ASSIGN discamt = (idAmt / iiQty) * 
                            (FATime.amt - FATime.used - FATime.TransQty)
                  idAmt   = idAmt - discAmt
                  DiscQty = FATime.amt - FATime.used - FATime.TransQty.

      ELSE IF iiqty > 0
      THEN ASSIGN discamt = ROUND(idAmt,2) 
                  idAmt   = 0
                  DiscQty = iiQty.

      ELSE RETURN FALSE. 

      bFATime.used = bFATime.used + DiscQty.
   END.

   ELSE RETURN FALSE.

   /* check that period is valid */ 
   ldtPerDate = fInt2Date(Fatime.Period,1).
   IF ldtPerDate = ? THEN DO:
      ASSIGN lRejBill    = TRUE
             lcRejReason = "FATPeriod"
             lcRejCLI    = FATime.CLI.
      RETURN FALSE.
   END. 
 
   /* combine by product code */
   FIND FIRST ttIR WHERE
              ttIR.BillCode = FatGroup.BillCode AND
              ttIR.CLI      = Fatime.CLI        AND      
              ttIR.bVat     = Fatime.VatIncl    AND
              ttIR.RowType  = 7 NO-ERROR.

   IF NOT AVAIL  ttIR THEN DO:
      CREATE ttIR.
      ASSIGN ttIR.FromDate  = ldtPerDate
             ttIR.ToDate    = fint2DATE(FATime.Period,2)
             ttIR.bVat      = Fatime.VatIncl
             ttIR.BillCode  = FatGroup.BillCode
             ttIR.CLI       = Fatime.CLI 
             ttIR.MsSeq     = iiMsSeq
             ttIR.FFRow     = FALSE
             ttIR.RowType   = 7.
   END.

   ASSIGN ttIR.FromDate  = ? /* MIN(ttir.FromDate,ldtPerDate) */
          ttIR.ToDate    = ? /* MAX(ttir.ToDate,fint2DATE(FATime.Period,2)) */
          ttIR.Qty       = 1 
          ttIR.Amt       = ttIR.Amt      + (DiscAmt * -1)
          ttIR.GrossAmt  = ttIR.GrossAmt + (Discamt * -1)
          ttIR.dgross    = ttIR.dgross   + (Discamt * -1)
          ttIR.dnet      = ttIR.dnet     + (DiscAmt * -1).
  
   fInvoiceItem(3,
                ttIR.MsSeq,
                RECID(bFATime)).
   
   RELEASE bFatime.
   
   RETURN TRUE. 

END FUNCTION.

/* transfer remaining fat amount to be used later */
FUNCTION fTransFatime RETURNS LOGICAL
   (iiMsSeq AS INT):
 
   DEF VAR ldFatRemain AS DEC NO-UNDO.
   
   IF Fatime.Transfer = FALSE THEN RETURN FALSE.
   
   IF FATime.Amt > 0 
   THEN ldFatRemain = FATime.Amt - FATime.Used - FATime.TransQty.
   ELSE ldFatRemain = 0.
   
   IF ldFatRemain > 0 AND ldFatRemain NE FATime.Amt THEN DO:

      CREATE xFATime.
      BUFFER-COPY FATime EXCEPT FATime.FATnum TO xFATime.
         
      ASSIGN xFATime.FatNum      = NEXT-VALUE(ftseq)
             xFATime.OrigFat     = FATime.FatNum
             xFATime.TransPeriod = FATime.Period
             xFatime.Amt         = ldFatRemain
             xFATime.Invnum      = 0
             xFATime.Used        = 0
             xFATime.TransQty    = 0.
      /* no need to alter period, because transferrables are all checked
         regardless of how old their period is */
              
      FIND bFatime WHERE RECID(bFatime) = RECID(Fatime) EXCLUSIVE-LOCK.
      bFATime.TransQty = bFatime.TransQty + ldFatRemain.
      RELEASE bFatime.       

      /* these need to be removed in case invoice is rejected */
      fInvoiceItem(5,
                   iiMsSeq,
                   RECID(xFATime)).
      
      RELEASE xFATime.
   END.
 
   RETURN (ldFatRemain > 0).
   
END FUNCTION.

FUNCTION fInvRowVAT RETURNS LOGICAL
   (iiVATUsage AS INT,
    icRegion   AS CHAR,
    icCategory AS CHAR).
   
   FOR EACH ttIR NO-LOCK:
   
      /* determine vat codes and percents (if SlsAccNum > 0 -> already handled)
      */
      IF ttIR.SlsAccNum = 0 THEN DO:
            
         ASSIGN ttIR.VatPerc = 0
                ttIR.VatCode = 0.
                  
         FIND BillItem NO-LOCK WHERE 
              BillItem.Brand    = gcBrand AND
              BillItem.BillCode = ttir.BillCode NO-ERROR.
         IF NOT AVAILABLE BillItem THEN NEXT. /* will be rejected later on */

         /* set this as default, also for vatusage 3/4 in case prices
            including vat have been used -> remove vat using domestic code,
            vatcode is emptied from row before invoice creation (pCounterVat
            needs also ttIr.vc-perc)
         */
         ttIR.VatCode  = fRegionTaxCode(icRegion,
                                        BillItem.TaxClass).
            
         /* customer's vatusage-code determines final vat handling 
            and sales account 
         */
         CASE iiVATUsage:
         WHEN 0 OR 
         WHEN 1 THEN        ttIR.SlsAccNum = IF icCategory = lcOwnUse
                                             THEN BillItem.AltAccNum
                                             ELSE BillItem.AccNum.
         WHEN 2 THEN        ttIR.SlsAccNum = BillItem.EUConAccNum.
         WHEN 3 THEN        ttIR.SlsAccNum = BillItem.EUAccNum.
         WHEN 4 THEN        ttIR.SlsAccNum = BillItem.FSAccNum.
         END CASE.

         IF ttIR.VatCode > 0 THEN DO:
            FIND VatCode NO-LOCK WHERE 
                 VATCode.VatCode = ttIR.VatCode NO-ERROR.
            IF AVAILABLE VatCode 
            THEN ttIR.VatPerc = VatCode.VatPerc.
         END.    
               
      END.

      /* make sure that all rows use the same vat method (incl/excl) */
      IF ttIR.bvat NE lCustVat THEN DO:

         IF lCustVat = TRUE THEN ASSIGN 
            ttIR.dgross   = ttIR.dgross   * (1 + ttIR.VatPerc / 100)
            ttIR.dnet     = ttIR.dnet     * (1 + ttIR.VatPerc / 100).
         ELSE ASSIGN
            ttIR.dgross   = ttIR.dgross   / (1 + ttIR.VatPerc / 100)
            ttIR.dnet     = ttIR.dnet     / (1 + ttIR.VatPerc / 100).

         FIND FIRST xxIR WHERE
                    xxIR.BillCode = ttir.BillCode AND
                    xxIR.CLI      = ttIR.CLI AND
                    xxIR.Bvat     = lCustVat      AND
                    xxIR.bOld     = ttIR.bOld     AND
                    xxIR.RowType  = ttIR.RowType  AND
                    xxIR.SlsAccNum > 0
         NO-ERROR.

         IF AVAIL xxIR THEN DO:

            ASSIGN
            xxIR.Qty        = xxIR.Qty        + ttIR.Qty  
            xxIR.Minutes    = xxIR.Minutes    + ttIR.Minutes
            xxIR.PeakMin    = xxIR.PeakMin    + ttIR.PeakMin
            xxIR.OffPeakMin = xxIR.OffPeakMin + ttIR.OffPeakMin
            xxIR.dgross     = xxIR.dgross     + ttIR.dgross
            xxIR.dnet       = xxIR.dnet       + ttIR.dnet
            xxIR.FromDate   = MIN(xxIR.FromDate,ttIR.FromDate)
            xxIR.ToDate     = MAX(xxIR.ToDate,ttIR.ToDate).

            DELETE ttIR.     

         END.

         ELSE ttIR.bVat = lCustVat.
      END.
                 
   END.  /* TTIR */         

END FUNCTION.

/* get next external invoice id */
FUNCTION fLocalNextExtID RETURNS CHARACTER
   (icSeqPrefix AS CHAR,
    icExtInvID  AS CHAR):

   DEF VAR liSeqInvNum AS INT NO-UNDO.
   
   /* remove prefix (don't use replace) */
   IF icSeqPrefix > "" AND icExtInvID BEGINS icSeqPrefix THEN DO:
      IF LENGTH(icExtInvID) > LENGTH(icSeqPrefix)
      THEN icExtInvID = SUBSTRING(icExtInvID,LENGTH(icSeqPrefix) + 1).
      ELSE icExtInvID = "".
   END.
         
   liSeqInvNum = INTEGER(icExtInvID) NO-ERROR.
         
   /* invalid integer value */
   IF ERROR-STATUS:ERROR THEN RETURN "".

   RETURN icSeqPrefix + STRING(liSeqInvNum + 1,"99999999").
   
END FUNCTION.


PROCEDURE pCreateInv:

   DEF INPUT  PARAMETER idtInvDate AS Date NO-UNDO.
   DEF INPUT  PARAMETER pDate1     AS Date NO-UNDO.
   DEF INPUT  PARAMETER pDate2     AS Date NO-UNDO.
   DEF INPUT  PARAMETER ciperiod   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilRerate   AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER ilDouble   AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER iiCustQty  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiInvType  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icBillRun  AS CHAR NO-UNDO.

   DEF BUFFER xcustomer    FOR Customer.
   DEF BUFFER bEventCust   FOR Customer.
   DEF BUFFER bttEventCust FOR ttEventCust.
   DEF BUFFER bOwner       FOR MsOwner.

   DEF VAR lCPer      AS INT  NO-UNDO.
   DEF VAR lCPer2     AS INT  NO-UNDO.
   DEF VAR lBSum      AS dec  NO-UNDO.
   DEF VAR lDProd     AS CHAR NO-UNDO.
   DEF VAR lDirDisc   AS INT  NO-UNDO.
   DEF VAR lInvNo     AS INT  NO-UNDO.
   DEF VAR lNotInv    AS dec  NO-UNDO.
   DEF VAR lLoop      AS INT  NO-UNDO.
   DEF VAR pNDate     AS DATE NO-UNDO. 
   
   DEF VAR ldBasis    AS DEC  NO-UNDO.
   DEF VAR liCnt      AS INT  NO-UNDO. 
   DEF VAR lcRepCode  AS CHAR NO-UNDO.
   DEF VAR lcInfo     AS CHAR NO-UNDO. 
   DEF VAR ldFromPer  AS DEC  NO-UNDO.
   DEF VAR ldToPer    AS DEC  NO-UNDO. 
   DEF VAR ldtFrom    AS DATE NO-UNDO.
   DEF VAR ldtTo      AS DATE NO-UNDO. 
   DEF VAR lcMobRep   AS CHAR NO-UNDO. 
   DEF VAR liType     AS INT  NO-UNDO.
   DEF VAR liCustQty  AS INT  NO-UNDO.
   DEF VAR ldVatPerc  AS DEC  NO-UNDO.
   DEF VAR lcExtInvID AS CHAR NO-UNDO. 
   DEF VAR lcSeqPref  AS CHAR NO-UNDO. 
   DEF VAR ldMinCons  AS DEC  NO-UNDO.
   DEF VAR lcMinItem  AS CHAR NO-UNDO. 
   DEF VAR ldBillPer  AS DEC  NO-UNDO EXTENT 2. 
   DEF VAR ldeTerm    AS DEC  NO-UNDO.
   DEF VAR liTemp1    AS INT  NO-UNDO.
   DEF VAR liTemp2    AS INT  NO-UNDO.
   
   ASSIGN ldtBSDate     = TODAY
          liBSTime      = TIME
          liLastCreated = 0
          ldBillPer[1]  = fMake2Dt(pDate1,0)
          ldBillPer[2]  = fMake2Dt(pDate2,86399).

   HIDE MESSAGE no-pause.
   PAUSE 0.
 
   /* id for this run */
   lcBillRun = icBillRun.
   IF lcBillRun = "" THEN lcBillRun = "BR".

   lcBillRun = lcBillRun + 
               STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + STRING(TIME,"99999").

   IF iiInvType = 0 THEN iiInvType = 1.
   
   INVRUNLOOP:
   FOR EACH ttInvCust no-lock,

      FIRST xcustomer no-lock where
            xcustomer.CustNum    = ttInvCust.CustNr AND
            /* if combined to one customer then get only invoicing customers */
            (IF llInvComb 
             THEN xCustomer.CustNum = xCustomer.InvCust
             ELSE TRUE),

      FIRST InvGroup  no-lock where
            InvGroup.Brand    = xCustomer.Brand AND
            InvGroup.InvGroup = xCustomer.InvGroup

   BREAK BY ttInvCust.InvRec
         BY ttInvCust.CustNr:

      /* end invoice has already been created */  
      IF xCustomer.EndInvType > 0 THEN NEXT.

      liCustQty = liCustQty + 1.
      
      IF liCustQty MOD 10 = 0 THEN
      put screen row 18 col 2 STRING(liCustQty,">>>>>>9") + "/" +
                              TRIM(STRING(iiCustQty,">>>>>>9")).

                              
      
      /* remove here just TO make sure they're gone */
      /* 1: invoice TEMP-TABLE */
      EMPTY TEMP-TABLE ttInv.
      /* 2: invoice ROW TEMP-TABLE */
      EMPTY TEMP-TABLE ttIR.
      /* 3: CCN counters FOR reporting */ 
      EMPTY TEMP-TABLE ttCCN.
      /* 4: CLI counters FOR reporting */ 
      EMPTY TEMP-TABLE ttCLI.
      /* 6: vat amounts */
      EMPTY TEMP-TABLE ttRowVat.
      /* 7: invoice items */
      EMPTY TEMP-TABLE ttInvoiceItem.
      /* fat counters */
      EMPTY TEMP-TABLE ttCallFat.
      /* ServiceCounters */ 
      EMPTY TEMP-TABLE ttServiceCounter.
      /* event customers */
      EMPTY TEMP-TABLE ttEventCust.
      /* cli-based discounts */
      EMPTY TEMP-TABLE ttCLIDisc.
      /* invseq handling */
      EMPTY TEMP-TABLE ttInvSeq.

      gcBrand = xCustomer.Brand.

      /* get a list of subscriptions that should not be billed */
      lcBillDeny = "".
      IF iiInvType = 1 OR iiInvType = 99 THEN 
      FOR EACH MobSub NO-LOCK WHERE
               MobSub.Brand    = gcBrand           AND
               MobSub.InvCust  = xCustomer.CustNum AND
               MobSub.RepCodes = "x":
         lcBillDeny = lcBillDeny + (IF lcBillDeny > "" THEN "," ELSE "") +
                      MobSub.CLI.
      END.         
 
      /* IF xCustomer.CustNum = xCustomer.InvCust THEN 
      */
      DO:

         &IF "{&InitPersistent}" NE "NO" 
         &THEN 
         IF ilDouble THEN run pDoubleCalls IN fhDCHandle (ttInvCust.CustNr).
         
         IF ilRerate THEN Run pRunRerate   IN fhRRHandle (ttInvCust.CustNr, 
                                                          pDate1, 
                                                          pDate2, 
                                                          TRUE).   
         PUT SCREEN ROW 2 col 78 "1".
         
         &ENDIF
         
         IF lcDepoItem = "" AND iiInvType NE 6 AND iiInvType NE 7 THEN
         RUN pCleanInvSeq(INPUT pDate2, 
                          ldBillPer[2],
                          ttInvCust.CustNr).
      END.
      
      
      /* mark this as "running" */
      IF FIRST-OF(ttInvCust.InvRec) AND ttInvCust.InvRec NE 0 THEN DO TRANS:
         FIND FIRST InvRunLog WHERE 
              RECID(InvRunLog) = ttInvCust.InvRec
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL InvRunLog THEN ASSIGN 
            InvRunLog.State = 1
            InvRunLog.BillDurat   = TIME.

         ASSIGN liIRLQty = 0
                ldIRLAmt = 0.
      END.

      /* mark this as "ready":
         DO it here since invoice might NOT be created */
      IF LAST-OF(ttInvCust.InvRec) AND ttInvCust.InvRec NE 0 THEN DO TRANS:
         FIND FIRST InvRunLog WHERE 
              RECID(InvRunLog) = ttInvCust.InvRec
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL InvRunLog THEN ASSIGN 
            InvRunLog.State     = 2
            InvRunLog.InvQty    = liIRLQty
            InvRunLog.InvAmt    = ldIRLAmt
            InvRunLog.BillDurat = TIME - InvRunLog.BillDurat.
         IF InvRunLog.BillDurat < 0 THEN 
            InvRunLog.BillDurat = InvRunLog.BillDurat + 86400.
      END.

      /* tax presentation method from customer,
         "including" is not sensible for vat0 sales */
      IF xCustomer.VatUsage >= 3 AND xCustomer.VatUsage <= 4
      THEN lCustVat = FALSE.
      ELSE lCustVat = xCustomer.VatIncl. 

      lLoop = 0.
      /* collect event customers */
      FOR EACH bEventCust NO-LOCK WHERE
               bEventCust.InvCust = xCustomer.CustNum:
         CREATE ttEventCust.
         ttEventCust.CustNum = bEventCust.CustNum.

         IF bEventCust.CustNum = xCustomer.CustNum THEN lLoop = 1.
      END.
      /* it may be that customer is invoice customer to others, but not 
         to itself */
      IF lLoop = 0 THEN DO:
         CREATE ttEventCust.
         ttEventCust.CustNum = xCustomer.CustNum.
      END.

      ASSIGN
         lSum2       = 0
         lISeq       = 0
         lRejBill    = FALSE
         lcRejReason = ""
         lcRejCLI    = ""
         liVASAcc    = 0
         pNDate      = ?
         liArAccNum  = 0.

      /* ROW FOR starting the invoice */
      fELog(katun,"INVOICE:Started:Customer:" + string(xcustomer.CustNum)).

      FIND FIRST ttInvSeq NO-ERROR.
      
      /* InvSeq exists FOR those who have Called */
      IF AVAIL ttInvSeq AND lcDepoItem = "" AND
         iiInvType NE 6 AND iiInvType NE 7
      THEN DO:

         ASSIGN
            lISeq     = ttInvSeq.InvSeq
            pODate    = pDate1
            pNDate    = pDate2
            lDBLVat   = FALSE.

         &IF "{&InitPersistent}" NE "NO" 
         &THEN 
         /* get volume discount definitions; just empties temp-table for 
            voldiscs if customer hasn't got any */
         RUN pVolDisc IN fhVDHandle (lISeq). 
         &ENDIF
         
         RUN pFixCDR
           (INPUT lISeq,INPUT xcustomer.AccGrp,INPUT pDate1,INPUT pDate2,
            INPUT-OUTPUT pODate).

         RUN pMobCDR
           (INPUT xcustomer.CustNum,
            INPUT xcustomer.AccGrp,INPUT pDate1,INPUT pDate2,
            INPUT-OUTPUT pODate).

         /* VAS tickets for VAS operators  */
         RUN pVASCDR
           (INPUT lISeq, INPUT pDate1,INPUT pDate2,
            INPUT xcustomer.CustNum,INPUT-OUTPUT pODate).

         /* duplicated VAT handling was found */
         IF NOT llAllowDbl AND lDBLVat THEN DO:
            fELog(katun,"INVOICE:DBLVAT:Customer:" + string(xcustomer.CustNum)).
            fErrorLog(xCustomer.CustNum,
                      "",
                      "DBLVAT").
            NEXT InvRunLoop.
         END.

         /* vat handling, run this already here so that counters (pCounterVat) 
            will get correct results */
         fInvRowVAT(xCustomer.VATUsage,
                    xCustomer.Region,
                    xCustomer.Category). 
     
         /* unify vat handling for counters */
         RUN pCounterVAT. 
 
      END. /* AVAIL InvSeq */

      /* if calls found -> create fee for call spec */
      IF CAN-FIND(FIRST ttIR) THEN 
      DO liCnt = 1 TO LENGTH(xCustomer.RepCodes):

         lcRepCode = SUBSTRING(xCustomer.RepCodes,liCnt,1).
         
         RUN Mc/creasfee (xCustomer.CustNum,
                       0,
                       pDate2,
                       "InvSpec",
                       lcRepCode,
                       1,
                       "¤" + lcBillRun,
                       FALSE,
                       OUTPUT lcInfo).
      END.
            
      /* fee also for cli level */
      FOR EACH ttCLI 
      BREAK BY ttCLI.CLI:
         
         IF FIRST-OF(ttCLI.CLI) THEN DO:

            lcMobRep = "".
            
             ASSIGN ldFromPer = fMake2Dt(IF ttCLI.FromDate NE ?
                                         THEN ttCLi.FromDate
                                         ELSE poDate,86399)
                    ldToPer   = fMake2Dt(IF ttCLI.ToDate NE ?
                                         THEN ttCLI.ToDate
                                         ELSE pDate2,0) /* use 0 -> get owner
                                                        of last full day */.
             
             /* all calls on one day */       
             IF TRUNCATE(ldFromPer,0) = TRUNCATE(ldToPer,0)
             THEN ldToPer = ldToPer + 0.86399.

             FOR FIRST MSOwner NO-LOCK WHERE
                       MSOwner.Brand = gcBrand   AND
                       MsOwner.CLI   = ttCLI.CLI AND
                       MsOwner.TsBeg <= ldToPer  AND
                       MsOwner.TsEnd >= ldFromPer:

               lcMobRep = fCallSpecDuring(MsOwner.MsSeq,
                                          idtInvDate).
               
               IF lcMobRep > "" THEN 
               DO liCnt = 1 TO LENGTH(lcMobRep):
         
                  lcRepCode = SUBSTRING(lcMobRep,liCnt,1).
         
                  RUN Mc/creasfee (MsOwner.CustNum,
                                MsOwner.MSSeq,
                                pDate2,
                                "CLISpec",
                                lcRepCode,
                                1,
                                "¤" + lcBillRun,
                                FALSE,
                                OUTPUT lcInfo).
               END.                 
         
            END.
         
      
         END.
      
      END.

      IF iiInvType < 3 OR iiInvType EQ 99 THEN
      FOR EACH ttEventCust WHERE
               (IF llInvComb 
                THEN TRUE
                ELSE ttEventCust.CustNum = xCustomer.CustNum),
          EACH FixedFee no-lock where
               FixedFee.Brand   = gcBrand             AND 
               FixedFee.CustNum = ttEventCust.CustNum AND
               FixedFee.InUse   = TRUE:

         /* Create more items, if needed! */
         IF FixedFee.EndPeriod = 999999 THEN DO:

            FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
            IF NOT AVAIL FFItem THEN NEXT.

            /* At least 1 year unbilled fixed fee items */
            IF FFItem.BillPeriod <= YEAR(Today) * 100 + MONTH(Today) + 100 
            THEN DO:
               fMakeContractMore(INPUT Fixedfee.FFNum, 
                                 INPUT FFItem.Concerns[2]).
               
            END.
         END.
               
         /* if deposit item is set then bill only that */
         IF lcDepoItem NE "" AND
            LOOKUP(FixedFee.BillCode,lcDepoItem) = 0 THEN NEXT.

         /* specific order */
         IF liOrderID > 0 THEN DO:
            IF FixedFee.HostTable NE "Order" OR
               FixedFee.KeyValue NE STRING(liOrderID)
            THEN NEXT. 
         END.
               
         /* specific clis */
         IF lcMSSeqLst > "" AND 
            (FixedFee.HostTable NE "MobSub" OR
             LOOKUP(FixedFee.KeyValue,lcMSSeqLst) = 0)
         THEN NEXT.

         ASSIGN 
            lcCLI   = ""
            liMsSeq = 0.
         IF FixedFee.HostTable = "MobSub" THEN DO:
            FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                      MSOwner.MsSeq   = INTEGER(FixedFee.KeyValue) AND
                      MsOwner.InvCust = FixedFee.CustNum:
               ASSIGN 
                  lcCLI   = MsOwner.CLI
                  liMsSeq = MsOwner.MsSeq.
            END.   
         END.
 
         /* billing denied */
         IF lcCLI > "" AND LOOKUP(lcCLI,lcBillDeny) > 0 THEN NEXT.
  
         FOR EACH FFItem of FixedFee exclusive-lock where 
                  FFItem.BillPeriod <= ciperiod      AND
                  FFItem.Billed     = FALSE:

            /* concerns must be checked for end invoices, future items
               cannot be billed */
            IF liEndInvType > 0 THEN DO:
               IF FFItem.Concerns[1] > YEAR(pDate2) * 10000 + 
                                       MONTH(pDate2) * 100 +
                                       DAY(pDate2) 
               THEN NEXT.                         
            END.

            /* FOR calculating the time Period FOR invoice ROW */
            lcper  = IF  FFItem.Concerns[1] > 0 THEN                  
                         FFItem.Concerns[1] ELSE  
                         FFItem.BillPeriod.
            lcper2 = IF  FFItem.Concerns[2] > 0 THEN
                         FFItem.Concerns[2] ELSE
                         lcPer.
          
            /* check that periods are valid */ 
            ldtFrom = fInt2Date(lcPer,1).
            ldtTo   = fInt2Date(lcPer2,2).
            IF ldtFrom = ? OR ldtTo = ? THEN DO:
               ASSIGN lRejBill    = TRUE
                      lcRejReason = "FFeePeriod"
                      lcRejCLI    = lcCLI.
               NEXT.
            END. 
          
            FIND FIRST ttIR WHERE
                       ttIR.BillCode = FFItem.BillCode  AND
                       ttIR.CLI      = lcCLI            AND 
                       ttIR.bVAT     = FixedFee.VATIncl AND
                       ttIR.RowType  = 3 NO-ERROR.

            IF NOT AVAIL ttIR THEN DO:

               CREATE ttIR.
               ASSIGN
                ttIR.FromDate  = ldtFrom
                ttIR.ToDate    = ldtTo
                ttIR.BillCode  = FFItem.BillCode
                ttIR.CLI       = lcCLI
                ttIR.MsSeq     = liMsSeq
                ttIR.FFRow     = TRUE
                ttIR.RowType   = 3
                ttIR.bVat      = FixedFee.VatIncl.
                /*
                ttIR.memo[1] = FFItem.memo[1]
                ttIR.memo[2] = FFItem.memo[2]
                ttIR.memo[3] = FFItem.memo[3]
                ttIR.memo[4] = FFItem.memo[4]
                ttIR.memo[5] = FFItem.memo[5] 
                ttIR.FFItemNum = FFItem.FFItemNum.
                */
            END.

            ASSIGN
               ttIR.FromDate = MIN(ttir.FromDate,ldtFrom)
               ttIR.ToDate   = MAX(ttir.ToDate,ldtTo)
               ttIR.Qty      = ttIR.Qty + 1
               ttIR.dgross   = ttIR.dgross   + FFItem.Amt
               ttIR.dNet     = ttIR.dNet     + FFItem.Amt
               FFItem.Billed = TRUE.

            fInvoiceItem(1,
                         ttIR.MsSeq,
                         RECID(FFItem)).
           
            CallQty = ?.
             
            /* FATime */                  
            FOR EACH bttEventCust,
                EACH FATime NO-LOCK USE-INDEX FATType WHERE
                     Fatime.Brand      = gcBrand               AND 
                     Fatime.CustNum    = bttEventCust.CustNum  AND 
                    (FATime.InvNum    = 0 or Fatime.InvNum = -1 * liSeq) AND
                     FATime.FatType    = 1 /* FIXED FEES */    AND
                     FATime.CLI        = lcCLI                 AND     
                    (IF Fatime.Transfer
                     THEN FATime.Period <= ciperiod 
                     ELSE FATime.Period = FFItem.BillPeriod),
               FIRST FatGroup NO-LOCK WHERE
                     FatGroup.Brand = gcBrand AND 
                     FatGroup.FTGrp = FATime.FTGrp,
               EACH  FatGMember of FatGroup NO-LOCK WHERE
                     FatGMember.MemberType = 1             AND
                     FatGMember.FTGMember  = FixedFee.BillCode
            BY FATGroup.Priority 
            BY FATime.Period:

               /* is target / customer level correct */
               IF FatGroup.FatTarget > "0" THEN DO:
                  IF lcCLI > "" THEN NEXT.

                  /* invoice/agreement */
                  IF (FatGroup.FatTarget = "1" OR FatGroup.FatTarget = "2")
                         AND 
                     FATime.CustNum NE xCustomer.CustNum THEN NEXT.
               END.
              
               /* subscription level */
               ELSE IF lcCLI = "" THEN NEXT.
            
               IF CallQty = ? THEN DO:
                  ASSIGN CallAmt = FFItem.Amt
                         CallQty = 1.
                   
                  /* unify vat handling with fatime */
                  IF FixedFee.VatIncl NE Fatime.VatIncl AND
                     xCustomer.VatUsage < 3 THEN 
                  FOR FIRST BillItem NO-LOCK WHERE
                            BillItem.Brand    = gcBrand AND
                            BillItem.BillCode = FFItem.BillCode:
                          
                      ldVatPerc = fRegionTaxPerc(xCustomer.Region,
                                                 BillItem.TaxClass).
                                                
                      /* always use domestic vat, % is the same also in EU */
                      IF Fatime.VatIncl 
                      THEN CallAmt = CallAmt * (1 + ldVatPerc / 100).
                      ELSE CallAmt = CallAmt / (1 + ldVatPerc / 100).
                  END.
               END.

               fMakeFATimeRow(INPUT-OUTPUT CallAmt,
                              INPUT-OUTPUT CallQty,
                              liMsSeq). 
             
               fTransFatime(liMsSeq).
             
               IF CallAmt <= 0 THEN LEAVE. 
             
            END. /* FATime */

         END. /* ffitem */
         
      END. /* FixedFee */

      IF iiInvType = 6 OR iiInvType = 7 THEN 
      FOR EACH SingleFee EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
               SingleFee.Brand     = gcBrand AND
               SingleFee.HostTable = "Order" AND
               SingleFee.KeyValue  = STRING(liOrderId) AND
               SingleFee.Billed    = FALSE   AND
               SingleFee.Active    = TRUE,
         FIRST Order NO-LOCK WHERE
               Order.Brand   = gcBrand AND
               Order.OrderID = liOrderId:
               
         /* billing denied */
         IF LOOKUP(Order.CLI,lcBillDeny) > 0 THEN NEXT.
 
         /* combine by product code */
         FIND FIRST ttIR WHERE
                    ttIR.BillCode = SingleFee.BillCode AND
                    ttIR.CLI      = Order.CLI          AND
                    ttIR.bVat     = SingleFee.VatIncl  AND
                    ttIR.RowType  = 4 NO-ERROR.

         IF NOT AVAIL ttIR THEN DO:
            CREATE ttIR.
            ASSIGN                                      
            ttIR.FromDate  = ? 
            ttIR.ToDate    = ? 
            ttIR.BillCode  = SingleFee.BillCode
            ttIR.CLI       = Order.CLI
            ttIR.MsSeq     = Order.MsSeq
            ttIR.FFRow     = FALSE
            ttIR.RowType   = 4
            ttIR.bVat      = SingleFee.VatIncl.
         END.

         ASSIGN
            ttIR.Qty         = ttIR.Qty    + 1
            ttIR.dNet        = ttIR.dNet   + SingleFee.Amt
            ttIR.dgross      = ttIR.dGross + SingleFee.Amt
            SingleFee.Billed = TRUE.

         fInvoiceItem(2,
                      ttIR.MsSeq,
                      RECID(SingleFee)).

      END.  /* invtype = 6/7 */

      /* normal, not a cash invoice */
      ELSE 
      FOR EACH ttEventCust WHERE
               (IF llInvComb 
                THEN TRUE
                ELSE ttEventCust.CustNum = xCustomer.CustNum),
          EACH SingleFee EXCLUSIVE-LOCK WHERE
               SingleFee.CustNum     = ttEventCust.CustNum AND
               SingleFee.BillPeriod <= ciperiod            AND
               SingleFee.Brand       = gcBrand             AND 
               SingleFee.Billed      = FALSE               AND
               SingleFee.Active      = TRUE:

         /* if deposit item is set then bill only that */
         IF lcDepoItem > "" AND
            LOOKUP(SingleFee.BillCode,lcDepoItem) = 0 THEN NEXT.

         /* specific clis */
         IF lcMSSeqLst > "" AND 
            (SingleFee.HostTable NE "MobSub" OR
             LOOKUP(SingleFee.KeyValue,lcMSSeqLst) = 0)
         THEN NEXT.
         
         /* specific order */
         IF liOrderID > 0 THEN DO:
            IF LOOKUP(SingleFee.HostTable,"Order,MsRequest") = 0 OR
               SingleFee.KeyValue NE STRING(liOrderID)
            THEN NEXT.
         END.  
         ELSE IF SingleFee.HostTable = "Order" AND 
                 SingleFee.CalcObj = "CASHFEE" THEN NEXT.


         /* FOR calculating the Date FORMAT time Period FOR invoice ROW */
         ASSIGN
            lcper   = INT(Substring(STRING(SingleFee.BillPeriod),1,6))
            lcCLI   = ""
            liMsSeq = 0.

         IF SingleFee.HostTable = "MobSub" THEN DO:
            FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                      MSOwner.MsSeq   = INTEGER(SingleFee.KeyValue) AND
                      MSOwner.InvCust = SingleFee.CustNum:
               ASSIGN 
                  lcCLI   = MsOwner.CLI
                  liMsSeq = MsOwner.MsSeq.
            END.    
         END.
         ELSE IF SingleFee.HostTable = "Order" THEN 
         FOR FIRST Order NO-LOCK WHERE
                   Order.Brand   = gcBrand AND
                   Order.OrderID = INTEGER(SingleFee.KeyValue):
            ASSIGN 
               lcCLI   = Order.CLI
               liMsSeq = Order.MsSeq.
         END.

         /* billing denied */
         IF lcCLI > "" AND LOOKUP(lcCLI,lcBillDeny) > 0 THEN NEXT.
 
         /* combine by product code */
         FIND FIRST ttIR WHERE
                    ttIR.BillCode = SingleFee.BillCode AND
                    ttIR.CLI      = lcCLI              AND
                    ttIR.bVat     = SingleFee.VatIncl  AND
                    ttIR.RowType  = 4 NO-ERROR.

         IF NOT AVAIL ttIR THEN DO:

            CREATE ttIR.
            ASSIGN                                      
            ttIR.FromDate  = ? /* fInt2date(SingleFee.Concerns[1],1) */
            ttIR.ToDate    = ? /*(IF SingleFee.Concerns[2] = 0 THEN 
                                  fInt2date(SingleFee.Concerns[1],2)
                              ELSE fInt2date(SingleFee.Concerns[2],2)) */
            ttIR.FromDate  = pDate1 WHEN SingleFee.BillCode = "TERMPERIOD"
            ttIR.ToDate    = pDate2 WHEN SingleFee.BillCode = "TERMPERIOD"
            ttIR.BillCode  = SingleFee.BillCode
            ttIR.CLI       = lcCLI
            ttIR.MsSeq     = liMsSeq
            ttIR.FFRow     = FALSE
            ttIR.RowType   = 4
            ttIR.bVat      = SingleFee.VatIncl.
            /*
            ttIR.memo[1]   = SingleFee.memo[1]
            ttIR.memo[2]   = SingleFee.memo[2]
            ttIR.memo[3]   = SingleFee.memo[3]
            ttIR.memo[4]   = SingleFee.memo[4]
            ttIR.memo[5]   = SingleFee.memo[5]
            ttIR.FFItemNum = SingleFee.FMItemId
            */
         END.

         ASSIGN
            ttIR.Qty         = ttIR.Qty    + 1
            ttIR.dNet        = ttIR.dNet   + SingleFee.Amt
            ttIR.dgross      = ttIR.dGross + SingleFee.Amt
            SingleFee.Billed = TRUE.

         fInvoiceItem(2,
                      ttIR.MsSeq,
                      RECID(SingleFee)).

         CallAmt = ?.
          
         /* FATime */                  
         IF iiInvType NE 6 AND iiInvType NE 7 THEN 
         FOR EACH bttEventCust,
             EACH FATime NO-LOCK USE-INDEX FATType WHERE
                  Fatime.Brand      = gcBrand               AND 
                  Fatime.CustNum    = bttEventCust.CustNum  AND 
                  (FATime.InvNum    = 0 or Fatime.InvNum = -1 * liSeq) AND
                  FATime.FatType    = 3 /* single fees */   AND
                  FATime.CLI        = lcCLI                 AND     
                  (IF Fatime.Transfer
                   THEN FATime.Period <= ciperiod 
                   ELSE FATime.Period = SingleFee.BillPeriod),
             FIRST FatGroup NO-LOCK WHERE
                   FatGroup.Brand = gcBrand AND 
                   FatGroup.FTGrp = FATime.FTGrp,
             EACH  FatGMember of FatGroup NO-LOCK WHERE
                   FatGMember.MemberType = 1             AND
                   FatGMember.FTGMember  = SingleFee.BillCode
         BY FATGroup.Priority 
         BY FATime.Period.

             /* is target / customer level correct */
             IF FatGroup.FatTarget > "0" THEN DO:
                IF lcCLI > "" THEN NEXT.

                /* invoice/agreement */
                IF (FatGroup.FatTarget = "1" OR FatGroup.FatTarget = "2") AND 
                   FATime.CustNum NE xCustomer.CustNum THEN NEXT.
             END.
             /* subscription level */
             ELSE IF lcCLI = "" THEN NEXT.
            
             IF CallQty = ? THEN DO:
                ASSIGN CallAmt = SingleFee.Amt
                       CallQty = 1.
                   
                /* unify vat handling with fatime */
                IF SingleFee.VatIncl NE Fatime.VatIncl AND
                   xCustomer.VatUsage < 3 THEN 
                FOR FIRST BillItem NO-LOCK WHERE
                          BillItem.Brand    = gcBrand AND
                          BillItem.BillCode = SingleFee.BillCode:
                          
                    ldVatPerc = fRegionTaxPerc(xCustomer.Region,
                                               BillItem.TaxClass).
                           
                    /* always use domestic vat, % is the same also in EU */
                    IF Fatime.VatIncl 
                    THEN CallAmt = CallAmt * (1 + ldVatPerc / 100).
                    ELSE CallAmt = CallAmt / (1 + ldVatPerc / 100).
                END.
             END.

             fMakeFATimeRow(INPUT-OUTPUT CallAmt,
                            INPUT-OUTPUT CallQty,
                            liMsSeq). 
             
             fTransFatime(liMsSeq).
             
             IF CallAmt <= 0 THEN LEAVE. 
             
         END. /* FATime */

      END. /* SingleFee */

      /* check vat handling for rows created so far */
      fInvRowVAT (xCustomer.VATUsage,
                  xCustomer.Region,  
                  xCustomer.Category). 
 
      /* minimum consumption */  
      IF LOOKUP(STRING(iiInvType),"1,99") > 0 THEN DO:

         /* if nothing to bill was found then create fake rows to each 
            invoice customer's cli which were active on the billing period,
            in order to get the minimum consumption */
         FOR EACH MsOwner NO-LOCK USE-INDEX InvCust WHERE 
                  MsOwner.InvCust  = xCustomer.CustNum AND
                  MsOwner.TsBeg   <= ldBillPer[2]      AND
                  MsOwner.TsEnd   >= ldBillPer[1],
            FIRST CLIType NO-LOCK WHERE
                  CLIType.Brand   = gcBrand AND
                  CLIType.CLIType = MsOwner.CLIType AND
                  CLIType.MinimAmt > 0:

            /* specific clis */
            IF lcMSSeqLst > "" AND 
               LOOKUP(STRING(MsOwner.MsSeq),lcMSSeqLst) = 0
            THEN NEXT.

            /* billing denied */
            IF LOOKUP(MsOwner.CLI,lcBillDeny) > 0 THEN NEXT.
 
            IF CAN-FIND(FIRST ttIR WHERE ttIR.MsSeq = MsOwner.MsSeq) 
            THEN NEXT.
                     
            CREATE ttIR.
            ASSIGN ttIR.RowType = 99
                   ttIR.CLI     = MsOwner.CLI
                   ttIR.MsSeq   = MsOwner.MsSeq
                   ttIR.dNet    = 0.
         END.

         ASSIGN lLoop      = 0
                liArAccNum = ReceivAcc.
 
         /* receivable account and minimum consumption (account for 
            cash invoices is updated in the end of the routine) */    
         FOR EACH xxIR WHERE
                  xxIR.RowType NE 5 AND
                  xxIR.RowType NE 7 
         BREAK BY xxIR.MsSeq:
            
            IF FIRST-OF(xxIR.MsSeq) THEN DO:
               ASSIGN lbSum   = 0
                      ldtFrom = ?
                      ldtTo   = ?.
            END.
                
            ldBasis = xxIR.dNet.
            /* minimum limit is vat excluded */
            IF lCustVat THEN 
               ldBasis = ldBasis / (1 + xxIR.VatPerc / 100). 
            lbSum = lbSum + ldBasis.   

            IF ldtFrom = ? THEN ASSIGN 
               ldtFrom = xxIR.FromDate
               ldtTo   = xxIR.ToDate.
                  
            ELSE IF xxIR.FromDate NE ? THEN ASSIGN 
               ldtFrom = MIN(ldtFrom,xxIR.FromDate)
               ldtTo   = MAX(ldtTo,xxIR.ToDate).
                
            IF LAST-OF(xxIR.MsSeq) AND xxIR.MsSeq > 0 THEN DO:

               IF ldtFrom = ? THEN ASSIGN 
                  ldtFrom = IF poDate NE ?
                            THEN poDate
                            ELSE pDate1
                  ldtTo   = pDate2.
                    
               ASSIGN     
                  ldFromPer = fMake2Dt(ldtFrom,0)
                  ldToPer   = fMake2Dt(ldtTo,86399)
                  ldeTerm   = 99999999.99999
                  ldMinCons = 0
                  lcMinItem = "".

               FOR FIRST MSOwner NO-LOCK USE-INDEX InvCust WHERE
                         MSOwner.InvCust = xCustomer.CustNum AND
                         MsOwner.MsSeq   = xxIR.MsSeq        AND
                         MsOwner.TsBeg  <= ldToPer           AND
                         MsOwner.TsEnd  >= ldFromPer,
                   FIRST CLIType NO-LOCK WHERE
                         CLIType.Brand   = gcBrand AND
                         CLIType.CLIType = MsOwner.CLIType:

                  IF CLIType.ArAccNum > 0 THEN 
                     liArAccNum = CLIType.ArAccNum.
               
                  /* this period already handled */
                  IF CAN-FIND(FIRST ActionLog WHERE
                     ActionLog.Brand        = gcBrand       AND
                     ActionLog.TableName    = "MobSub"      AND
                     ActionLog.KeyValue     = STRING(MsOwner.MsSeq) AND
                     ActionLog.ActionID     = "MINCONS"     AND
                     ActionLog.ActionPeriod = YEAR(pDate2) * 100 + 
                                              MONTH(pDate2) AND
                     ActionLog.ActionStatus = 0) 
                  THEN NEXT. 
                 
                  ldeTerm = MsOwner.TsEnd.

                  /* minimum consumption only for normal invoices */       
                  IF iiInvType <= 1 OR iiInvType EQ 99 THEN DO:
   
                     /* not if subscription has been activated on the
                        billing period */
                     IF MsOwner.TsBeg  > ldBillPer[1] AND 
                        MsOwner.TsBeg <= ldBillPer[2] THEN DO:
                         
                        IF NOT CAN-FIND
                           (FIRST bOwner WHERE
                                  bOwner.MsSeq   = MsOwner.MsSeq   AND
                                  bOwner.InvCust = MsOwner.InvCust AND
                                  bOwner.TsBeg < ldBillPer[1]) THEN NEXT.
                     END. 
                        
                     ASSIGN 
                        ldMinCons = CLIType.MinimAmt
                        lcMinItem = CLIType.BillCode.
                  END.   
               END.
               
               lbSum = ROUND(lbSum,2). 
                  
               IF lbSum >= 0 AND lbSum < ldMinCons AND lcMinItem > "" THEN DO:

                  /* Minimum Consumption for Leaving Customer */
                  liTemp2 = INT(SUBSTR(STRING(ldToPer),7,2)).
                  IF ldeTerm < ldToPer THEN ASSIGN
                     liTemp1 = INT(SUBSTR(STRING(ldeTerm),7,2)) - 1.
                  ELSE
                     liTemp1 = liTemp2.

                  IF liTemp1 > 0 THEN   
                     ldMinCons = liTemp1 / liTemp2 * ldMinCons.
                  ELSE
                     ldMinCons = 0.
                     
                  IF lbSum < ldMinCons THEN DO:

                     CREATE ttIR.
                     ASSIGN                                      
                        ttIR.FromDate  = pDate1
                        ttIR.ToDate    = pDate2
                        ttIR.BillCode  = lcMinItem
                        ttIR.CLI       = xxIR.CLI
                        ttIR.MsSeq     = xxIR.MsSeq
                        ttIR.FFRow     = FALSE
                        ttIR.RowType   = 5
                        ttIR.bVat      = FALSE
                        ttIR.Qty       = 1
                        ttIR.dNet      = ldMinCons - lbSum
                        ttIR.dgross    = ttIR.dNet
                        lLoop          = lLoop + 1.
               
                  END.

               END.

            END.
               
            /* dummy rows that were created for min.consumption */
            IF xxIR.RowType = 99 THEN DELETE xxIR.   
         END.

      END.

      /*  FREE AIR TIME (FATime) FOR CALLS */ 
      FOR EACH ttCLI
      BREAK BY ttCLI.CLI:

         IF FIRST-OF(ttCLI.CLI) THEN DO:
            /* is something defined for this cli */
            IF CAN-FIND(FIRST FATime WHERE
                              FATime.Brand = gcBrand AND 
                              FATime.CLI   = ttcli.CLI)              
            THEN ASSIGN ldtFrom = ttCLI.FromDate
                        ldtTo   = ttCLI.ToDate.
            ELSE ldtFrom = ?.
         END.   
      
         IF ldtFrom = ? THEN NEXT.
         
         ASSIGN ldtFrom = MIN(ldtFrom,ttCLI.FromDate)
                ldtTo   = MAX(ldtTo,ttCLI.ToDate).

         IF LAST-OF(ttCLI.CLI) THEN DO:
            
            ASSIGN lcPer  = YEAR(ldtFrom) * 100 + MONTH(ldtFrom)
                   lcPer2 = YEAR(ldtTo) * 100 + MONTH(ldtTo).
                   
            FOR EACH FATime NO-LOCK WHERE
                     Fatime.Brand    = gcBrand          AND 
                     FATime.cli      = ttcli.CLI        AND
                     (FATime.InvNum  = 0 OR Fatime.InvNum = -1 * liSeq) AND
                     FATime.FatType  = 0  /* calls */   AND
                     FATime.Period  <= lcPer2           AND
                     (IF Fatime.Transfer = FALSE
                      THEN Fatime.Period >= lcPer
                      ELSE TRUE),
               FIRST FatGroup OF Fatime NO-LOCK
            BREAK BY FATGroup.Priority 
                  BY FATime.Period DESC:

               /* if fatime includes vat -> also base amount should */
               IF FIRST(Fatime.Period) THEN DO:
                  llFatVat = Fatime.VatIncl.
                  IF llFatVat THEN 
                  FOR EACH ttCallFat WHERE ttCallFat.CLI = ttCLI.CLI:
                     ttCallFat.Amt = ttCallFat.AmtWVat.
                  END.
               END.   
               /* all fatimes for one cli must use the same vat method */ 
               IF Fatime.VatIncl NE llFatVat THEN NEXT.
               
               FOR EACH FatGMember OF FatGroup NO-LOCK
               BY FatGMember.MemberType: /* ccns first */
               
                  IF FatGMember.MemberType = 0 THEN
                  FOR EACH ttCallFat WHERE
                           ttCallFat.CLI = ttCLI.CLI AND
                           ttCallFat.CCN = INTEGER(FatGMember.FTGMember):
      
                     /* is fatime valid for these events */
                     IF Fatime.Period > ttCallFat.Period OR
                        (Fatime.Transfer = FALSE AND
                         Fatime.Period < ttCallFat.Period)
                     THEN NEXT. 
                    
                     /* function reduces ttcallfat.amt if fatime used */  
                     fMakeFATimeRow(INPUT-OUTPUT ttCallFat.Amt,
                                    INPUT-OUTPUT ttCallFat.Qty,
                                    ttCLI.MsSeq). 
                  END.
                     
                  ELSE IF FatGMember.MemberType = 1 THEN
                  FOR EACH ttCallFat WHERE
                           ttCallFat.CLI      = ttCLI.CLI AND
                           ttCallFat.BillCode = FatGMember.FTGMember:

                     /* is fatime valid for these events */
                     IF Fatime.Period > ttCallFat.Period OR
                        (Fatime.Transfer = FALSE AND
                         Fatime.Period < ttCallFat.Period)
                     THEN NEXT. 
  
                     /* function reduces ttcallfat.amt if fatime used */  
                     fMakeFATimeRow(INPUT-OUTPUT ttCallFat.Amt,
                                    INPUT-OUTPUT ttCallFat.Qty,
                                    ttCLI.MsSeq). 
                  END.         
               END.
              
               fTransFatime(ttCLI.MsSeq). 
               
            END.
             
         END.
      END.   /* FATime */

 
      /* check vat handling for rows created so far
         -> possible fatime ("all") gets correct sum */
      fInvRowVAT (xCustomer.VATUsage,
                  xCustomer.Region,  
                  xCustomer.Category). 
      
      ldtFrom = pDate1.
      
       /* FATime for all events (after all rows including minimumfee are done);
          collect cli based sums */
      FOR EACH ttIR:
         
         FIND FIRST ttCLIDisc WHERE 
                    ttCLIDisc.CLI   = ttIR.CLI AND
                    ttCLIDisc.MsSeq = ttIR.MsSeq NO-ERROR.
         IF NOT AVAILABLE ttCLIDisc THEN DO:
            CREATE ttCLIDisc.
            ASSIGN ttCLIDisc.CLI      = ttIR.CLI
                   ttCLIDisc.MsSeq    = ttIR.MsSeq
                   ttCLIDisc.FromDate = pDate1.
         END.

         ldBasis = ROUND(ttIR.dNet,2).

         IF ttIR.bVat THEN ASSIGN 
            lbSum   = ldBasis 
            ldBasis = ldBasis / (1 + ttIR.VatPerc / 100).
         ELSE ASSIGN 
            lbSum   = ldBasis * (1 + ttIR.VatPerc / 100).
          
         ASSIGN ttCLIDisc.Amt     = ttCLIDisc.Amt + ldBasis
                ttCLIDisc.AmtWVat = ttCLIDisc.AmtWVat + lbSum.

         IF ttIR.FromDate NE ? THEN ASSIGN 
            ttCLIDisc.FromDate = MIN(ttCLIDisc.FromDate,ttIR.FromDate)
            ldtFrom            = MIN(ldtFrom,ttIR.FromDate).
      END.

      IF pNDate NE ? 
      THEN liCnt = YEAR(pNdate) * 100 + MONTH(pNdate).
      ELSE liCnt = 0.
      liCnt = MAX(liCnt,ciperiod).

      /* first handle cli based fatimes */   
      FOR EACH ttCLIDisc WHERE
               ttCLIDisc.CLI > "":
               
         ASSIGN CallAmt     = ROUND(ttCLIDisc.Amt,2)
                CallAmtWVat = ROUND(ttCLIDisc.AmtWVat,2)
                CallQty     = ?
                lcPer       = YEAR(ttCLIDisc.FromDate) * 100 +
                              MONTH(ttCLIDisc.FromDate).
                
         FOR EACH FATime NO-LOCK WHERE
                  FATime.CLI      = ttCLIDisc.CLI AND 
                  FATime.FatType  = 2             AND 
                  FATime.Period  <= liCnt         AND
                  (FATime.InvNum  = 0 OR Fatime.InvNum = -1 * liSeq) AND
                  (IF Fatime.Transfer = FALSE
                   THEN Fatime.Period >= lcPer
                   ELSE TRUE),
         FIRST FatGroup NO-LOCK WHERE
               FatGroup.FTGrp = FATime.FTGrp
         BY FATGroup.Priority 
         BY FATime.Period:

            /* is target correct */
            IF FatGroup.FatTarget > "0" THEN NEXT.
         
            IF CallQty = ? THEN DO:  
               /* vat included in fat amount */
               IF Fatime.VatIncl THEN ASSIGN 
                  CallAmt     = CallAmtWVat
                  CallAmtWVat = ?.
           
               CallQty = 0.
            END.

            fMakeFATimeRow(INPUT-OUTPUT CallAmt,
                           INPUT-OUTPUT CallQty,
                           ttCLIDisc.MsSeq).      
          
            fTransFatime(ttCLIDisc.MsSeq).
 
            IF CallAmt <= 0 THEN LEAVE. 
         END. 

         /* if amount with vat was used then update amount without vat,
            and vice versa */
         IF CallAmtWVat = ? THEN ASSIGN 
            CallAmtWVat = CallAmt
            CallAmt     = ROUND(ttCLIDisc.Amt *
                                CallAmtWVat / ttCLIDisc.AmtWVat,3).
         ELSE ASSIGN 
            CallAmtWVat = ROUND(ttCLIDisc.AmtWVat * 
                                CallAmt / ttCLIDisc.Amt,3).
                                 
         /* update used amounts */
         ASSIGN ttCLIDisc.Amt     = CallAmt
                ttCLIDisc.AmtWVat = CallAmtWVat.
       END.         

      /* what is left */
      ASSIGN CallAmt     = 0
             CallAmtWVat = 0
             CallQty     = ?.
      FOR EACH ttCLIDisc:
         ASSIGN CallAmt     = CallAmt + ttCLIDisc.Amt
                CallAmtWVat = CallAmtWVat + ttCLIDisc.AmtWVat.
         /* should ldtfrom be updated here */       
      END.

      ASSIGN lcPer       = YEAR(ldtFrom) * 100 + MONTH(ldtFrom)
             CallAmt     = ROUND(CallAmt,2)
             CallAmtWVat = ROUND(CallAmtWVat,2).
             
      IF CallAmt > 0 THEN 
      /* then general, customer based */
      FOR EACH ttEventCust WHERE 
              (IF llInvComb 
               THEN TRUE
               ELSE ttEventCust.CustNum = xCustomer.CustNum),
          EACH FATime NO-LOCK WHERE
               FATime.CustNum  = ttEventCust.CustNum   AND 
               FATime.FatType  = 2                     AND 
               FATime.Period  <= liCnt                 AND
               (FATime.InvNum  = 0 OR Fatime.InvNum = -1 * liSeq) AND
               (IF Fatime.Transfer = FALSE
                THEN Fatime.Period >= lcPer
                ELSE TRUE),
         FIRST FatGroup NO-LOCK WHERE
               FatGroup.FTGrp = FATime.FTGrp
      BY FATGroup.Priority 
      BY FATime.Period:

         /* is target / customer level correct */
         IF FatGroup.FatTarget > "0" THEN DO:
            IF FATime.CLI > "" THEN NEXT.
               
            /* invoice/agreement */
            IF (FatGroup.FatTarget = "1" OR FatGroup.FatTarget = "2") AND 
               FATime.CustNum NE xCustomer.CustNum THEN NEXT.
         END.
         /* subscription level */
         ELSE NEXT.
         
         IF CallQty = ? THEN DO:   
            /* vat included in fat amount */
            IF Fatime.VatIncl THEN CallAmt = CallAmtWVat.
            CallQty = 0.
         END.

         fMakeFATimeRow(INPUT-OUTPUT CallAmt,
                        INPUT-OUTPUT CallQty,
                        0).      

         fTransFatime(0).
 
         IF CallAmt <= 0 THEN LEAVE. 
      END.         


      /* TRANSACTION TO reduce lockings */
      DO TRANS:

         /* invoice is created only IF invoice rows are made */
         IF CAN-FIND(FIRST ttIR) THEN DO:

            FIND Customer WHERE RECID(Customer) = RECID(xCustomer) NO-LOCK.

            /* vat handling for rows (fat created after last run) */
            fInvRowVAT (xCustomer.VATUsage,
                        xCustomer.Region,    
                        xCustomer.Category). 
            
            lLoop = 0.
            
            /* calculate overtime Interest */
            IF llBillInt AND lcDepoItem = "" AND lcIntItem > "" AND
               iiInvType NE 6 AND iiInvType NE 7 THEN 
            FOR EACH CustIntEvent no-lock USE-INDEX CustNum where
                     CustIntEvent.Brand        = gcBrand           AND 
                     CustIntEvent.CustNum      = xcustomer.CustNum AND
                     CustIntEvent.BilledInvNum = 0,
               FIRST Invoice NO-LOCK WHERE
                     Invoice.InvNum  = CustIntEvent.InvNum:

               /* specific clis */
               IF lcMSSeqLst > "" AND
                  LOOKUP(STRING(Invoice.MsSeq),lcMSSeqLst) = 0
               THEN NEXT.

               /* billing denied */
               IF Invoice.CLI > "" AND LOOKUP(Invoice.CLI,lcBillDeny) > 0 
               THEN NEXT.
 
               FIND FIRST ttIR where
                          ttIR.BillCode = lcIntItem   AND
                          ttIR.CLI      = Invoice.CLI AND
                          ttIR.RowType  = 9 NO-ERROR.
                          
               IF NOT AVAILABLE ttIR THEN DO:
                  CREATE ttIR.
                  ASSIGN ttIR.FromDate  = poDate 
                         ttIR.ToDate    = pDate2
                         ttIR.BillCode  = lcIntItem
                         ttIR.CLI       = Invoice.CLI
                         ttIR.MsSeq     = Invoice.MsSeq
                         ttIR.FFRow     = FALSE
                         ttIR.RowType   = 9
                         ttIR.bVat      = FALSE.
               END.

               ASSIGN ttIR.Qty     = ttIR.Qty + 1
                      ttIR.dNet    = ttIR.dNet + CustIntEvent.Amt
                      ttIR.dgross  = ttIR.dNet
                      lLoop        = lLoop + 1.
            
               fInvoiceItem(4,
                            ttIR.MsSeq,
                            RECID(CustIntEvent)).
            END.

            /* interest rows were created, vat handling for rows */
            IF lLoop > 0 THEN DO:
               fInvRowVAT (xCustomer.VATUsage,
                           xCustomer.Region,    
                           xCustomer.Category). 
            END.
            
            /* total invoicable sum */
            FOR EACH ttIR:

               ASSIGN
                  /* from several to 2 decimals */
                  ttIR.Amt        = round(ttIR.dNet,2)
                  ttIR.GrossAmt   = round(ttIR.dgross,2)
                  /* amounts of Billed products */
                  lSum2           = lSum2 + ttIR.Qty
                  /* seconds into minutes 
                  ttIR.Minutes    = int(ttIR.Minutes / 60)
                  */
                  ttIR.PeakMin    = int(ttIR.PeakMin / 60)
                  ttIR.OffPeakMin = int(ttIR.OffPeakMin / 60).

               /* possible vat has now been removed -> temporary settings
                  can be cleared */
               IF xCustomer.VATUsage >= 3 AND xCustomer.VatUsage <= 4 
               THEN ASSIGN ttIR.VatCode = 0
                           ttIR.VatPerc = 0.
 
               IF NOT CAN-FIND(BillItem WHERE 
                               BillItem.Brand    = gcBrand AND
                               BillItem.BillCode = ttir.BillCode)
               THEN DO:
                  ASSIGN lRejBill    = TRUE
                         lcRejReason = "Product" + ttir.BillCode + "Missing" +
                                       "(" + STRING(ttIR.RowType) + ")"
                         lcRejCLI    = ttIR.CLI.              
                  LEAVE. 
               END.

               /* amounts by vat percent (one invoice can include only one 
                  type of vat usage -> one vatcode per percent) */
               FIND FIRST ttRowVat WHERE
                          ttRowVat.CLI     = ttIR.CLI   AND
                          ttRowVat.MsSeq   = ttIR.MsSeq AND
                          ttRowVat.VatPerc = ttIR.VatPerc NO-ERROR.
               
               IF NOT AVAILABLE ttRowVat THEN DO:
                  CREATE ttRowVat.
                  ASSIGN ttRowVat.CLI     = ttIR.CLI
                         ttRowVat.MsSeq   = ttIR.MsSeq
                         ttRowVat.VatPerc = ttIR.VatPerc
                         ttRowVat.VatCode = ttIR.VatCode.
                  
                  IF ttRowVat.VatCode > 0 THEN DO:  
                     FIND VATCode WHERE VATCode.VatCode = ttRowVat.VATCode
                     NO-LOCK NO-ERROR. 
                     IF AVAILABLE VatCode
                     THEN ttRowVat.VatAcc = VatCode.AccNum.
                  END.
                  
               END.
 
               ttRowVat.Amount = ttRowVat.Amount + ttIR.Amt.
            END.

            /* reject reason found, write it to log */
            IF lRejBill THEN DO:    
                fELog(katun,"INVOICE:" + lcRejReason + 
                        ":Customer:" + string(xCustomer.CustNum)).
                fErrorLog(xCustomer.CustNum,
                          lcRejCLI,
                          lcRejReason).
                NEXT INVRUNLOOP.
            END.
            
            /* make an invoice for each cli that had events */
            RUN pInvoiceHeader(idtInvDate,
                               pDate1,
                               pDate2,
                               extratime,
                               iiCustQty,
                               iiInvType).
                               
         END. /* TRANS */

      END. /* can-find first ttir */

   END.

   HIDE MESSAGE NO-PAUSE.
   
END PROCEDURE.

PROCEDURE pCancel:

   /* IF we answered no, we should DELETE ALL invoices AND rows recently made */
   message "Cancelling invoices, wait ...".

   FOR EACH newinv:

      RUN Inv/del_inv (newinv.InvNum). 

      /* no need to save eventlog to db */
      FOR EACH EventLog EXCLUSIVE-LOCK WHERE
               EventLog.TableName = "Invoice" AND
               EventLog.Key       = STRING(newinv.InvNum):
          DELETE EventLog.
      END.

      /* row to external eventlog however */
      fELog(katun,"INVOICE:" + string(newinv.InvNum) + ":Cancelled").

      DELETE newinv.
   END.

END PROCEDURE.

PROCEDURE pCleanInvSeq:

   DEF INPUT PARAMETER idtInvDate AS Date NO-UNDO.
   DEF INPUT PARAMETER idPeriodTo AS DEC  NO-UNDO.
   DEF INPUT PARAMETER iiInvCust  AS INT  NO-UNDO.
  
   DEF BUFFER bufseq1   FOR InvSeq.
   DEF BUFFER bufseq2   FOR InvSeq.
   DEF BUFFER bufseq3   FOR InvSeq.
   DEF BUFFER xcall     FOR FixCDR.
   DEF BUFFER mcall     FOR MobCDR.
   DEF BUFFER bvas      FOR VASCdr.

   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.CustNum = iiInvCust  AND
            InvSeq.ToDate <= idtInvDate AND
            InvSeq.Billed = FALSE
   BREAK BY InvSeq.MsSeq
         BY InvSeq.ToDate DESC:

      IF NOT FIRST-OF(InvSeq.MsSeq) THEN NEXT.
 
      /* specific clis */
      IF lcMSSeqLst > "" AND 
         LOOKUP(STRING(InvSeq.MsSeq),lcMSSeqLst) = 0
      THEN NEXT.
 
      lcCLI = "".
      /* get the latest with msseq index in case msisdn has been changed */
      FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                MSOwner.MsSeq   = InvSeq.MsSeq AND
                MsOwner.TsBeg   < idPeriodTo   AND
                MsOwner.InvCust = InvSeq.CustNum:
         lcCLI = MsOwner.CLI.
      END.
      IF lcCLI = "" THEN NEXT.  
 
      /* billing denied */
      IF LOOKUP(lcCLI,lcBillDeny) > 0 THEN NEXT.
       
      CREATE ttInvSeq.
      ASSIGN ttInvSeq.InvSeq = InvSeq.InvSeq
             ttInvSeq.MsSeq  = InvSeq.MsSeq.
   END.
      
   FOR EACH ttInvSeq,
      FIRST InvSeq NO-LOCK WHERE
            InvSeq.InvSeq = ttInvSeq.InvSeq:
            
      /* UPDATE InvSeq FOR Calls & remove unused InvSeq record */
      FOR EACH bufseq1 NO-LOCK USE-INDEX MsSeq WHERE
               bufseq1.MsSeq   = InvSeq.MsSeq   AND
               bufseq1.CustNum = InvSeq.CustNum AND
               bufseq1.ToDate <= InvSeq.ToDate  AND
               bufseq1.Billed  = FALSE          AND
               recid(bufseq1) NE recid(InvSeq):

         /* change NEW InvSeq TO previously uninvoiced Calls */
         FOR EACH FixCDR no-lock USE-INDEX InvSeq where
                  FixCDR.InvSeq = bufseq1.InvSeq:

            /* reduce TRANSACTION lockings */
            DO TRANS:
               FIND xcall where recid(xcall) = recid(FixCDR) EXCLUSIVE-LOCK.
               xcall.InvSeq = InvSeq.InvSeq.
            END.      

         END.

         /* change NEW InvSeq TO previously uninvoiced Calls */
         FOR EACH MobCDR no-lock where
                  MobCDR.InvCust = bufseq1.CustNum AND
                  MobCDR.InvSeq  = bufseq1.InvSeq  AND
                  MobCDR.MsSeq   = ttInvSeq.MsSeq:

            /* reduce TRANSACTION lockings */
            DO TRANS:
               FIND mcall where recid(mcall) = recid(MobCDR) EXCLUSIVE-LOCK.
               mcall.InvSeq = InvSeq.InvSeq.
            END.      

         END.

         FOR EACH VASCdr NO-LOCK WHERE
                  VASCDR.InvCust = bufseq1.CustNum AND
                  VASCdr.invseq  = bufseq1.invseq  AND
                  VASCDR.MsSeq   = ttInvSeq.MsSeq:

            /* reduce TRANSACTION lockings */
            DO TRANS:
               FIND bvas WHERE recid(bvas) = recid(VASCdr) EXCLUSIVE-LOCK.
               bvas.invseq = invseq.invseq.
            END.      

         END.
         
         /* reduce TRANSACTION lockings */
         DO TRANS:

            FIND bufseq2 where recid(bufseq2) = recid(InvSeq) EXCLUSIVE-LOCK.
            /* UPDATE Calls FROM FOR CURRENT InvSeq record */
            bufseq2.FromDate = min(bufseq1.FromDate,InvSeq.FromDate).

            /* remove Unbilled duplicates */
            FIND bufseq3 where recid(bufseq3) = recid(bufseq1) EXCLUSIVE-LOCK.
            DELETE bufseq3.

         END.

      END.

   END.

END PROCEDURE.   

PROCEDURE pFixCDR:

   DEF INPUT PARAMETER pFixInvSeq    AS INT  NO-UNDO.
   DEF INPUT PARAMETER pAccGrp       AS INT  NO-UNDO.
   DEF INPUT PARAMETER pDate1        AS Date NO-UNDO.
   DEF INPUT PARAMETER pDate2        AS Date NO-UNDO.
   DEF INPUT-OUTPUT PARAMETER pODate AS Date NO-UNDO.

   DEF VAR lOldCall AS LOG  NO-UNDO.
   DEF VAR lCall    AS LOG  NO-UNDO.
   DEF VAR lCLI     AS CHAR NO-UNDO.
   DEF VAR liCount  AS INT  NO-UNDO. 

   lPrevVAT = ?.

   FOR EACH FixCDR no-lock USE-INDEX InvSeq where
            FixCDR.InvSeq  = pFixInvSeq.

      &IF "{&InitPersistent}" NE "NO" 
      &THEN 
      /* check / calculate volume discount (disctype 1 = fixed%) */
      IF FixCDR.DiscType = 2 THEN fVolDiscFixed(BUFFER FixCDR). 
      &ENDIF 
      
      /* different vat handling */
      IF lPrevVat = ? THEN lPrevVat = FixCDR.VatIncl.
      ELSE IF FixCDR.VatIncl NE lPrevVat THEN lDblVat = TRUE.

       /* DO TRANS TO reduce lockings */
      DO TRANS:

         /* older that CURRENT invoicing Period ? */
         ASSIGN
            lOldCall = (FixCDR.Date < pDate1)
            pODate   = min(pODate,FixCDR.Date).

         FIND FIRST ttIR where
                    ttIR.BillCode = FixCDR.BillCode AND
                    ttIR.CLI      = FixCDR.CLI      AND
                    ttIR.Bvat     = FixCDR.VATIncl  AND
                    ttIR.bOld     = lOldCall        AND
                    ttIR.RowType  = 1               AND 
                    /* prefix when needed */
                   (IF LOOKUP(FixCdr.BillCode,lcMarkPref) > 0 
                    THEN ttIR.Prefix = FixCDR.Prefix 
                    ELSE TRUE)                    
         NO-ERROR.

         IF NOT AVAIL ttIR THEN DO:
            CREATE ttIR.
            ASSIGN
               ttIR.BillCode  = FixCDR.BillCode
               ttIR.CLI       = FixCDR.CLI
               ttIR.bOld      = lOldCall
               ttIR.bvat      = FixCDR.VATIncl
               ttIR.RowType   = 1
               ttIr.Prefix    = FixCDR.Prefix 
                                WHEN LOOKUP(FixCDR.BillCode,lcMarkPref) > 0
               lCall          = TRUE.

             /* CURRENT invoicing Period */
            IF NOT bOld THEN ASSIGN
               ttIR.FromDate = pDate1
               ttIR.ToDate   = pDate2.

            /* older Calls */   
            ELSE ASSIGN
               ttIR.FromDate = FixCDR.Date
               ttIR.ToDate   = FixCDR.Date.
         END.

         /* Date of oldest AND newest call when NOT CURRENT Period */
         IF bOld THEN ASSIGN 
            ttIR.FromDate = min(ttIR.FromDate,FixCDR.Date)
            ttIR.ToDate   = max(ttIR.ToDate,FixCDR.Date). 

         /* is currency unit full or sub (assign ldnet & ldgross) */
         fCurrUnit(FixCDR.GrossPrice - FixCDR.DiscValue,
                   FixCDR.GrossPrice,
                   FixCDR.CurrUnit,
                   "",
                   FixCDR.TariffID,
                   gcBrand,
                   OUTPUT ldNet,
                   OUTPUT ldGross).

         ASSIGN
            ttIR.Qty        = ttIR.Qty        + 1
            ttIR.dNet       = ttIR.dNet       + ldNet
            ttIR.dgross     = ttIR.dgross     + ldGross
            ttIR.PeakMin    = ttIR.PeakMin    + FixCDR.PKDuration
            ttIR.OffPeakMin = ttIR.OffPeakMin + FixCDR.OPDuration
            ttIR.Minutes    = ttIR.Minutes    + FixCDR.Duration.

         /* webspeed calculations: CCN */
         IF liInvCCN = 1 THEN DO:
            FIND FIRST ttCCN where
                       ttCCN.InvSeq    = FixCDR.InvSeq    AND
                       ttCCN.CCN       = FixCDR.CCN       AND
                       ttCCN.BillCode  = FixCDR.BillCode  AND
                       ttCCN.TariffNum = FixCDR.TariffID  AND
                       ttCCN.BVat      = FixCDR.VATIncl   AND
                       ttCCN.CLI       = FixCDR.CLI
            no-error.
            IF NOT AVAIL ttCCN THEN DO:
               CREATE ttCCN.
               ASSIGN 
               ttCCN.InvSeq    = FixCDR.InvSeq
               ttCCN.InvNum    = -1 * FixCDR.InvSeq
               ttCCN.CCN       = FixCDR.CCN
               ttCCN.BillCode  = FixCDR.BillCode
               ttCCN.TariffNum = FixCDR.TariffID
               ttCCN.bvat      = FixCDR.VATIncl
               ttCCN.CLI       = FixCDR.CLI.
            END.
            ASSIGN
            ttCCN.Qty      = ttCCN.Qty      + 1
            ttCCN.Minutes  = ttCCN.Minutes  + FixCDR.Duration
            ttCCN.Amt      = ttCCN.Amt      + ldNet
            ttCCN.GenPrice = ttCCN.GenPrice + 0.

            /* TimeBand durations */
            DO liCount = 1 TO 6:
               ttCCN.TBDurat[liCount] = ttCCN.TBDurat[liCount] +
                                        FixCDR.TBDurat[liCount].
            END.
         END.
         
         /* webspeed calculations: CLI */
         lCLI = "".
         /* 1: a known freephone number */
         lD2D = fHMS2TS(FixCDR.Date,string(FixCDR.TimeStart,"hh:mm:ss")).
         FIND FIRST BDestHist where
                    BDestHist.Brand   = gcBrand        AND 
                    BDestHist.BDest   = FixCDR.BDest   AND
                    BDestHist.CustNum = FixCDR.InvCust AND
                    BDestHist.vFrom  <= lD2D           AND
                    BDestHist.vTo    >= lD2D
         NO-LOCK NO-ERROR.

         /* 2: a know CLI */
         IF NOT AVAIL BDestHist THEN DO:
            FIND FIRST CLI where
                       CLI.CLI  = FixCDR.CLI
            no-lock no-error.
            IF AVAIL CLI THEN lCLI = FixCDR.CLI.
         END.
         ELSE lCLI = FixCDR.BSub.

         /* IF known CLI OR BDest */
         FIND FIRST ttCLI where
                    ttCLI.MsSeq     = 0                AND    
                    ttCLI.InvSeq    = FixCDR.InvSeq    AND
                    ttCLI.CLI       = lCLI             AND
                    ttCLI.CCN       = FixCDR.CCN       AND
                    ttCLI.BillCode  = FixCDR.BillCode  AND
                    ttCLI.TariffNum = FixCDR.TariffID  AND
                    ttCLI.BVat      = FixCDR.VATIncl   AND
                    ttCLI.ServRid   = ""               AND
                    ttCLI.MPMRid    = "" 
         no-error.
         
         IF NOT AVAIL ttCLI THEN DO:
            CREATE ttCLI.
            ASSIGN ttCLI.InvNum    = -1 * FixCDR.InvSeq
                   ttCLI.InvSeq    = FixCDR.InvSeq
                   ttCLI.MsSeq     = 0
                   ttCLI.CCN       = FixCDR.CCN
                   ttCLI.BillCode  = FixCDR.BillCode
                   ttCLI.bvat      = FixCDR.VATIncl
                   ttCLI.TariffNum = FixCDR.TariffID
                   ttCLI.CLI       = lCLI
                   ttCLI.FromDate  = FixCDR.Date
                   ttCli.ToDate    = FixCDR.Date
                   ttCLI.ServRid   = ""
                   ttCLI.MPMRid    = "".
         END.

         ASSIGN ttCLI.Qty      = ttCLI.Qty      + 1
                ttCLI.Minutes  = ttCLI.Minutes  + FixCDR.Duration
                ttCLI.Amt      = ttCLI.Amt      + ldNet
                ttCLI.GenPrice = ttCLI.GenPrice + 0
                ttCLI.FromDate = MIN(ttCLI.FromDate,FixCDR.Date)
                ttCLI.ToDate   = MAX(ttCLI.ToDate,FixCDR.Date).

         /* TimeBand durations */
         DO liCount = 1 TO 6:
            ttCLI.TBDurat[liCount] = ttCLI.TBDurat[liCount] +
                                     FixCDR.TBDurat[liCount].
         END.

         /* for fat calculation in period level */       
         liFatPer = YEAR(FixCDR.Date) * 100 + MONTH(FixCDR.Date).

         FIND FIRST ttCallFat where
                    ttCallFat.CLI       = lCLI             AND
                    ttCallFat.CCN       = FixCDR.CCN       AND
                    ttCallFat.BillCode  = FixCDR.BillCode  AND
                    ttCallFat.Period    = liFatPer         AND
                    ttCallFat.VatIncl   = FixCDR.VatIncl NO-ERROR.

         IF NOT AVAIL ttCallFat THEN DO:
            CREATE ttCallFat.
            ASSIGN ttCallFat.CCN       = FixCDR.CCN     
                   ttCallFat.BillCode  = FixCDR.BillCode   
                   ttCallFat.CLI       = lCLI
                   ttCallFat.VatIncl   = FixCDR.VatIncl
                   ttCallFat.Period    = liFatPer.
         END.

         ASSIGN ttCallFat.Qty = ttCallFat.Qty + 1
                ttCallFat.Amt = ttCallFat.Amt + ldNet.

      END. /* DO TRANS */

   END. /* FixCDR */

END PROCEDURE.

PROCEDURE pMobCDR:

   DEF INPUT PARAMETER pCustNum      AS INT  NO-UNDO.
   DEF INPUT PARAMETER pAccGrp       AS INT  NO-UNDO.
   DEF INPUT PARAMETER pDate1        AS Date NO-UNDO.
   DEF INPUT PARAMETER pDate2        AS Date NO-UNDO.
   DEF INPUT-OUTPUT PARAMETER pODate AS Date NO-UNDO.

   DEF VAR lOldCall AS LOG  NO-UNDO.
   DEF VAR lCall    AS LOG  NO-UNDO.
   DEF VAR lCLI     AS CHAR NO-UNDO.

   lPrevVat = ?.

   FOR EACH ttInvSeq,
       EACH MobCDR no-lock USE-INDEX InvSeq where
            MobCDR.InvCust   = pCustNum        AND
            MobCDR.InvSeq    = ttInvSeq.InvSeq AND
            MobCDR.ErrorCode = 0:
      
      IF LOOKUP(MobCDR.CLI,lcBillDeny) > 0 THEN NEXT.

      &IF "{&InitPersistent}" NE "NO" 
      &THEN 
      /* check / calculate volume discount (disctype 1 = fixed%) */
      IF MobCDR.DiscType = 2 THEN fVolDiscMob(BUFFER MobCDR). 
      &ENDIF
      
      /* different vat handling */   
      IF lPrevVat = ? THEN lPrevVat = MobCDR.VatIncl.
      ELSE IF MobCDR.VatIncl NE lPrevVat THEN lDblVat = TRUE.

       /* DO TRANS TO reduce lockings */
      DO TRANS:

         /* older that CURRENT invoicing Period ? */
         ASSIGN
            lOldCall = (MobCDR.DateSt < pDate1)
            pODate   = min(pODate,MobCDR.DateSt).

         FIND FIRST ttIR where
                    ttIR.BillCode = MobCDR.BillCode AND
                    ttIR.CLI      = MobCDR.CLI      AND
                    ttIR.Bvat     = MOBCDR.VatIncl  AND
                    ttIR.bOld     = lOldCall        AND
                    ttIR.RowType  = 2               AND
                    ttIR.MsSeq    = MobCDR.MsSeq    AND 
                    /* prefix when needed */
                   (IF LOOKUP(MobCDR.BillCode,lcMarkPref) > 0 
                    THEN ttIR.Prefix = MobCDR.BPref 
                    ELSE TRUE)                    
         no-error.

         IF NOT AVAIL ttIR THEN DO:
            CREATE ttIR.
            ASSIGN
               ttIR.BillCode  = MobCDR.BillCode
               ttIR.CLI       = MobCDR.CLI
               ttIR.MsSeq     = MobCDR.MsSeq
               ttIR.bOld      = lOldCall
               ttIr.Prefix    = MobCDR.BPref 
                                WHEN LOOKUP(MobCDR.BillCode,lcMarkPref) > 0
               ttIR.Bvat      = MobCDR.VatIncl
               ttIR.RowType   = 2
               lCall          = TRUE.

            /* CURRENT invoicing Period */
            IF NOT bOld THEN ASSIGN
               ttIR.FromDate = pDate1
               ttIR.ToDate = pDate2.
            /* older Calls */   
            ELSE ASSIGN
               ttIR.FromDate = MobCDR.DateSt
               ttIR.ToDate = MobCDR.DateSt.
         END.

         /* Date of oldest AND newest call when NOT CURRENT Period */
         IF bOld THEN ASSIGN 
            ttIR.FromDate = min(ttIR.FromDate,MobCDR.DateSt)
            ttIR.ToDate = max(ttIR.ToDate,MobCDR.DateSt). 

         /* is currency unit full or sub (assign ldnet & ldgross) */
         fCurrUnit(MobCDR.Amount,
                   MobCDR.MPMAmt,   /* mpm */
                   MobCDR.CurrUnit,
                   "",
                   MobCDR.TariffNum,
                   gcBrand,
                   OUTPUT ldNet,
                   OUTPUT ldGross).

         ASSIGN
            ttIR.Qty      = ttIR.Qty     + 1
            ttIR.dNet     = ttIR.dNet    + ldNet
            ttIR.dgross   = ttIR.dgross  + ldGross
            ttIR.Minutes  = ttIR.Minutes + MobCDR.BillDur.

         /* webspeed calculations: CCN */
         IF liInvCCN = 1 THEN DO:
            FIND FIRST ttCCN where
                       ttCCN.MsSeq     = MobCDR.MsSeq     AND 
                       ttCCN.InvSeq    = MobCDR.InvSeq    AND
                       ttCCN.CCN       = MobCDR.CCN       AND
                       ttCCN.BillCode  = MobCDR.BillCode  AND
                       ttCCN.TariffNum = MobCDR.TariffNum AND
                       ttCCN.bvat      = MobCDR.VatIncl   AND
                       ttCCN.CLI       = MobCDR.CLI
                       no-error.
            IF NOT AVAIL ttCCN THEN DO:
               CREATE ttCCN.
               ASSIGN 
               ttCCN.InvSeq    = MobCDR.InvSeq
               ttCCN.InvNum    = -1 * MobCDR.InvSeq
               ttCCN.MsSeq     = MobCDR.MsSeq
               ttCCN.CCN       = MobCDR.CCN
               ttCCN.BillCode  = MobCDR.BillCode
               ttCCN.TariffNum = MobCDR.TariffNum
               ttCCN.bvat      = MobCDR.VatIncl
               ttCCN.CLI       = MobCDR.CLI.
            END.
            ASSIGN
            ttCCN.Qty      = ttCCN.Qty  + 1
            ttCCN.Minutes  = ttCCN.Minutes  + MobCDR.BillDur
            ttCCN.Amt      = ttCCN.Amt      + ldNet
            ttCCN.GenPrice = ttCCN.GenPrice + ldGross
            ttCCN.DataAmt  = ttCCN.DataAmt  + MobCDR.DataIn + MobCDR.DataOut.
         END.
         
         /* webspeed calculations: CLI */
         lcli = MobCDR.CLI.

         /* IF known CLI OR BDest */
         FIND FIRST ttCLI where
                    ttCLI.MsSeq     = MobCDR.MsSeq     AND
                    ttCLI.InvSeq    = MobCDR.InvSeq    AND
                    ttCLI.CLI       = lCLI             AND
                    ttCLI.CCN       = MobCDR.CCN       AND
                    ttCLI.BillCode  = MobCDR.BillCode  AND
                    ttCLI.TariffNum = MobCDR.TariffNum AND
                    ttCLI.bvat      = MobCDR.VatIncl   AND
                    ttCLI.ServRid   = MobCDR.ServRid   AND
                    ttCLI.MPMRid    = MobCDR.MPMRid 
         no-error.

         IF NOT AVAIL ttCLI THEN DO:
            CREATE ttCLI.
            ASSIGN ttCLI.InvNum    = -1 * MobCDR.InvSeq
                   ttCLI.InvSeq    = MobCDR.InvSeq 
                   ttCLI.MsSeq     = MobCDR.MsSeq
                   ttCLI.CCN       = MobCDR.CCN     
                   ttCLI.BillCode  = MobCDR.BillCode   
                   ttCLI.CLI       = lCLI
                   ttCLI.TariffNum = MobCDR.TariffNum
                   ttCli.bvat      = MobCDR.VatIncl
                   ttCLI.FromDate  = MobCDR.DateSt
                   ttCLI.ToDate    = MobCDR.DateSt
                   ttCLI.ServRid   = MobCDR.ServRid
                   ttCLI.MPMRid    = MobCDR.MPMRid. 
         END.

         ASSIGN ttCLI.Qty      = ttCLI.Qty      + 1
                ttCLI.Minutes  = ttCLI.Minutes  + MobCDR.BillDur
                ttCLI.Amt      = ttCLI.Amt      + ldNet
                ttCLI.GenPrice = ttCLI.GenPrice + ldGross
                ttCLI.MPMAmt   = ttCLI.MPMAmt   + ldGross
                ttCLI.DataAmt  = ttCLI.DataAmt  + MobCDR.DataIn + 
                                                  MobCDR.DataOut
                ttCLI.FromDate = MIN(ttCLI.FromDate,MobCDR.DateSt)
                ttCLI.ToDate   = MAX(ttCLI.ToDate,MobCDR.DateSt).

         /* for fat calculation on period level */       
         liFatPer = YEAR(MobCDR.DateSt) * 100 + MONTH(MobCDR.DateSt).

         FIND FIRST ttCallFat where
                    ttCallFat.CLI       = lCLI             AND
                    ttCallFat.CCN       = MobCDR.CCN       AND
                    ttCallFat.BillCode  = MobCDR.BillCode  AND
                    ttCallFat.Period    = liFatPer         AND
                    ttCallFat.VatIncl   = MobCDR.VatIncl NO-ERROR.

         IF NOT AVAIL ttCallFat THEN DO:
            CREATE ttCallFat.
            ASSIGN ttCallFat.CCN       = MobCDR.CCN     
                   ttCallFat.BillCode  = MobCDR.BillCode   
                   ttCallFat.CLI       = lCLI
                   ttCallFat.MsSeq     = MobCDR.MsSeq
                   ttCallFat.VatIncl   = MobCDR.VatIncl
                   ttCallFat.Period    = liFatPer.
         END.

         ASSIGN ttCallFat.Qty = ttCallFat.Qty + 1
                ttCallFat.Amt = ttCallFat.Amt + ldNet.

         /*   
         IF Mobcdr.ChrInfo > "" AND NUM-ENTRIES(Mobcdr.chrinfo,"|") = 3 THEN DO:
            FIND FIRST ttServiceCounter WHERE 
                       ttServiceCounter.MSSeq  = Mobcdr.MSSeq               AND 
                       ttServiceCounter.Period = YEAR(Mobcdr.DAtest) * 100 +
                                                 MONTH(Mobcdr.Datest)       AND
                       ttServiceCounter.SLSeq  = INT(ENTRY(2,
                                                    Mobcdr.ChrInfo,
                                                    "|"))              
            NO-ERROR.                                         
                                                 
            IF NOT AVAIL ttServiceCounter THEN DO:
               CREATE ttServiceCounter.
               ASSIGN
                  ttServiceCounter.MSSeq  = Mobcdr.MSSeq      
                  ttServiceCounter.CLI    = MobCDR.CLI
                  ttServiceCounter.Period = YEAR(Mobcdr.DAtest) * 100 +
                                            MONTH(Mobcdr.Datest)
                  ttServiceCounter.SLSeq  = INT(ENTRY(2,
                                            Mobcdr.ChrInfo,
                                            "|")).
            END.           
         
            ASSIGN ttServiceCounter.Amount = DEC(ENTRY(3,
                                             Mobcdr.ChrInfo,
                                             "|")) .
                    
         END.
         */
         
      END. /* DO TRANS */

   END. /* MobCDR  */

END PROCEDURE.

PROCEDURE pVASCDR:

   DEF INPUT        PARAMETER pinvSeq   AS INT  NO-UNDO.
   DEF INPUT        PARAMETER pDate1    AS DATE NO-UNDO.
   DEF INPUT        PARAMETER pDate2    AS DATE NO-UNDO.
   DEF INPUT        PARAMETER iiCustNum AS INT  NO-UNDO. 
   DEF input-output PARAMETER pODate    AS DATE NO-UNDO.

   DEF VAR lOldCall   AS LOG  NO-UNDO.
   DEF VAR ldtFirst   AS DATE NO-UNDO. 

   IF NOT CAN-FIND(FIRST VASCdr WHERE
                         VASCdr.InvCust = iiCustNum AND
                         VASCdr.Invseq  = pInvSeq)  THEN RETURN. 
   
   FIND FIRST VASOper WHERE 
              VASOper.CustNum = iiCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE VASOper THEN RETURN.               
   
   /* collect vascdrs, calculate invoicing fees etc. */
   fVASInvRow(pInvSeq,
              iiCustNum,
              pDate1,
              pDate2,
              ?,
              OUTPUT ldtFirst).
              
   IF ldtFirst NE ? THEN pODate = MIN(pODate,ldtFirst).
   
   FOR EACH ttVasRow
   BY ttVasRow.BillItem
   BY ttVasRow.ToDate:
      
      CREATE ttIR.
      ASSIGN ttIR.BillCode       = ttVASRow.BillItem
             ttIR.Bvat           = FALSE
             ttIR.RowType        = 8
             ttIR.ServiceAddress = ttVASRow.ServAddr
             ttIR.ServiceName    = ttVASRow.ServName
             ttIR.FromDate       = ttVASRow.FromDate
             ttIR.ToDate         = ttVASRow.ToDate
             ttIR.Qty            = ttVASRow.VASQty
             ttIR.VASConAmt      = ttVASRow.ConAmt 
             ttIR.VASOperAmt     = ttVASRow.OperAmt
             ttIR.dgross         = -1 * ROUND(ttVasRow.ConAmt -
                                              ttVasRow.OperAmt,3)   
             ttIR.dnet           = ttVASRow.InvAmt
             liVASAcc            = VASOper.AccNum.
       
   END.
   
END PROCEDURE.

PROCEDURE pCounterVAT:

   FOR EACH ttCCN:
      /* data amounts to Kb (do this in a separate loop) */
      ttCCN.DataAmt = ttCCN.DataAmt / 1024.
   END.
                  
                  
   /* make sure that all counters use the same vat method (incl/excl) */
   FOR EACH ttCCN,
      /* get vat% from invrow (already determined in pInvRowVat) */      
      FIRST ttIR WHERE
            ttIR.BillCode = ttCCN.BillCode:

      IF ttCCN.bvat NE lCustVat THEN DO:

         IF lCustVat = TRUE THEN ASSIGN 
            ttCCN.GenPrice = ttCCN.GenPrice * (1 + ttIR.VatPerc / 100)
            ttCCN.Amt      = ttCCN.Amt  * (1 + ttIR.VatPerc / 100).
         ELSE ASSIGN
            ttCCN.GenPrice = ttCCN.GenPrice / (1 + ttIR.VatPerc / 100)
            ttCCN.Amt      = ttCCN.Amt  / (1 + ttIR.VatPerc / 100).

         FIND FIRST xxCCN WHERE
                    xxCCN.MsSeq     = ttCCN.MsSeq    AND
                    xxCCN.invseq    = ttCCN.invseq   AND
                    xxCCN.ccn       = ttCCN.ccn      AND
                    xxCCN.BillCode  = ttCCN.BillCode AND
                    xxCCN.TariffNum = ttCCN.TariffNum AND
                    xxCCN.bvat      = lCustVat NO-ERROR.

         IF AVAIL xxCCN THEN DO:

            ASSIGN
            xxCCN.Qty      = xxCCN.Qty  + ttCCN.Qty    
            xxCCN.min      = xxCCN.min  + ttCCN.min
            xxCCN.Amt      = xxCCN.Amt  + ttCCN.Amt
            xxCCN.GenPrice = xxCCN.GenPrice + ttCCN.GenPrice
            xxCCN.DataAmt  = xxCCN.DataAmt + ttCCN.DataAmt.

            DELETE ttCCN.     
         END.
      
         ELSE ttCCN.bVat = lCustVat.
      END.
      
   END.  /*ttCCN */        

   FOR EACH ttCLI:
      /* data amounts to Kb (do this in a separate loop) */
      ttCLI.DataAmt = ttCLI.DataAmt / 1024.
   END.
                  
   FOR EACH ttCLI,
      FIRST ttIR WHERE
            ttIR.BillCode = ttCLI.BillCode:

      /* amount with vat is needed for fat and minfee */
      IF ttCLI.bVat 
      THEN ttCLI.AmtWVat = ttCLI.Amt.
      ELSE ttCLI.AmtWVat = ttCLI.Amt * (1 + ttIR.VatPerc / 100). 
      
      IF ttCLI.bvat NE lCustVat THEN DO:
      
         IF lCustVat = TRUE THEN ASSIGN 
            ttCLI.GenPrice = ttCLI.GenPrice * (1 + ttIR.VatPerc / 100)
            ttCLI.Amt      = ttCLI.Amt  * (1 + ttIR.VatPerc / 100)
            ttCLI.MPMAmt   = ttCLI.MPMAmt * (1 + ttIR.VatPerc / 100).
         ELSE ASSIGN
            ttCLI.GenPrice = ttCLI.GenPrice / (1 + ttIR.VatPerc / 100)
            ttCLI.Amt      = ttCLI.Amt  / (1 + ttIR.VatPerc / 100)
            ttCLI.MPMAmt   = ttCLI.MPMAmt / (1 + ttIR.VatPerc / 100).

         FIND FIRST xxCLI WHERE
                    xxCLI.MsSeq     = ttCLI.MsSeq     AND
                    xxCLI.invseq    = ttCLI.invseq    AND
                    xxCLI.CLI       = ttCLI.CLI       AND                 
                    xxCLI.ccn       = ttCLI.ccn       AND
                    xxCLI.BillCode  = ttCLI.BillCode  AND
                    xxCLI.TariffNum = ttCLI.TariffNum AND
                    xxCLI.bvat      = lCustVat        AND
                    xxCLI.ServRid   = ttCLI.ServRid   AND
                    xxCLI.MPMRid    = ttCLI.MPMRid NO-ERROR.

         IF AVAIL xxCLI THEN DO:
            ASSIGN
            xxCLI.Qty      = xxCLI.Qty     + ttCLI.Qty    
            xxCLI.min      = xxCLI.min     + ttCLI.min
            xxCLI.Amt      = xxCLI.Amt     + ttCLI.Amt
            xxCLI.GenPrice = xxCLI.GenPrice + ttCLI.GenPrice
            xxCLI.MPMAmt   = xxCLI.MPMAmt  + ttCLI.MPMAmt
            xxCLI.DataAmt  = xxCLI.DataAmt + ttCLI.DataAmt
            xxCLI.AmtWVat  = xxCLI.AmtWVat + ttCLI.AmtWVat
            xxCLI.FromDate = MIN(xxCLI.FromDate,ttCLI.FromDate)
            xxCLI.ToDate   = MAX(xxCLI.ToDate,ttCLI.ToDate).

            DELETE ttCLI.     

         END.
      
         ELSE ttCLI.bVat = lCustVat.
      END.
      
   END.  /*ttCLI */        

   FOR EACH ttCallFat,
      FIRST ttIR WHERE
            ttIR.BillCode = ttCallFat.BillCode:

      /* amount with vat is needed for fat and minfee */
      IF ttCallFat.VatIncl THEN ASSIGN 
         ttCallFat.AmtWVat = ttCallFat.Amt
         ttCallFat.Amt     = ttCallFat.Amt / (1 + ttIR.VatPerc / 100).
      ELSE ttCallFat.AmtWVat = ttCallFat.Amt * (1 + ttIR.VatPerc / 100). 
      
   END.  /*ttCallFat */        

END PROCEDURE.


PROCEDURE pUpdInvGroup:

   FOR EACH newinv 
   BREAK BY newinv.InvGroup
         BY newinv.InvType
         BY newinv.ExtInvID:

      IF last-of(newinv.InvType) THEN DO:

         FIND Invoice WHERE Invoice.InvNum = NewInv.InvNum NO-LOCK NO-ERROR.
         IF AVAILABLE Invoice THEN    
         /* update last used invoice number */
         fUpdateInvNum(NewInv.InvGroup,
                       Invoice.InvType,
                       Invoice.InvDate,
                       Invoice.ExtInvID).
      END.

      DELETE NewInv.  
   END.

END PROCEDURE.

PROCEDURE pGetAmt:

   DEF OUTPUT PARAMETER pQty  AS INT NO-UNDO.
   DEF OUTPUT PARAMETER pAmt  AS dec NO-UNDO.
   DEF OUTPUT PARAMETER pVAmt AS dec NO-UNDO. 

   FOR EACH newinv no-lock:
      ASSIGN pQty  = pQty  + 1
             pAmt  = pAmt  + newinv.Qty
             pVAmt = pVAmt + newinv.VATAmt. 
   END.

END PROCEDURE.

PROCEDURE pGetBillRunID:

   DEF OUTPUT PARAMETER ocBillRun AS CHAR NO-UNDO.
   
   FOR FIRST NewInv,
       FIRST Invoice NO-LOCK WHERE
             Invoice.InvNum = NewInv.InvNum:
      ocBillRun = Invoice.BillRun.       
   END.
   
END PROCEDURE.

/* called from cleanrunui, in order to pass few extra parameters */
PROCEDURE pCleanRun:

   DEF INPUT  PARAMETER pDate      AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate1     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate2     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER ciperiod   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idCredLoss AS DEC  NO-UNDO.
   DEF INPUT  PARAMETER iiMinDays  AS INT  NO-UNDO.

   ASSIGN ldCredLoss = idCredLoss
          liMinDays  = iiMinDays.

   run pCreateInv (pDate,
                   pDate1,
                   pDate2,
                   ciperiod,
                   extratime,
                   TRUE,
                   TRUE,
                   0,
                   1,
                   "").

END PROCEDURE.

PROCEDURE pEndInvoice:

   DEF INPUT  PARAMETER pDate      AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate1     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate2     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER ciperiod   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiEndType  AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiCreated  AS INT  NO-UNDO.

   liEndInvType = iiEndType.

   RUN pCreateInv (pDate,
                   pDate1,
                   pDate2,
                   ciperiod,
                   0,
                   TRUE,
                   TRUE,
                   0,
                   1,
                   "").

   /* was invoice created */
   oiCreated = liLastCreated.
   
END PROCEDURE.

PROCEDURE pCleanMem:

   &IF "{&InitPersistent}" NE "NO" 
   &THEN 
   IF VALID-HANDLE(fhVDHandle) THEN DELETE PROCEDURE fhVDHandle.
   IF VALID-HANDLE(fhRRHandle) THEN DELETE PROCEDURE fhRRHandle.
   IF VALID-HANDLE(fhDCHandle) THEN DELETE PROCEDURE fhDCHandle.
   &ENDIF
   
   /* clean out dynamic temp-tables that eventlog created */
   fCleanEventObjects(). 

END PROCEDURE. 

/* create deposit/adv.payment invoices */
PROCEDURE pDepositInvoice:

   DEF INPUT  PARAMETER iiInvType  AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER iiOrderID  AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER icMSSeq    AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER pDate      AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate1     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate2     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER ciperiod   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiPaymTerm AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER ilSilent   AS LOG  NO-UNDO. 

   /* normal invoice */
   IF iiInvType = 1 THEN ASSIGN 
      liDepoAcc   = 0
      lcDepoItem  = fCParamC("FirstInvItem")
      liDepoPTerm = 0.
   /* deposit */
   ELSE IF iiInvType = 3 THEN ASSIGN 
      liDepoAcc   = fCParamI("ResDepositsAcc")
      lcDepoItem  = fCParamC("DepositItem")
      liDepoPTerm = fCParamI("DepositPayTerm").
   /* adv.payment */
   ELSE IF iiInvType = 4 THEN ASSIGN 
      liDepoAcc   = fCParamI("AdvPaymAcc")
      lcDepoItem  = fCParamC("AdvPaymItem")
      liDepoPTerm = fCParamI("AdvPaymPayTerm").
      
   IF iiInvType NE 1 THEN DO:
      IF iiPaymTerm > 0 THEN liDepoPTerm = iiPaymTerm.
   END. 
      
   ASSIGN liSilent   = ilSilent
          lcMSSeqLst = icMSSeq
          liOrderID  = iiOrderID.
          
   IF liDepoPTerm = ? THEN liDepoPTerm = 0.    

   /* billing item is mandatory */       
   IF lcDepoItem = "" OR lcDepoItem = ? THEN RETURN. 

   /* account is mandatory (if not normal invoice) */       
   IF (liDepoAcc = 0 OR liDepoAcc = ?) AND iiInvType >= 3
   THEN RETURN. 

   /* when lcDepoItem is set then only single fees with that item
      are billed, and invoice type is set as 3/4 */
   run pCreateInv (pDate,
                   pDate1,
                   pDate2,
                   ciperiod,
                   extratime,
                   FALSE,
                   FALSE,
                   1,
                   iiInvType,
                   "").

END PROCEDURE.

/* create cash invoices */
PROCEDURE pCashInvoice:

   DEF INPUT  PARAMETER iiInvType  AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER iiOrderID  AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER icMSSeq    AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER pDate      AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate1     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER pDate2     AS DATE NO-UNDO.
   DEF INPUT  PARAMETER ciperiod   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiPaymTerm AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER ilSilent   AS LOG  NO-UNDO. 

   ASSIGN liSilent  = ilSilent
          liOrderID = iiOrderID.

   run pCreateInv (pDate,
                   pDate1,
                   pDate2,
                   ciperiod,
                   extratime,
                   FALSE,
                   FALSE,
                   1,
                   iiInvType,
                   "").

END PROCEDURE.

/* create actual invoice to db */
PROCEDURE pInvoiceHeader:

   DEF INPUT  PARAMETER idtInvDate AS Date NO-UNDO.
   DEF INPUT  PARAMETER pDate1     AS Date NO-UNDO.
   DEF INPUT  PARAMETER pDate2     AS Date NO-UNDO.
   DEF INPUT  PARAMETER extratime  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiCustQty  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiInvType  AS INT  NO-UNDO. 

   DEF BUFFER bufseq  FOR InvSeq.
   DEF BUFFER bChkInv FOR Invoice. 

   DEF VAR lCPer      AS INT  NO-UNDO.
   DEF VAR lCPer2     AS INT  NO-UNDO.
   DEF VAR lBSum      AS dec  NO-UNDO.
   DEF VAR lDProd     AS CHAR NO-UNDO.
   DEF VAR lDirDisc   AS INT  NO-UNDO.
   DEF VAR lCurRate   AS dec  NO-UNDO.
   DEF VAR lInterest  AS dec  NO-UNDO.
   DEF VAR lReduce    AS dec  NO-UNDO.
   DEF VAR lAdvRed    AS dec  NO-UNDO. 
   DEF VAR lInvNo     AS INT  NO-UNDO.
   DEF VAR lNotInv    AS dec  NO-UNDO.
   DEF VAR lLoop      AS INT  NO-UNDO.
   DEF VAR liLastBill AS INT  NO-UNDO. 
   DEF VAR llCreate   AS LOG  NO-UNDO. 
   DEF VAR liVoucher  AS INT  NO-UNDO. 

   DEF VAR ldBasis    AS DEC  NO-UNDO.
   DEF VAR ldVat      AS DEC  NO-UNDO.
   DEF VAR ldVatPort  AS DEC  NO-UNDO.
   DEF VAR ldRedBasis AS DEC  NO-UNDO.
   DEF VAR ldRedVat   AS DEC  NO-UNDO.
   DEF VAR liCnt      AS INT  NO-UNDO. 
   DEF VAR lcRepCode  AS CHAR NO-UNDO.
   DEF VAR lcInfo     AS CHAR NO-UNDO. 
   DEF VAR ldFromPer  AS DEC  NO-UNDO.
   DEF VAR ldToPer    AS DEC  NO-UNDO. 
   DEF VAR ldtFrom    AS DATE NO-UNDO.
   DEF VAR ldtTo      AS DATE NO-UNDO. 
   DEF VAR liType     AS INT  NO-UNDO.
   DEF VAR liCustQty  AS INT  NO-UNDO.
   DEF VAR ldAPBase   AS DEC  NO-UNDO. 
   DEF VAR ldVatPerc  AS DEC  NO-UNDO.
   DEF VAR lcExtInvID AS CHAR NO-UNDO. 
   DEF VAR lcSeqPref  AS CHAR NO-UNDO. 
   DEF VAR liInvRecid AS RECID NO-UNDO. 

   ASSIGN 
      liSeq    = 0
      lCurRate = fCurrRate(Customer.Currency,idtInvDate). 
   
   /* one invoice per each cli 
      transaction covers all invoices created here, so if an undo happens
      rollback concerns this customer totally */
   FOR EACH ttRowVat 
   BREAK BY ttRowVat.MsSeq
         BY ttRowVat.VatPerc:
   
      /* new cli, create header */
      IF FIRST-OF(ttRowVat.MsSeq) THEN DO:
   
         FIND FIRST ttInvSeq WHERE ttInvSeq.MsSeq = ttRowVat.MsSeq NO-ERROR.
         IF AVAILABLE ttInvSeq 
         THEN liSeq = ttInvSeq.InvSeq.
         ELSE liSeq = NEXT-VALUE(invseq). 

         lcCLI = ttRowVat.CLI.
         /* get the latest with msseq index in case msisdn has been changed */
         FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                   MSOwner.MsSeq   = ttRowVat.MsSeq AND
                   MsOwner.InvCust = Customer.CustNum:
            lcCLI = MsOwner.CLI.
         END.
 
         liMsSeq = ttRowVat.MsSeq.

         CREATE ttInv.
         ASSIGN ttInv.ChgStamp     = fMakeTS()
                ttInv.Brand        = Customer.Brand 
                ttInv.InvNum       = lISeq
                ttInv.InvSeq       = lISeq
                ttInv.CLI          = lcCLI
                ttInv.MsSeq        = liMsSeq 
                ttInv.CustNum      = Customer.CustNum
                ttInv.InvDate      = idtInvDate
                ttInv.FirstCall    = minimum(pODate,pDate1)
                ttInv.FromDate     = pDate1
                ttInv.ToDate       = pDate2
                ttInv.InvType      = IF iiInvType > 0
                                     THEN iiInvType
                                     ELSE 1 
                ttInv.ArAccNum     = liArAccNum                     
                ttInv.RoundAccNum  = RoundAcc
                ttInv.IntAccNum    = OTIntAcc
                ttInv.InterestAmt  = lInterest
                ttInv.Currency     = Customer.Currency 
                ttInv.ExchRate     = lCurRate
                ttInv.InterestPerm = Customer.InterestPerm
                ttInv.ClaimPerm    = IF iiInvType >= 3 AND iiInvType <= 4
                                     THEN FALSE
                                     ELSE Customer.ClaimPerm
                ttInv.VATIncl      = lCustVat
                ttInv.VatUsage     = Customer.VatUsage
                ttInv.ChargeType   = IF iiInvType >= 6 AND iiInvType <= 7
                                     THEN 1
                                     ELSE Customer.ChargeType
                ttInv.DelType      = Customer.DelType
                ttInv.SpecDel      = Customer.SpecDel
                ttInv.WInvDisp     = FALSE
                ttInv.BillRun      = lcBillRun
                ttInv.EndInvoice   = liEndInvType
                ttInv.Region       = Customer.Region.

         FIND Region WHERE Region.Region = ttInv.Region NO-LOCK NO-ERROR.
         IF AVAILABLE Region THEN ttInv.TaxZone = Region.TaxZone.
            
         /* adv.payment / deposit */
         IF iiInvType >= 3 AND iiInvType <= 4 THEN ASSIGN 
            ttInv.DueDate  = ttInv.InvDate + extratime + liDepoPTerm
            ttInv.ARAccNum = liDepoAcc.

         /* cash invoices */
         ELSE IF iiInvType >= 6 AND iiInvType <= 7 THEN
            ttInv.DueDate = ttInv.InvDate + 1.
               
         /* od invoices, first working day after invoice date */
         ELSE IF ttInv.BillRun BEGINS "OD" THEN DO:
         
            ttInv.DueDate = ttInv.InvDate.
            
            DO WHILE TRUE:
               ttInv.DueDate = ttInv.DueDate + 1.
               
               /* weekend */  
               IF WEEKDAY(ttInv.DueDate) = 7 OR WEEKDAY(ttInv.DueDate) = 1 
               THEN NEXT.
      
               /* national holiday */
               IF CAN-FIND(FIRST NatHoliday WHERE
                                 NatHoliday.Holiday = ttInv.DueDate) THEN NEXT.
               LEAVE.
            END. 
         END.
         
         /* due day is the n.th (according to payment term) working day 
            of month for normal invoices */
         ELSE DO:
            
            IF MONTH(ttInv.InvDate) = 12 THEN ASSIGN
               lcPer  = 1
               lcPer2 = YEAR(ttInv.InvDate) + 1.
            ELSE ASSIGN
               lcPer  = MONTH(ttInv.InvDate) + 1
               lcPer2 = YEAR(ttInv.InvDate).
                 
            liCnt = MIN(Customer.PaymTerm,20).
               
            DO liCnt = liCnt TO MAX(liCnt + 10,lLoop):
               ldtFrom = DATE(lcPer,liCnt,lcPer2) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN NEXT.

               /* weekend */  
               IF WEEKDAY(ldtFrom) = 7 OR WEEKDAY(ldtFrom) = 1 THEN NEXT.
      
               /* national holiday */
               IF CAN-FIND(FIRST NatHoliday WHERE
                                 NatHoliday.Holiday = ldtFrom) THEN NEXT.
               LEAVE.
            END. 
             
            IF ldtFrom = ? THEN ldtFrom = ttInv.InvDate.
               
            ttInv.DueDate = ldtFrom.
         END.    

         lLoop = 0.

         /* names and addresses */               
         IF iiInvType >= 3 AND iiInvType < 99 AND liOrderID > 0 THEN DO:
            
            /* is there a delivery address */
            FIND FIRST OrderCustomer WHERE
                       OrderCustomer.Brand   = gcBrand   AND
                       OrderCustomer.OrderID = liOrderID AND
                       OrderCustomer.RowType = 4 
            NO-LOCK NO-ERROR.
            IF AVAILABLE OrderCustomer THEN DO:
               ASSIGN 
                  ttInv.IDelName    = DYNAMIC-FUNCTION("fPrintOrderName"
                                                          IN ghFunc1,
                                                       BUFFER OrderCustomer)
                  ttInv.IDelAddr    = OrderCustomer.Address
                  ttInv.IDelZipCode = OrderCustomer.ZipCode
                  ttInv.IDelPost    = OrderCustomer.PostOffice.
                     
               IF OrderCustomer.Region > "" THEN DO:
                  FIND Region WHERE Region.Region = OrderCustomer.Region
                     NO-LOCK NO-ERROR.
                  IF AVAILABLE Region THEN 
                     ttInv.IDelPost = ttInv.IDelPost + ", " + 
                                      Region.RgName.
               END.
            END.
                     
            /* actual invoice customer */             
            FIND FIRST OrderCustomer WHERE
                       OrderCustomer.Brand   = gcBrand   AND
                       OrderCustomer.OrderID = liOrderID AND
                       OrderCustomer.RowType = Order.InvCustRole 
            NO-LOCK NO-ERROR.
            IF AVAILABLE OrderCustomer THEN DO:
               ASSIGN 
               ttInv.CustName    = DYNAMIC-FUNCTION("fPrintOrderName"
                                                       IN ghFunc1,
                                                    BUFFER OrderCustomer)
               ttInv.Address    = OrderCustomer.Address
               ttInv.PostOffice = OrderCustomer.ZipCode + " " + 
                                  OrderCustomer.PostOffice.

               IF OrderCustomer.Region > "" THEN DO:
                  FIND Region WHERE Region.Region = OrderCustomer.Region
                     NO-LOCK NO-ERROR.
                  IF AVAILABLE Region THEN 
                     ttInv.PostOffice = ttInv.PostOffice + ", " + 
                                        Region.RgName.
               END.
                                    
            END.
         END.
            
         ELSE ASSIGN 
            ttInv.COName      = Customer.COName
            ttInv.Address     = Customer.Address
            ttInv.PostOffice  = Customer.ZipCode + " " + 
                                Customer.PostOffice
            ttInv.IDelName    = Customer.IDelName
            ttInv.IDelCOName  = Customer.IDelCOName
            ttInv.IDelAddr    = Customer.IDelAddr
            ttInv.IDelZipCode = Customer.IDelZipCode
            ttInv.IDelPost    = Customer.IDelPost
            ttInv.IDelCountry = Customer.IDelCountry                        
            ttInv.FirstName   = Customer.FirstName
            ttInv.SurName2    = Customer.SurName2
            ttInv.CustName    = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                                 BUFFER Customer).

         /* check that due date is a regular day */
         ttInv.DueDate = fChkDueDate(ttInv.DueDate).
            
         /* for VAS invoice take ar-account from operator */
         IF liVASAcc > 0 THEN ttInv.ARAccNum = liVASAcc.
         
      END.
      
      ELSE FIND FIRST ttInv WHERE ttInv.InvNum = liSeq.
      
      ASSIGN lloop                   = lloop + 1
             ttInv.VatBasis[lloop]   = ttRowVat.Amount
             ttInv.VatPercent[lloop] = ttRowVat.VatPerc
             ttInv.VatAccount[lloop] = ttRowVat.VatAcc. 

      IF ttRowVat.VatPerc = 0 
      THEN ttInv.Vat0Amt = ttInv.Vat0Amt + ttRowVat.Amount.

      /* VAT included in prices */
      IF lCustVat THEN ASSIGN
         ttInv.VatAmount[lloop] = ROUND(ttRowVat.Amount * ttRowVat.VatPerc /
                                        (100 + ttRowVat.VATPerc),2)       
         ttInv.VATAmt           = ttInv.VatAmt + ttInv.VatAmount[lloop]
         ttInv.InvAmt           = ttInv.InvAmt + ttRowVat.Amount
         ttInv.AmtExclVat       = ttInv.InvAmt - ttInv.VatAMt.

      /* VAT excluded in prices */
      ELSE ASSIGN 
         ttInv.AmtExclVAT       = ttInv.AmtExclVat + ttRowVat.Amount
         ttInv.VatAmount[lloop] = ROUND(ttRowVat.Amount * 
                                        ttRowVat.VatPerc / 100,2)
         ttInv.VATAmt           = ttInv.VATAmt + ttInv.VatAmount[lloop]
         ttInv.InvAmt           = ttInv.AmtExclVAT + ttInv.VATAmt.

      /* finalize or cancel invoice */
      IF LAST-OF(ttRowVat.MsSeq) THEN DO:
         
         ASSIGN 
            /* portion of vat related sales */
            ldVatPort      = IF ttInv.Vat0Amt = 0 OR ttInv.AmtExclVat = 0
                              THEN 1
                              ELSE (ttInv.AmtExclVat - ttInv.Vat0Amt)
                                    / ttInv.AmtExclVat
            /* interest is added to gross amount */
            ttInv.InvAmt   = ttInv.InvAmt + lInterest.

         /* get customer balances */
         IF lcDepoItem = "" THEN ASSIGN 
            ldCustOP = fGetCustBal(Customer.CustNum,
                                   ttRowVat.CLI,
                                   "OP")
            ldCustAP = fGetCustBal(Customer.CustNum,
                                   ttRowVat.CLI,
                                   "AP").
         ELSE ASSIGN 
            ldCustOP = 0
            ldCustAP = 0. 

         /* IF any advance payments (advance payment includes VATAmt),
            reduce from GROSS VALUE */
         IF ldCustAP NE 0 THEN DO:
            IF ttInv.InvAmt - ldCustAP < 0 
            THEN lAdvRed = ttInv.InvAmt.
            ELSE lAdvRed = ldCustAP.
         END.
         ELSE lAdvRed = 0.

         /* also overpayment is reduced */
         IF ldCustOP NE 0 THEN DO:
            IF ttInv.InvAmt - lAdvRed - ldCustOP < 0 
            THEN lReduce = ttInv.InvAmt - lAdvRed.
            ELSE lReduce = ldCustOP.
         END.
         ELSE lReduce = 0.

         /* advance payment / overpayment */
         ASSIGN
            ttInv.OverPaym = 0 - lReduce
            ttInv.OPAccNum = IF lReduce NE 0
                             THEN OverPayAcc
                             ELSE 0
            ttInv.AdvPaym  = 0 - lAdvRed  
            ttInv.APAccNum = IF lAdvRed NE 0 
                             THEN AdvPaymAcc
                             ELSE 0
            ttInv.InvAmt   = ttInv.InvAmt - lAdvRed - lReduce
            ldAPBase       = lAdvRed. 

         /* check if there are advance payment type fat rows */
         FOR EACH ttIR WHERE
                  ttIR.MsSeq   = ttRowVat.MsSeq AND
                  ttIR.RowType = 7,
            FIRST Account NO-LOCK WHERE
                  Account.Brand  = gcBrand AND
                  Account.AccNum = ttIR.SlsAcc:
                     
            IF Account.AccType = 19 THEN 
               ldAPBase = ldAPBase - ttIR.Amt. /* negative amount -> - */
         END.
                         
         /* VAT handling FOR advance payment */
         IF ldAPBase NE 0 AND ttInv.VATAmt NE 0 THEN DO:
            ASSIGN 
               lloop                  = lloop + 1
               ttinv.VatAmount[lLoop] = IF ttinv.AmtExclVat + 
                                           ttinv.VatAmt = ldAPBase
                                        THEN ttinv.VatAmt
                                        ELSE fAPVatAmt(ldAPBase * ldVatPort).
      
            /* not more than original vat */
            IF ABS(ttInv.VatAmt) < ABS(ttInv.VatAmount[lLoop])
            THEN ttInv.VatAmount[lLoop] = ttInv.VatAmt.
      
            ASSIGN                                  
               ttInv.VatAcc[lLoop]  = liAPVatAcc
               ttInv.VatPerc[lLoop] = ldeAPVatPerc
               ttInv.VatBasis[lLoop]= IF ttInv.AmtExclVat + ttInv.VatAmt =
                                             ldAPBase
                                      THEN (IF ttInv.VatIncl
                                            THEN ttInv.AmtExclVat + 
                                                 ttInv.VatAmt - 
                                                 ttInv.Vat0Amt
                                            ELSE ttInv.AmtExclVat -
                                                 ttInv.Vat0Amt)
                                      ELSE (IF ttInv.VatIncl
                                            THEN ldAPBase * ldVatPort
                                            ELSE ldAPBase * ldVatPort - 
                                                  ttInv.VatAmount[lLoop])
               ldBasis              = ttInv.VatBasis[lLoop]
               ldVat                = ttInv.VatAmount[lLoop].

            /* reduce from normal vat amounts (vat percents are in 
               ascending order) */  
            DO liCnt = 10 TO 1 BY -1:
               IF liCnt = lLoop OR ttInv.VatAmount[liCnt] = 0 THEN NEXT.
         
               IF ldBasis >= 0 THEN ASSIGN 
                  ldRedBasis = MIN(ttInv.VatBasis[liCnt],ldBasis)
                  ldRedVat   = MIN(ttInv.VatAmount[liCnt],ldVat).
               ELSE ASSIGN 
                  ldRedBasis = MAX(ttInv.VatBasis[liCnt],ldBasis)
                  ldRedVat   = MAX(ttInv.VatAmount[liCnt],ldVat).
         
               ASSIGN
                  ttInv.VatBasis[liCnt]  = ttInv.VatBasis[liCnt] - ldRedBasis
                  ttInv.VatAmount[liCnt] = ttInv.VatAmount[liCnt] - ldRedVat
                  ldBasis                = ldBasis - ldRedBasis
                  ldVat                  = ldVat - ldRedVat.
         
            END.         
         END.
 
         /*
         case {&country}:
            when {&FIN} THEN
               /* rounding total */
               ASSIGN
                  ttInv.Rounding   = (round(ttInv.InvAmt,1)) - ttInv.InvAmt
                  ttInv.InvAmt = ttInv.InvAmt + ttInv.Rounding.
            otherwise
               /* rounding total  */
               ASSIGN
                  ttInv.Rounding   = (round(ttInv.InvAmt,2)) - ttInv.InvAmt
                  ttInv.InvAmt = ttInv.InvAmt + ttInv.Rounding.
         END.
         */

         /* amount in home currency */
         ttInv.CurrAmt = fToHomeCurr(ttInv.InvAmt,lCurRate). 

         /* if datelimit is given then check when customer was last billed */
         ASSIGN liLastBill = 9999. 

         IF liMinDays > 0 THEN DO:

            FOR EACH Invoice NO-LOCK WHERE
                     Invoice.Brand   = gcBrand AND
                     Invoice.CustNum = Customer.CustNum:
               IF pDate2 - Invoice.ToDate < liLastBill
               THEN liLastBill = pDate2 - Invoice.ToDate.
            END.

            /* check calls if customer has no invoices yet */
            IF liLastBill = 9999 THEN DO:
               FOR FIRST FixCDR NO-LOCK WHERE
                         FixCDR.InvCust = Customer.CustNum AND
                         FixCDR.Date  > pDate2 - liMinDays:
                  liLastBill = pDate2 - FixCDR.Date.
               END.

               IF liLastBill = 9999 THEN
               FOR FIRST MobCDR NO-LOCK WHERE
                         MobCDR.InvCust = Customer.CustNum AND
                         MobCDR.Date    > pDate2 - liMinDays:
                  liLastBill = pDate2 - MobCDR.Date.
               END. 
            END.    

         END. 

         /* check that there are other products besides the one that has
            been defined AS NOT Billable when alone */
         IF NOT lRejBill THEN DO:
            ASSIGN lRejBill    = TRUE
                   lcRejReason = "ProdNoBill"
                   lcRejCLI    = ttInv.CLI.
            FOR FIRST ttIr WHERE
                      ttIR.MsSeq = ttRowVat.MsSeq AND 
                      ttIr.BillCode NE lNotBilled:
               ASSIGN lRejBill    = FALSE
                      lcRejReason = ""
                      lcRejCLI    = "".
            END.
         END.

         llCreate = FALSE. 

         /* minimum limit exceeded or can be skipped */
         IF ttInv.InvAmt >= ttInvCust.MinInv OR
            ttInvCust.LowVal = TRUE        
         THEN llCreate = TRUE.

         ELSE IF 
            /* IF overpayment causes the VALUE TO decrease under limit
               THEN CREATE the invoice  */
            (ttInv.AdvPaym + ttInv.OverPaym < 0 AND
             ROUND(ttInv.InvAmt - ttInv.AdvPaym - ttInv.OverPaym,2) > 
                                            ttInvCust.MinInv)   
               OR
            /* limit for credit loss postings has been given */
            (ldCredLoss     > 0 AND 
             ttInv.AdvPaym  = 0 AND
             ttInv.OverPaym = 0 AND
             ttInv.InvAmt   > 0 AND 
             ttInv.InvAmt   < ldCredLoss)   
         THEN llCreate = TRUE. 

         /* when minimum days from last bill is given it overrides 
            other terms */
         IF liMinDays > 0 AND liLastBill < liMinDays
         THEN llCreate = FALSE. 

         if can-find(first invoice use-index cli where
                           invoice.brand = "1" and
                           invoice.cli   = ttinv.cli and
                           invoice.invdate = 12/10/7 and
                           invoice.invtype = 1)
         then llcreate = false.
                           
         IF llCreate AND NOT lRejBill AND 
            (ttInv.InvAmt > 0 OR
             (ROUND(ttInv.InvAmt,2) = 0 AND
               ((ttInv.InvType >= 6 AND ttInv.InvType <= 7) OR
                ttInv.AdvPaym + ttInv.OverPaym < 0          OR
                CAN-FIND(FIRST ttIR WHERE 
                               ttIR.MsSeq   = ttInv.MsSeq AND 
                               ttIR.RowType = 7 AND 
                               ttIR.Amt < 0)
               ) 
              )
             )
         THEN DO:

            ASSIGN lUpdAcc  = IF iiInvType < 3 OR iiInvType EQ 99
                              THEN InvGroup.UpdCustBal
                              ELSE FALSE
                   lcSeqPref = "".


            /* NEW Invoice record */
            CREATE Invoice.

            /* internal invoice nbr */ 
            REPEAT:
               Invoice.InvNum = NEXT-VALUE(IntInvNum) NO-ERROR. 
                   
               VALIDATE Invoice NO-ERROR.
                  
               /* another process has just used the same number */
               IF ERROR-STATUS:ERROR OR Invoice.InvNum = 0 THEN NEXT.
               ELSE LEAVE.
            END.

            ASSIGN 
               lInvno     = Invoice.InvNum
               liInvRecid = RECID(Invoice).

            FIND LAST newinv where
                      newinv.InvGroup = Customer.InvGroup AND
                      newinv.InvType  = iiInvType no-error.
            IF NOT AVAIL newinv 
            THEN lcExtInvID = fGetInvNum(InvGroup.InvGroup,
                                         iiInvType,
                                         idtInvDate,
                                         OUTPUT lcSeqPref).
            ELSE DO:
               lcExtInvID = fLocalNextExtID(NewInv.SeqPrefix,
                                            Newinv.ExtInvID).
               lcSeqPref  = NewInv.SeqPrefix.                              
            END.
               
            /* external invoice id */
            REPEAT:
               Invoice.ExtInvID = lcExtInvID NO-ERROR.

               /* this doesn't give error from doubles, index is not unique */
               VALIDATE Invoice NO-ERROR.
                  
               /* make sure that new field values are visible to other
                  sessions */
               FIND Invoice WHERE RECID(Invoice) = liInvRecid EXCLUSIVE-LOCK.
               
               /* another process has used the same number */
               IF ERROR-STATUS:ERROR OR Invoice.ExtInvID = ""  OR
                  CAN-FIND(FIRST bChkInv USE-INDEX ExtInvID WHERE
                                 bChkInv.Brand    = gcBrand AND 
                                 bChkInv.ExtInvID = lcExtInvID AND
                                 RECID(bChkInv) NE RECID(Invoice)) 
               THEN DO:
                  lcExtInvID = fLocalNextExtID(lcSeqPref,
                                               lcExtInvID).
                  NEXT.                             
               END.   

               LEAVE.
            END.

            /* copy TEMP-TABLE TO Invoice */
            buffer-copy ttInv except ttInv.InvNum ttInv.ExtInvID TO Invoice.

            Invoice.RefNum = Invoice.ExtInvID.
               
            /* total amount with 2 decimals */
            Invoice.InvAmt = ROUND(Invoice.InvAmt,2).

            IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhInvoice).

            /* check and mark direct debiting */
            IF Invoice.ChargeType = 2 AND Invoice.InvAmt NE 0 THEN 
            RUN Ar/nnsvte (Invoice.CustNum,
                        TODAY,
                        OUTPUT Invoice.DDBankAcc). 

            FIND FIRST Invoice where Invoice.InvNum = lInvNo
               no-lock no-error.

            FOR EACH ttIR no-lock WHERE
                     ttIR.MsSeq = ttRowVat.MsSeq:

               CREATE InvRow.
               ASSIGN
                  InvRow.InvNum    = lInvNo
                  InvRow.InvRowNum = next-value(irid).

               /* copy TEMP-TABLE TO InvRow */
               buffer-copy ttIR 
                  except ttIR.bOld ttIR.InvNum ttIR.InvRowNum TO InvRow.
            END.
                                    
            /* mark events */
            fMarkInvoiceItems(ttRowVat.MsSeq,
                              lInvNo).
                                    
            /* CCN counters to database */
            FOR EACH ttCCN WHERE ttCCN.MsSeq = ttRowVat.MsSeq:

               CREATE InvCCN.
               buffer-copy ttCCN TO InvCCN.
               ASSIGN InvCCN.InvNum = Invoice.InvNum. 
            END.

            /* CLI counters to database */
            FOR EACH ttCLI WHERE ttCLI.MsSeq = ttRowVat.MsSeq:

               CREATE InvASub.
               buffer-copy ttCLI TO InvASub.
               ASSIGN InvASub.InvNum = Invoice.InvNum.
            END.

            FOR EACH ttServiceCounter WHERE
                     ttServiceCounter.MsSeq = ttRowVat.MsSeq:
                     
               FIND ServiceLCounter WHERE
                    ServiceLCounter.MSSEq  = ttServiceCounter.MSSeq  AND
                    ServiceLCounter.Period = ttServiceCounter.Period AND
                    ServiceLCounter.SLSeq  = ttServiceCounter.SLSeq 
               EXCLUSIVE-LOCK NO-ERROR NO-WAIT.        
               
               IF AVAIL ServiceLCounter AND NOT LOCKED(ServicelCounter) 
               THEN DO:
                   ServiceLCounter.BilledAmt = 
                       ServiceLCounter.BilledAmt  + ttServiceCounter.Amount.
                   RELEASE ServiceLCounter.
               END.
            END.
               
            liLastCreated = lInvno.
                
            &IF "{&InitPersistent}" NE "NO" 
            &THEN 
            /* update monthly counters 
               (voldisc may have changed call amounts) */
            RUN pUpdateMthCounter IN fhVDHandle (Customer.CustNum). 
               
            /* update CALL counters 
               (voldisc may have changed call amounts) */
            RUN pUpdateCallCounter IN fhVDHandle (INPUT LISeq).
            &ENDIF
               
            /* list of newly created invoices */
            FIND FIRST newinv where
                       newinv.InvGroup = Customer.InvGroup AND
                       newinv.InvType  = iiInvType         AND
                       newinv.ExtInvID = Invoice.ExtInvID no-error.
            IF NOT AVAILABLE newinv THEN DO:
               CREATE newinv.
               ASSIGN 
                  newinv.InvNum     = lInvNo
                  newinv.ExtInvID   = Invoice.ExtInvID
                  newinv.InvGroup   = Customer.InvGroup
                  newinv.UpdCustBal = InvGroup.UpdCustBal
                  newinv.InvType    = iiInvType
                  newinv.SeqPrefix  = lcSeqPref
                  newinv.InvSeq     = lISeq.

            END.
            ASSIGN newinv.Qty    = Invoice.AmtExclVAT
                   newinv.VATAmt = Invoice.InvAmt
                   liIRLQty      = liIRLQty + 1
                   ldIRLAmt      = ldIRLAmt + Invoice.InvAmt.

            /* mark invoice sequence AS Billed using BUFFER */
            FIND LAST bufseq where 
                      bufseq.InvSeq  = lISeq AND
                      bufseq.Custnum = customer.custnum AND
                      bufseq.billed  = FALSE
            exclusive-lock no-error.
            IF AVAILABLE bufseq THEN ASSIGN 
               bufseq.Billed = TRUE
               bufseq.InvNum  = lInvNo.

            /* mark invoice nbr to order */
            IF liOrderID > 0 THEN 
            FOR FIRST Order EXCLUSIVE-LOCK WHERE
                      Order.Brand   = gcBrand AND
                      Order.OrderID = liOrderID:
               Order.InvNum = Invoice.InvNum.

               /* ar account for cash invoices */
               FOR FIRST CLIType NO-LOCK WHERE
                         CLIType.Brand   = gcBrand AND
                         CLIType.CLIType = Order.CLIType:
                  IF CLIType.ArAccNum > 0 THEN DO:
                     FIND CURRENT Invoice EXCLUSIVE-LOCK.
                     Invoice.ArAccNum = CLIType.ArAccNum.
                  END.   
               END.
            END.

            /* mark this period as handled for minimum consumption
               always, even if this invoice exceeded min.consumption limit */
            IF LOOKUP(STRING(Invoice.InvType),"1,99") > 0 THEN DO:
               CREATE ActionLog.
               ASSIGN 
                  ActionLog.Brand        = gcBrand   
                  ActionLog.TableName    = "MobSub"  
                  ActionLog.KeyValue     = STRING(Invoice.MsSeq) 
                  ActionLog.ActionChar   = Invoice.CLI
                  ActionLog.ActionID     = "MINCONS"
                  ActionLog.ActionPeriod = YEAR(Invoice.ToDate) * 100 + 
                                           MONTH(Invoice.ToDate)
                  ActionLog.ActionTS     = Invoice.ChgStamp
                  ActionLog.ActionDec    = Invoice.InvNum
                  ActionLog.ActionStatus = 0.
            END.                                           
               
            /* ROW FOR when invoice is created */
            fELog(katun,"INVOICE:"  + string(lInvNo) + ":Created:" +
                        "Customer:" + string(Invoice.CustNum)).

            IF lReduce NE 0 THEN DO:

               fCustBal(Customer.CustNum,
                        Invoice.CLI,
                        "OP",
                        -1 * lReduce). 

               /* CREATE optrans-log */
               CREATE OPLog.
               ASSIGN OPLog.CustNum    = Invoice.CustNum
                      OPLog.EventDate  = Invoice.InvDate
                      OPLog.UserCode   = katun
                      OPLog.EventType  = 3
                      OPLog.InvNum     = Invoice.InvNum
                      OPLog.Amt        = 0 - lReduce
                      OPLog.CreStamp   = Invoice.ChgStamp.
            END.

            /* advance payments exists */
            IF lAdvRed NE 0 THEN DO:

               fCustBal(Customer.CustNum,
                        Invoice.CLI,
                        "AP",
                        -1 * lAdvRed). 

               /* CREATE optrans-log */
               CREATE OPLog.
               ASSIGN OPLog.CustNum   = Invoice.CustNum
                      OPLog.EventDate = Invoice.InvDate
                      OPLog.UserCode  = katun
                      OPLog.EventType = 11
                      OPLog.InvNum    = Invoice.InvNum
                      OPLog.Amt       = 0 - lAdvRed
                      OPLog.CreStamp  = Invoice.ChgStamp.
            END.

            /* IF accounts are TO be updated */
            IF lUpdAcc THEN DO:

               /* a/r balance */
               fCustBal(Customer.CustNum,
                        Invoice.CLI,
                        "ARBAL",
                        Invoice.InvAmt). 

               /* unbilled balance */
               fCustCount(Customer.CustNum,
                          "UB",
                          -1 * Invoice.AmtExclVat). 
            END.

            fLatestInv(Customer.CustNum,
                       Invoice.CLI, 
                       pDate1). 


            /* interest balance */
            IF Invoice.InterestAmt NE 0 THEN 
            fCustBal(Customer.CustNum,
                     Invoice.CLI,
                     "INT",
                     -1 * Invoice.InterestAmt). 

            /* IF total VALUE is less than given limit or
               customer is a credit loss customer 
               THEN post TO credit loss  */
            IF Invoice.CustNum = liCLossCust OR 
               (ldCredLoss       > 0 AND        
                Invoice.AdvPaym  = 0 AND
                Invoice.OverPaym = 0 AND
                Invoice.InvAmt   > 0 AND 
                Invoice.InvAmt   < ldCredLoss)
            THEN DO:

               FIND CURRENT Invoice EXCLUSIVE-LOCK. 
               RUN Ar/makepaym (BUFFER Invoice,
                             Invoice.InvAmt,
                             Invoice.InvDate,
                             liCLossAcc,
                             "CL",
                             1,
                             FALSE,
                             FALSE,
                             "",
                             (IF Customer.CustNum = liCLossCust
                              THEN "Credit loss customer"
                              ELSE "Cleaning run") + 
                             " Handler: " + katun,
                             OUTPUT liVoucher).

                Invoice.PaymState = 3. 
            END. 

            /* IF customer is in "own use" category then post to own use */
            ELSE IF Customer.Category = lcOwnUse THEN DO:
                                
               FIND CURRENT Invoice EXCLUSIVE-LOCK.
               RUN Ar/makepaym (BUFFER Invoice,
                             Invoice.InvAmt,
                             Invoice.InvDate,
                             liOwnUsePA,
                             "BR",
                             0,
                             FALSE,
                             FALSE,
                             "",
                             "Own use  Handler: " + katun,
                             OUTPUT liVoucher).
            END.
                 
            /* nul current month's saldo counter */
            IF Invoice.ToDate >= TODAY THEN 
            FOR FIRST SaldoCounter EXCLUSIVE-LOCK WHERE
                      SaldoCounter.MsSeq  = Invoice.MsSeq AND
                      SaldoCounter.Period = YEAR(Invoice.ToDate) * 100 + 
                                            MONTH(Invoice.ToDate):
               SaldoCounter.Amt = 0.                             
            END.
            
            /* Showing EACH customers transactions */
            IF NOT liSilent THEN DO:
               lAmt = lSum2.
               PAUSE 0.
               DISPLAY
               lAmt
               Invoice.InvNum
               Invoice.CustNum
               Invoice.CustName
               Invoice.PostOffice
               Invoice.AmtExclVAT
               WITH FRAME LOG.
               DOWN WITH FRAME LOG.
            END.   

            ASSIGN lSum3 = lSum3 + Invoice.AmtExclVAT.
            
            IF NOT liSilent THEN 
            /* Updating the grand total on the screen */
            put screen row 18 col 40 " * Total (ex VAT)" + 
               string(lSum3,"zzzzzzzz9.99-").

            PAUSE 0.

            RELEASE Invoice.

         END. 

         /* cancel creation */   
         ELSE DO:
               
            /* calculate amount of NOT Billed Calls */
            lNotInv = lNotInv + ttInv.AmtExclVAT.

            /* unmark events */ 
            fMarkInvoiceItems(ttInv.MsSeq,0).
               
            IF lcRejReason = "" THEN lcRejReason = "ValueTooLow:" +
                                     STRING(ttInv.InvAmt).
               
            /* ROW FOR NOT created invoices */
            fELog(katun,"INVOICE:" + lcRejReason + 
                        ":Customer:" + string(ttInv.CustNum)).
            fErrorLog(ttInv.CustNum,
                      ttInv.CLI,
                      lcRejReason).

            DELETE ttInv.
         END.

      END. /* last-of ttrowvat.cli */

   END.  /* foreach ttrowvat */
   
END PROCEDURE.

/* create TEST invoices */
PROCEDURE pCreateTestInv:

   DEF INPUT  PARAMETER idtInvDate AS Date NO-UNDO.
   DEF INPUT  PARAMETER pDate1     AS Date NO-UNDO.
   DEF INPUT  PARAMETER pDate2     AS Date NO-UNDO.
   DEF INPUT  PARAMETER ciperiod   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilRerate   AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER ilDouble   AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER iiCustQty  AS INT  NO-UNDO.

   run pCreateInv (idtInvDate,
                   pDate1,
                   pDate2,
                   ciperiod,
                   extratime,
                   ilRerate,
                   ilDouble,
                   iiCustQty,
                   99,  /* force test invtype */
                   "TEST-BR").

END PROCEDURE.

/* create on demand invoices */
PROCEDURE pCreateODInv:

   DEF INPUT  PARAMETER iiMsSeq     AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idtInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idtDate1    AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idtDate2    AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiPeriod    AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiExtraTime AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilRerate    AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER ilDouble    AS LOG  NO-UNDO.
   DEF OUTPUT PARAMETER oiInvNum    AS INT  NO-UNDO.

   lcMSSeqLst = STRING(iiMsSeq).

   run pCreateInv (idtInvDate,
                   idtDate1,
                   idtDate2,
                   iiPeriod,
                   iiExtraTime,
                   ilRerate,
                   ilDouble,
                   1,
                   99, /* temporary test version, normally 1 */     
                   "OD-BR").

   /* was invoice created */
   oiInvNum = liLastCreated.
 
END PROCEDURE.



