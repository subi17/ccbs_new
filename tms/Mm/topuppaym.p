/* topuppaym.p      30.11.06/aam (from makepaym.p)
   create topup payments

   changes:         12.01.07/aam CLI to fCustBal
                    25.01.07/aam use fGetAndUpdExtVoucher to update
                                 Payment.ExtVoucher
                    27.03.07/aam don't use adv.payment account for deductions
                    29.03.07/aam account for compensation,
                                 use original accounts for adjustments
                    01.06.07/aam new request type; minimum consumption
                    28.06.07/aam new source "MCNOC"
*/   

{commali.i}
{cparam2.i}
{timestamp.i}
{fvoucher.i}
{eventval.i}
{fcustbal.i}
{fpaymconfig.i}
{msbalance.i}

DEF INPUT  PARAMETER iiRequest  AS INT  NO-UNDO.  /* prepaid request id */
DEF INPUT  PARAMETER icCLI      AS CHAR NO-UNDO.  /* msisdn */
DEF INPUT  PARAMETER idAmount   AS DEC  NO-UNDO.  /* paid amount vat0 */
DEF INPUT  PARAMETER idVatAmt   AS DEC  NO-UNDO.  /* vat amount */
DEF INPUT  PARAMETER idtDate    AS DATE NO-UNDO.  /* acc.& paym. date */
DEF INPUT  PARAMETER icSource   AS CHAR NO-UNDO.  /* payment source */
DEF OUTPUT PARAMETER oiVoucher  AS INT  NO-UNDO.  /* created voucher */

DEF VAR liCount         AS INT  NO-UNDO.
DEF VAR liBankAcc       AS INT  NO-UNDO.
DEF VAR liVatAcc        AS INT  NO-UNDO.
DEF VAR liTopUpAcc      AS INT  NO-UNDO.
DEF VAR liExtraAcc      AS INT  NO-UNDO.
DEF VAR liAdvAcc        AS INT  NO-UNDO. 
DEF VAR lcMemo          AS CHAR NO-UNDO. 
DEF VAR liAction        AS INT  NO-UNDO.  /* 1=add,2=reduce (cancel) */
DEF VAR lcPref          AS CHAR NO-UNDO. 
DEF VAR ldVatPerc       AS DEC  NO-UNDO. 
DEF VAR lcInvGroup      AS CHAR NO-UNDO.
DEF VAR lcTaxZone       AS CHAR NO-UNDO.
DEF VAR lcSource        AS CHAR NO-UNDO.
DEF VAR lcExtVoucher    AS CHAR NO-UNDO.
DEF VAR lrRecid         AS RECID NO-UNDO.
DEF VAR lcPaymSrc       AS CHAR  NO-UNDO.
DEF VAR liPaymType      AS INT   NO-UNDO.
DEF VAR liAccount       AS INT   NO-UNDO EXTENT 10.
DEF VAR llMinus         AS LOG   NO-UNDO.
DEF VAR liMsSeq         AS INT   NO-UNDO.
DEF VAR lcBalType       AS CHAR  NO-UNDO.
DEF VAR liUserCust      AS INT   NO-UNDO.
DEF VAR lcCLIType       AS CHAR  NO-UNDO.

DEF BUFFER bOrigReq FOR PrePaidRequest.


FUNCTION fCreateOPLog RETURNS LOGICAL
    (iiType AS INT,
     idAmt  AS DEC).

   /* creating OPLog */
   IF idAmt NE 0 THEN DO:
      CREATE OPLog.
      ASSIGN
         OPLog.CreStamp  = fMakeTS()
         OPLog.CustNum   = Customer.CustNum
         OPLog.EventDate = Payment.PaymDate
         OPLog.UserCode  = katun
         OPLog.EventType = iiType 
         OPLog.InvNum    = Payment.InvNum
         OPLog.Voucher   = Payment.Voucher
         OPLog.Amt       = idAmt. 
   END.       
   
END FUNCTION.


IF idtDate = ? THEN idtDate = TODAY.

IF idAmount = 0 THEN DO:
   oiVoucher = 1.   /* let caller mark itself as handled */
   RETURN "ERROR:NOTHING TO DO".
END.

FIND MobSub WHERE MobSub.CLI = icCLI NO-LOCK NO-ERROR.
IF AVAILABLE MobSub THEN DO:
   FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK NO-ERROR.
   ASSIGN
      liMsSeq    = MobSub.MsSeq
      liUserCust = MobSub.CustNum
      lcCLIType  = MobSub.CLIType.
END.
ELSE DO:
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.Brand = gcBrand AND
              MsOwner.CLI   = icCLI NO-ERROR.
   IF AVAILABLE MsOwner THEN 
      FIND Customer WHERE Customer.CustNum = MsOwner.InvCust NO-LOCK NO-ERROR.
   ELSE DO:
      oiVoucher = -1.
      RETURN "ERROR:Unknown CLI".
   END.
 
   ASSIGN
      liMsSeq    = MsOwner.MsSeq
      liUserCust = MsOwner.CustNum
      lcCLIType  = MsOwner.CLIType.
END.

IF NOT AVAILABLE Customer THEN DO:
   oiVoucher = -1.
   RETURN "ERROR:Unknown customer".
END.   

lcSource = ENTRY(1,icSource,":").

FIND PrePaidRequest WHERE
     PrePaidRequest.Brand     = gcBrand AND
     PrePaidRequest.PPRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE PrePaidRequest OR iiRequest = ? THEN DO:
   IF lcSource NE "MCNOC" THEN DO:
      oiVoucher = -1.
      RETURN "ERROR:Unknown request".
   END.
END.
ELSE liMsSeq = PrePaidRequest.MsSeq.

ASSIGN lcInvGroup = Customer.InvGroup
       ldVatPerc  = INTEGER(100 * idVatAmt / idAmount).
        
/* get correct invgroup through taxzone for atm events */
IF lcSource = "ATM" THEN DO:
   FOR FIRST InvGroup NO-LOCK WHERE
             InvGroup.Brand   = gcBrand AND
             InvGroup.TaxZone = PrepaidRequest.TaxZone:
      ASSIGN lcInvGroup = InvGroup.InvGroup
             lcTaxZone  = InvGroup.TaxZone.
   END.
END.
        
IF lcTaxZone = "" THEN DO:
   FIND Region WHERE Region.Region = Customer.Region NO-LOCK NO-ERROR.
   IF AVAILABLE Region THEN lcTaxZone = Region.TaxZone.
END.
 
ASSIGN
   lcPaymSrc  = lcSource
   liPaymType = 7
   llMinus    = FALSE.


CASE lcSource:

/* orders; initial topup, campaign, commission */
WHEN "web order" THEN DO:

   liPaymType = 9.
   
   CASE PrePaidRequest.PPReqPrefix:
   /* campaign */
   WHEN "994" OR WHEN "991" THEN ASSIGN
      lcPaymSrc = "PRCAMP"
      lcBalType = "CAM".
   /* initial */   
   OTHERWISE ASSIGN
      lcPaymSrc = "IT"
      lcBalType = "IT".
   END CASE.
   
END.

/* commission */
WHEN "COMM" THEN DO:
   ASSIGN
      liPaymType = 10
      lcPaymSrc  = "COMM"
      lcBalType  = "CMM".
END.

/* charge */
WHEN "CHARGE" THEN DO:
   
   ASSIGN
      liPaymType = 13
      lcPaymSrc  = "CHARGE"
      lcBalType  = "".
END.

/* customer care or script */
WHEN "CC" OR WHEN "MANFIX" OR  WHEN "COMP" THEN DO:

   liPaymType = 10.
   
   /* compensation */
   IF PrePaidRequest.TopUpAmt >= 0 THEN DO:
      IF lcSource = "CC" OR  lcSource = "COMP" THEN ASSIGN
         lcPaymSrc = "COMP"
         lcBalType = "COM".
   END.   
   
   /* minus adjustment */
   ELSE DO:
   
      ASSIGN 
         lcPaymSrc = "PUMA"
         lcBalType = "COM".
         llMinus   = TRUE.
      
      IF PrePaidRequest.OrigRequest > 0 THEN 
      FOR FIRST bOrigReq NO-LOCK WHERE
                bOrigReq.Brand     = gcBrand AND
                bOrigReq.PPRequest = PrePaidRequest.OrigRequest:

         CASE bOrigReq.Source:
         
         WHEN "web order" THEN DO:
            ASSIGN 
               liPaymType = 9
               lcBalType = "IT".
         
            IF bOrigReq.PPReqPrefix = "994" THEN ASSIGN
               lcPaymSrc = "MACAMP"
               lcBalType = "CAM".
         END.
       
         WHEN "ATM" THEN ASSIGN
            liPaymType = 7
            lcBalType = "TOP".
         
         WHEN "MANFIX" THEN ASSIGN 
            lcPaymSrc = "MAMANFIX"
            lcBalType = "TOP".
            
         END CASE.
         
      END.
      
   END.
   
END.   

/* minimum consumption */
WHEN "MINCONS" OR WHEN "MCNOC" THEN DO:
   ASSIGN 
      liPaymType = 12
      lcPaymSrc  = lcSource.
      
   /* uncharged, no affect on balances  */
   IF lcSource = "MCNOC" THEN lcBalType = "".   

   ELSE DO:
      IF NUM-ENTRIES(icSource,":") > 1 THEN 
         lcBalType = ENTRY(2,icSource,":").
      ELSE lcBalType = "TOP".
   END.
END. 

/* Recharge & minus adjustment from ATM */
WHEN "ATM" THEN DO:

   lcBalType = "TOP".
    
   IF PrePaidRequest.TopupAmt < 0 THEN ASSIGN 
      lcPaymSrc = "PAMA"
      llMinus   = TRUE.
   ELSE 
      lcPaymSrc = lcSource.
END.      

END CASE.

/* minus adjustment */
IF llMinus THEN DO:

   /* try to get original accounts */
   IF PrePaidRequest.OrigRequest > 0 THEN 
   FOR FIRST Payment NO-LOCK WHERE
             Payment.Brand   = gcBrand AND
             Payment.CustNum = Customer.CustNum AND
             Payment.RefNum  = STRING(PrePaidRequest.OrigRequest):
      
      DO liCount = 1 TO 5:
         IF LOOKUP(STRING(Payment.AccType[liCount]),"4,13,21,23,24") > 0 THEN 
            liBankAcc = Payment.AccNum[liCount].
         ELSE IF Payment.AccType[liCount] = 5 THEN
            liVatAcc = Payment.AccNum[liCount].
         ELSE IF Payment.AccType[liCount] = 20 THEN 
            liTopUpAcc = Payment.AccNum[liCount].
      END.
   END.
   
END.

/* get default accounts according to payment type and source */
IF liBankAcc = 0 AND liTopUpAcc = 0 AND liVatAcc = 0 THEN DO:

   fGetPaymentAccounts(liPaymType,
                       lcPaymSrc,
                       idtDate,
                       lcTaxZone,
                       OUTPUT liAccount).

   /* posting rules have been configured so, that debit account should always
      be used for positive amounts */
   IF idAmount >= 0 THEN ASSIGN 
      liBankAcc  = liAccount[1]
      liTopUpAcc = liAccount[2].
   ELSE ASSIGN
      liBankAcc  = liAccount[2]
      liTopUpAcc = liAccount[1].

   liVatAcc = liAccount[3].
      
   IF liVatAcc = 0 AND idVatAmt NE 0 THEN 
      liVatAcc = fCParamI("TopUpVat" + lcTaxZone).

   IF liBankAcc = 0 THEN DO:
      CASE lcPaymSrc:
      WHEN "MINCONS" THEN liBankAcc = fCParamI("TopUpMcBank").
      WHEN "MCNOC"   THEN liBankAcc = fCParamI("TopUpMcBank").
      OTHERWISE           liBankAcc = fCParamI("TopUpBank").
      END CASE. 
   END.
   
   IF liTopUpAcc = 0 THEN DO:
      CASE lcPaymSrc:
      WHEN "MINCONS" THEN liTopUpAcc = fCParamI("TopUpMinCons").
      WHEN "MCNOC"   THEN liTopUpAcc = fCParamI("TopUpMCNotCharged").
      OTHERWISE           liTopUpAcc = fCParamI("TopUpAcc").
      END CASE.
   END.
   
END.   

lcMemo = "CLI: " + icCLI + 
         IF iiRequest NE ? 
         THEN ", request " + STRING(iiRequest)
         ELSE "Not charged".


CREATE Payment.

/* internal voucher nbr */ 
REPEAT:

   Payment.Voucher = fGetIntVoucher() NO-ERROR.

   VALIDATE Payment NO-ERROR.

   /* another process has just used the same number */
   IF ERROR-STATUS:ERROR OR Payment.Voucher = 0 THEN NEXT.
   ELSE LEAVE.
END.

ASSIGN Payment.Brand    = gcBrand
       Payment.CustNum  = Customer.CustNum
       Payment.CustName = Customer.CustName + " " + Customer.FirstName
       Payment.InvNum   = 0
       Payment.InvAmt   = 0
       Payment.PaymAmt  = idAmount + idVatAmt 
       Payment.TotAmt   = idAmount + idVatAmt 
       Payment.Discount = 0
       Payment.InvDate  = ?
       Payment.DueDate  = ?
       Payment.PaymDate = idtDate
       Payment.AccDate  = idtDate
       Payment.PaymSrc  = lcPaymSrc
       Payment.PaymType = liPaymType 
       Payment.RefNum   = IF iiRequest NE ?
                          THEN STRING(iiRequest)
                          ELSE icCLI.
       
       
ASSIGN 
   Payment.AccNum[1]  = liBankAcc
   Payment.Posting[1] = idAmount + idVatAmt

   Payment.AccNum[2]  = liTopUpAcc
   Payment.Posting[2] = -1 * idAmount.
        
IF idVatAmt NE 0 THEN ASSIGN 
   Payment.AccNum[3]  = liVatAcc
   Payment.Posting[3] = -1 * idVatAmt.
 
/* time when posted */
Payment.ImportStamp = fMakeTS().

/* account types */
DO liCount = 1 TO 3:
   IF Payment.AccNum[liCount] = 0 THEN NEXT. 

   FIND Account where 
        Account.Brand  = gcBrand AND 
        Account.AccNum = Payment.AccNum[liCount]
      NO-LOCK NO-ERROR.
   IF AVAILABLE Account THEN ASSIGN 
      Payment.AccType[liCount] = Account.AccType.
END.

/* prepaid balance update */
IF lcBalType > "" THEN DO:

   fUpdateMsBalance(liMsSeq,
                    Customer.CustNum,
                    lcBalType,
                    idAmount).

END.
                 
IF lcMemo > "" THEN DO:
   
   /* separate Memo */
   CREATE Memo.
   ASSIGN Memo.Brand     = gcBrand
          Memo.HostTable = "payment"
          Memo.KeyValue  = STRING(Payment.Voucher)
          Memo.CustNum   = Payment.CustNum
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = "TOPUP Payment"
          Memo.MemoText  = lcMemo.
          Memo.CreStamp  = fMakeTS().
   
END.


IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).

   RUN StarEventMakeCreateEvent(lhPayment).    
END.

ASSIGN
   lrRecid = RECID(Payment)
   liCount = 0.

REPEAT:

   /* external voucher id */
   Payment.ExtVoucher = fGetAndUpdExtVoucher(lcInvGroup,
                                             Payment.PaymType,
                                             Payment.AccDate,
                                             OUTPUT lcPref).

   IF Payment.ExtVoucher = "" THEN DO:
      liCount = 11000.
      LEAVE.
   END.
   
   /* this makes sure that field values of this buffer are visible to other 
      sessions, already before transaction ends or release is done */
   FIND FIRST Payment WHERE RECID(Payment) = lrRecid EXCLUSIVE-LOCK.
   
   ASSIGN
      lcExtVoucher = Payment.ExtVoucher
      liCount      = liCount + 1.
  
   IF liCount > 10000 THEN LEAVE.

   IF NOT CAN-FIND(FIRST Payment WHERE
                         Payment.Brand      = gcBrand AND
                         Payment.ExtVoucher = lcExtVoucher AND
                         RECID(Payment) NE lrRecid)
   THEN LEAVE.

END.

IF liCount > 10000 THEN DO:
   oiVoucher = -1.
   fCleanEventObjects().
   UNDO, RETURN "ERROR:ExtVoucher unavailable".
END.

oiVoucher = Payment.Voucher.
   
RELEASE Payment. 

/* to counters */
CREATE TMQueue.
ASSIGN 
   TMQueue.MsSeq    = liMsSeq
   TMQueue.CustNum  = liUserCust
   TMQueue.InvCust  = Customer.CustNum
   TMQueue.DateSt   = TODAY
   TMQueue.BillCode = "TopupEvent"
   TMQueue.Amount   = idAmount
   TMQueue.Qty      = -1
   TMQueue.CLIType  = lcCLIType
   TMQueue.PayType  = 2.
RELEASE TMQueue.
   
/* clean eventlog objects */
fCleanEventObjects().


