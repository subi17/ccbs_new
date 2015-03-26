/* createpaym.p     04.09.07/aam  (separated from makepaym.p)
   create payments

   changes:       
*/   

{commali.i}
{timestamp.i}
{fvoucher.i}
{eventval.i}
{fcustbal.i}
{fclvat.i}
{fapvat.i}
{ftaxdata.i}
{fpaymconfig.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
END.


DEF INPUT  PARAMETER iiCustNum   AS INT  NO-UNDO.  /* customer    */
DEF INPUT  PARAMETER iiInvNum    AS INT  NO-UNDO.  /* invoice     */
DEF INPUT  PARAMETER icCLI       AS CHAR NO-UNDO.  /* subscription */
DEF INPUT  PARAMETER idtAccDate  AS DATE NO-UNDO.  /* posting (acc.) date */
DEF INPUT  PARAMETER idtPaymDate AS DATE NO-UNDO.  /* payment date */
DEF INPUT  PARAMETER idPosting   AS DEC  NO-UNDO EXTENT 10.  /* amounts */
DEF INPUT  PARAMETER iiAccount   AS INT  NO-UNDO EXTENT 10.  /* accounts */
DEF INPUT  PARAMETER icSource    AS CHAR NO-UNDO.  /* payment source */
DEF INPUT  PARAMETER iiPaymType  AS INT  NO-UNDO.  /* payment type */
DEF INPUT  PARAMETER icReference AS CHAR NO-UNDO.  /* reference (link) */
DEF INPUT  PARAMETER icMemo      AS CHAR NO-UNDO.  /* Memo text */
DEF OUTPUT PARAMETER oiVoucher   AS INT  NO-UNDO. 

DEF VAR liCount      AS INT   NO-UNDO.
DEF VAR lcTitle      AS CHAR  NO-UNDO. 
DEF VAR liAccCnt     AS INT   NO-UNDO.
DEF VAR liPaymInt    AS INT   NO-UNDO. 
DEF VAR lcPref       AS CHAR  NO-UNDO.
DEF VAR lcExtVoucher AS CHAR  NO-UNDO.
DEF VAR lrRecid      AS RECID NO-UNDO. 
DEF VAR lcTaxZone    AS CHAR  NO-UNDO.

DEF BUFFER bPaidInv FOR Invoice.


IF idtAccDate = ? THEN idtAccDate = TODAY.
IF idtPaymDate = ? THEN idtPaymDate = idtAccDate.

FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN "ERROR:Unknown customer".

IF iiInvNum > 0 THEN DO:
   FIND bPaidInv WHERE bPaidInv.InvNum = iiInvNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bPaidInv THEN RETURN "ERROR:Unknown invoice".
END.

/* invoice is mandatory for credit loss posting */
ELSE IF iiPaymType = 1 THEN DO:
   RETURN "ERROR:Invoice not available for credit loss posting".
END.    

IF llDoEvent THEN DO:
   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).
END.

/* if accounts have not been given then try to get defaults */
IF iiAccount[1] = 0 AND iiAccount[2] = 0 THEN DO:
   
   lcTaxZone = fRegionTaxZone(IF iiInvNum > 0 AND bPaidInv.Region > ""
                              THEN bPaidInv.Region
                              ELSE Customer.Region).
                              
   fGetPaymentAccounts(iiPaymType,
                       icSource,
                       idtAccDate,
                       lcTaxZone,
                       OUTPUT iiAccount).
END.


CREATE Payment.

/* internal voucher nbr */ 
REPEAT:

   Payment.Voucher = fGetIntVoucher() NO-ERROR.

   VALIDATE Payment NO-ERROR.
                  
   /* another process has just used the same number */
   IF ERROR-STATUS:ERROR OR Payment.Voucher = 0 THEN NEXT.
   ELSE LEAVE.
END.

ASSIGN Payment.Brand      = gcBrand
       Payment.CustNum    = iiCustNum
       Payment.PaymAmt    = idPosting[1]
       Payment.TotAmt     = idPosting[1]
       Payment.Discount   = 0
       Payment.PaymDate   = idtPaymDate
       Payment.AccDate    = idtAccDate
       Payment.PaymSrc    = icSource
       Payment.PaymType   = iiPaymType 
       Payment.RefNum     = icReference.

DO liCount = 1 TO 10:
   ASSIGN 
      Payment.Posting[liCount] = idPosting[liCount]
      Payment.AccNum[liCount]  = iiAccount[liCount].
   
   IF Payment.Posting[liCount] NE 0 THEN liAccCnt = liCount.
END.      

IF iiInvNum > 0 THEN ASSIGN 
   Payment.CustName = bPaidInv.CustName
   Payment.InvNum   = bPaidInv.InvNum
   Payment.ExtInvID = bPaidInv.ExtInvID
   Payment.InvAmt   = bPaidInv.InvAmt
   Payment.InvDate  = bPaidInv.InvDate
   Payment.DueDate  = bPaidInv.DueDate.
   
ELSE
   Payment.CustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                       BUFFER Customer).

/* created stamp */
Payment.ImportStamp = fMakeTS().

lcPref = "".

DO liCount = 1 TO liAccCnt:

   FIND Account where 
        Account.Brand  = gcBrand AND 
        Account.AccNum = Payment.AccNum[liCount]
   NO-LOCK NO-ERROR.
   IF AVAILABLE Account THEN 
      Payment.AccType[liCount] = Account.AccType.

   /* vat already posted */
   IF Payment.AccType[liCount] = 5 THEN lcPref = "v".
END.

/* separate loop so that value of lcPref is known */
DO liCount = 1 TO liAccCnt:

   /* if credit loss then vat must be handled (unless was ready in postings) */
   IF Payment.AccType[liCount] = 18 AND iiInvNum > 0 AND lcPref = "" THEN DO:

      ASSIGN   
         liAccCnt                  = liAccCnt + 1   
         Payment.Posting[liAccCnt] = fCLVat(Payment.Posting[liCount],
                                            BUFFER bPaidInv,
                                            OUTPUT liPaymInt).

      /* was vat retrieved */
      IF Payment.Posting[liAccCnt] = 0
      THEN liAccCnt = liAccCnt - 1.
      ELSE ASSIGN 
         Payment.AccNum[liAccCnt]  = liPaymInt
         Payment.AccType[liAccCnt] = 5
         Payment.Posting[liCount]  = Payment.Posting[liCount] - 
                                     Payment.Posting[liAccCnt].
   END. 

   /* if adv.payment then vat must be handled (unless was in postings) */
   ELSE IF Payment.AccType[liCount] = 19 AND lcPref = "" THEN DO:

      IF ((iiInvNum > 0 AND bPaidInv.VATUsage < 3) OR
          (iiInvNum = 0 AND Customer.VATUsage < 3))
      THEN ASSIGN 
         liAccCnt                  = liAccCnt + 1
         Payment.Posting[liAccCnt] = fAPVATAmt(Customer.Region,
                                               -1 * Payment.Posting[liCount],
                                               Payment.AccDate)
         Payment.AccNum[liAccCnt]  = liAPVatAcc
         Payment.AccType[liAccCnt] = 5
        
         liAccCnt                  = liAccCnt + 1
         Payment.Posting[liAccCnt] = -1 * Payment.Posting[liAccCnt - 1]
         Payment.AccNum[liAccCnt]  = liAPVatDebtAcc
         Payment.AccType[liAccCnt] = 5. 
   END.

   lcTitle = "".
   
   CASE Payment.AccType[liCount]:
   WHEN 6  THEN ASSIGN 
      lcTitle   = "OP"   
      liPaymInt = 1.
   WHEN 7  THEN ASSIGN 
      lcTitle   = "DP"
      liPaymInt = 13.
   WHEN 19 THEN ASSIGN 
      lcTitle   = "AP"
      liPaymInt = 10.
   WHEN 22 THEN ASSIGN 
      lcTitle   = "REF"
      liPaymInt = 27.
   END CASE.
   
   IF lcTitle > "" THEN DO:

      /* customer balance */
      fCustBal(Payment.CustNum,
               "",  
               lcTitle,
               -1 * Payment.Posting[liCount]).

      CREATE OPLog.
      ASSIGN OPLog.CustNum   = Payment.CustNum
             OPLog.EventDate = Payment.AccDate
             OPLog.UserCode  = katun
             OPLog.InvNum    = Payment.InvNum
             OPLog.Voucher   = Payment.Voucher
             OPLog.Amt       = -1 * Payment.Posting[liCount]
             OPLog.CreStamp  = Payment.ImportStamp
             OPLog.EventType = liPaymInt.
   END.
   
END.

IF icMemo > "" THEN DO:
   
   /* title for Memo (first word) */
   liCount = INDEX(icMemo,CHR(32)).
   IF liCount < 2 THEN liCount = 21.
   lcTitle = SUBSTRING(icMemo,1,liCount - 1).

   /* separate Memo */
   CREATE Memo.
   ASSIGN Memo.Brand     = gcBrand
          Memo.HostTable = "Payment"
          Memo.KeyValue  = STRING(Payment.Voucher)
          Memo.CustNum   = Payment.CustNum
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = lcTitle
          Memo.MemoText  = icMemo
          Memo.CreStamp  = Payment.ImportStamp.
   
END.

ASSIGN
   lrRecid = RECID(Payment)
   liCount = 0.

REPEAT:
   /* external voucher id */
   Payment.ExtVoucher = fGetAndUpdExtVoucher(Customer.InvGroup,
                                             Payment.PaymType,
                                             Payment.AccDate,
                                             OUTPUT lcPref).

   IF Payment.ExtVoucher = "" THEN DO:
      liCount = 11000.
      LEAVE.
   END.
 
   /* this makes sure that field values of this buffer are visible to other 
      sessions, already before transaction ends or release is done */
   FIND FIRST Payment WHERE RECID(Payment) = lrRecid NO-LOCK.
   FIND CURRENT Payment EXCLUSIVE-LOCK.
   
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
 
IF liCount > 10000 THEN UNDO, RETURN "ERROR:ExtVoucher unavailable".

IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPayment).    

oiVoucher = Payment.Voucher.

RELEASE Payment. 

/* clean eventlog objects */
fCleanEventObjects().

RETURN "".


