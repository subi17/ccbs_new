/* ---------------------------------------------------------------------------
  MODULE .......: ADVPAYM
  FUNCTION .....: create an advance payment to customer
  APPLICATION ..: TMS
  CREATED ......: 02.01.06/aam 
  MODIFIED .....: 24.01.06/jt DYNAMIC-FUNCTION("fDispCustName"
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fcustbal.i}
{Func/timestamp.i}
{Func/fapvat.i}
{Func/fvoucher.i}
{Syst/eventval.i}

DEF INPUT  PARAMETER iiCustNum   AS INT  NO-UNDO.
DEF INPUT  PARAMETER idAmount    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER idtPaymDate AS DATE NO-UNDO. 
DEF INPUT  PARAMETER iiFromAcc   AS INT  NO-UNDO.  /* 'money' account */
DEF INPUT  PARAMETER iiEventType AS INT  NO-UNDO.  /* type for oplog */
DEF INPUT  PARAMETER icMemo      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiVoucher   AS INT  NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).

   DEFINE VARIABLE lhMemo AS HANDLE NO-UNDO.
   lhMemo = BUFFER Memo:HANDLE.
   RUN StarEventInitialize(lhMemo).

END.

DEF VAR liAPAccNum    AS INT  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO.
DEF VAR ldCurrStamp   AS DEC  NO-UNDO.
DEF VAR lcTitle       AS CHAR NO-UNDO. 

liAPAccNum = fCParamI("AdvPaymAcc").
FIND Account WHERE 
     Account.Brand  = gcBrand AND
     Account.AccNum = liAPAccNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Account THEN DO:
   RETURN "Unknown AP account".
END. 

IF Account.AccType NE 19 THEN DO:
   RETURN "Invalid AP account".
END.

IF NOT CAN-FIND(Account WHERE 
                Account.Brand  = gcBrand AND
                Account.AccNum = iiFromAcc)
THEN DO:
   RETURN "Unknown transfer account".
END. 

FIND Customer NO-LOCK WHERE 
     Customer.CustNum = iiCustNum NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   RETURN "Unknown customer".
END.

IF idtPaymDate = ? THEN idtPaymDate = TODAY.
IF iiEventType = 0 THEN iiEventType = 10.


/* Get the voucher no. */
oiVoucher = fGetIntVoucher().

ldCurrStamp = fMakeTS().

DO TRANS: 

   CREATE Payment.
   /* payment */
   ASSIGN  Payment.Brand       = gcBrand
           Payment.Voucher     = oiVoucher
           Payment.CustNum     = Customer.CustNum
           Payment.InvNum      = 0
           Payment.PaymAmt     = idAmount
           Payment.TotAmt      = idAmount
           Payment.PaymDate    = idtPaymDate
           Payment.AccDate     = idtPaymDate
           Payment.PaymSrc     = "AT"
           Payment.PaymType    = 4 
           Payment.ImportStamp = ldCurrStamp

           /* debit posting */
           Payment.AccNum[1]   = iiFromAcc
           Payment.Posting[1]  = idAmount
           /* ap posting */
           Payment.AccNum[2]   = liAPAccNum
           Payment.Posting[2]  = -1 * idAmount.
 
   Payment.CustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                               BUFFER Customer).
 
   /* vat */
   IF Customer.VATUsage < 3 THEN ASSIGN 
      Payment.Posting[3] = fAPVatAmt(Customer.Region,
                                     -1 * Payment.Posting[2],
                                     idtPaymDate)
      Payment.AccNum[3]  = liAPVatAcc

      Payment.Posting[4] = -1 * Payment.Posting[3]
      Payment.AccNum[4]  = liAPVatDebtAcc.
   
   /* account types */
   DO liCount = 1 TO 4:
      IF Payment.AccNum[liCount] = 0 THEN NEXT. 
      FIND Account where 
           Account.Brand  = gcBrand AND 
           Account.AccNum = Payment.AccNum[liCount]
      NO-LOCK NO-ERROR.
      IF AVAILABLE Account THEN 
         Payment.AccType[liCount] = Account.AccType.
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
             Memo.CreStamp  = ldCurrStamp.
   
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMemo).
   END.

   IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPayment).    

   RELEASE Payment. 

   /* add AP to subscriber customer */
   fCustBal(Customer.CustNum,
            "",
            "AP",
            idAmount).
            
   CREATE OPLog.
   ASSIGN OPLog.CustNum   = Customer.CustNum
          OPLog.EventDate = idtPaymDate
          OPLog.UserCode  = katun
          OPLog.EventType = iiEventType      
          OPLog.InvNum    = 0
          OPLog.Voucher   = oiVoucher
          OPLog.Amt       = idAmount
          OPLog.CreStamp  = ldCurrStamp.

END.

/* clean eventlog objects */
fCleanEventObjects().

