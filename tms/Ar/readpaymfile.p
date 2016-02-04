/* ----------------------------------------------------------------------
  MODULE .......: readpaymfile.p
  TASK .........: Read payments from files to invoices 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.04.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

SESSION:NUMERIC-FORMAT = "EUROPEAN".

&GLOBAL-DEFINE AllIncludes YES

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/finvpayment.i}
{Func/finvbal.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}
END.


DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icLogFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiErrors    AS INT  NO-UNDO.

DEF STREAM sRead.
DEF STREAM sLog.

DEF VAR liDone      AS INT  NO-UNDO.
DEF VAR liVoucher   AS INT  NO-UNDO.
DEF VAR lcLine      AS CHAR NO-UNDO.
DEF VAR lcCLI       AS CHAR NO-UNDO.
DEF VAR lcExtInvID  AS CHAR NO-UNDO.
DEF VAR liCustNum   AS INT  NO-UNDO.
DEF VAR lcCustID    AS CHAR NO-UNDO.
DEF VAR ldPaidAmt   AS DEC  NO-UNDO.
DEF VAR ldInvAmt    AS DEC  NO-UNDO.
DEF VAR lcPaymDate  AS CHAR NO-UNDO.
DEF VAR lcPaymSrc   AS CHAR NO-UNDO.
DEF VAR liDebitAcc  AS INT  NO-UNDO.
DEF VAR liCreditAcc AS INT  NO-UNDO.
DEF VAR ldtAccDate  AS DATE NO-UNDO.
DEF VAR ldtPaymDate AS DATE NO-UNDO.
DEF VAR lcSep       AS CHAR NO-UNDO. 
DEF VAR ldInvBal    AS DEC  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR llCustBal   AS LOG  NO-UNDO. 
DEF VAR ldPosting   AS DEC  NO-UNDO EXTENT 10.
DEF VAR liAccount   AS DEC  NO-UNDO EXTENT 10.


DEF BUFFER bInv FOR Invoice.


FUNCTION fWriteLog RETURNS LOGIC
   (icMessage AS CHAR):
   
   PUT STREAM sLog UNFORMATTED
      lcCLI       "|"
      liCustNum   "|"
      lcExtInvID  "|"
      ldPaidAmt   "|" 
      icMessage
      SKIP.
   
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fWriteLog("ERROR: " + icMessage).
    
   oiErrors = oiErrors + 1.
END.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

/* file without the dir */
lcPlainFile = icFile.
IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
   lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").

ASSIGN
   lcSep      = CHR(9)
   ldtAccDate = TODAY.

IF llDoEvent THEN DO:
   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).
END.

  
INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(icLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
  "MSISDN"    "|"
  "Customer"  "|"
  "Invoice"   "|"
  "Paid Amt"  "|"
  "Action"   SKIP.
  
REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.


   ASSIGN 
      lcCLI       = ENTRY(1,lcLine,lcSep)
      liCustNum   = INTEGER(ENTRY(2,lcLine,lcSep))
      lcCustID    = ENTRY(3,lcLine,lcSep)
      lcExtInvID  = ENTRY(4,lcLine,lcSep)
      ldInvAmt    = DECIMAL(ENTRY(5,lcLine,lcSep))
      ldPaidAmt   = DECIMAL(ENTRY(6,lcLine,lcSep))
      lcPaymDate  = ENTRY(7,lcLine,lcSep)
      lcPaymSrc   = ENTRY(8,lcLine,lcSep)
      liDebitAcc  = INTEGER(ENTRY(9,lcLine,lcSep))
      liCreditAcc = INTEGER(ENTRY(10,lcLine,lcSep))
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR OR lcExtInvID = "" THEN NEXT.

   oiRead = oiRead + 1.
   
   ldtPaymDate = ?.
   ldtPaymDate = DATE(lcPaymDate) NO-ERROR.
   IF ldtPaymDate = ? THEN ldtPaymDate = TODAY.
   
   FIND Invoice WHERE
        Invoice.Brand    = gcBrand AND
        Invoice.ExtInvID = lcExtInvID NO-LOCK NO-ERROR.
        
   IF NOT AVAILABLE Invoice THEN DO:
      fError("Unknown invoice").
      NEXT.
   END.
   
   IF Invoice.CustNum NE liCustNum THEN DO:
      fError("Invalid customer number").
      NEXT.
   END.

   IF Invoice.InvAmt NE ldInvAmt THEN DO:
      fError("Invalid invoice amount").
      NEXT.
   END.

   ldInvBal = fInvBal(BUFFER Invoice,
                      TODAY).
                      
   IF ldPaidAmt > ldInvBal THEN DO:
      fError("Overpayment").
      NEXT.       
   END.
   
   IF liCreditAcc NE Invoice.ArAccNum THEN DO:
      fError("Invalid AR account").
      NEXT.
   END.
   
   FIND Customer WHERE Customer.CustNum = Invoice.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      fError("Unknown customer").
      NEXT.
   END.
   
   IF Customer.OrgID NE lcCustID THEN DO:
      fError("Invalid customer ID").
      NEXT.
   END.
   
   FIND InvGroup where 
        InvGroup.Brand    = gcBrand AND 
        InvGroup.InvGroup = Customer.InvGroup NO-LOCK NO-ERROR.

   ASSIGN 
      llCustBal    = IF AVAILABLE InvGroup
                     THEN InvGroup.UpdCustBal
                     ELSE FALSE
      ldPosting[1] = ldPaidAmt
      ldPosting[2] = -1 * ldPaidAmt
      liAccount[1] = liDebitAcc
      liAccount[2] = liCreditAcc.

   liDone = liDone + 1.

   RUN Ar/createpaym (Invoice.CustNum,
                   Invoice.InvNum,
                   "",
                   ldtAccDate,
                   ldtPaymDate,
                   ldPosting,
                   liAccount,
                   lcPaymSrc,
                   0,                                /* payment type */
                   "",                               /* reference */
                   "Data from file " + lcPlainFile,  /* memo */
                   OUTPUT liVoucher).

   IF liVoucher = 0 THEN DO:
      fError("Payment creation failed; " + RETURN-VALUE).
      NEXT.
   END.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

   /* interest, balances etc. */
   fInvoicePaymentUpdate(BUFFER Invoice,
                         ldtPaymDate,
                         ldPosting[1],
                         liVoucher,
                         FALSE,        /* interest */
                         llCustBal,
                         FALSE).       /* payment behaviour */

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice). 

   CREATE Memo.
   ASSIGN 
      Memo.Brand     = gcBrand
      Memo.HostTable = "Invoice"
      Memo.KeyValue  = STRING(Invoice.InvNum)
      Memo.CustNum   = Invoice.CustNum
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = katun 
      Memo.MemoTitle = "Payment Posting"
      Memo.MemoText  = "Based on data delivered in file by Yoigo.".
      Memo.CreStamp  = fMakeTS().

   FIND Payment WHERE Payment.Voucher = liVoucher NO-LOCK.

   fWriteLog("VOUCHER: " + Payment.ExtVoucher).

END.     

fCleanEventObjects().




