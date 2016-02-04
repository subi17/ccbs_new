{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{Func/timestamp.i}
{Syst/eventval.i}
{Func/faccper.i}
{Func/fcustbal.i}
{Func/fcustcnt.i}
{Func/finvnum.i}
{Func/fhdrtext.i}
{Func/finvoiceacc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}
END.

session:numeric-format = "european".

def var i as int no-undo.
def var j as int no-undo.
def var lcline as char no-undo.
def var limsseq as int no-undo.
def var ld1credamt as dec no-undo.
def var liinv as int no-undo.
def var lccli as char no-undo.
def var ldtaxamt as dec no-undo.
def var lccreditnote as char no-undo.
def var lc1cred as char no-undo.
def var ld1credtax as dec no-undo.
def var ldbal as dec no-undo.
def var ldamt as dec no-undo.
def var ldlistbal as dec no-undo.

def buffer breq for msrequest. 
def buffer binv for invoice.

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


def stream sread.
input stream sread from /apps/snet/201003/yts1917_invoices_status_v1.txt.

def stream slog.
output stream slog to /apps/snet/201003/yts1917_second_credit.log.

put stream slog unformatted
   "MSISDN"     ";"
   "Subscr.ID"  ";"
   "Customer"   ";"
   "Cust.ID"    ";"
   "Credit Note" ";"
   "Amount"      ";"
   "Taxes"       ";"
   "Tax Type"    ";"
   "Total Amt"   skip.
   
repeat:

   import stream sread unformatted lcline.

   assign 
      limsseq = integer(entry(3,lcline,chr(9)))
      lccli   = entry(2,lcline,chr(9))
      lc1cred = entry(4,lcline,chr(9))
      ldlistbal = decimal(entry(8,lcline,chr(9)))
      no-error.
   
   if error-status:error then next.

   assign
      i = i + 1
      liinv = 0.
   
   find first invoice where 
              invoice.brand = "1" and
              invoice.extinvid = lc1cred no-lock no-error.
   if not available invoice then next.
   
   find first binv where binv.invnum = invoice.crinvnum no-lock.
   assign
      liinv = invoice.invnum                                  
      ld1credamt = -1 * binv.invamt
      ld1credtax = -1 * binv.vatamt.
   
   if invoice.invtype ne 1 or invoice.invdate ne 1/1/10 or
      invoice.msseq ne limsseq or invoice.vatincl = true
   then do:
      message "check invoice:" liinv lc1cred
              invoice.invtype
              invoice.invdate
              invoice.msseq
              invoice.vatincl
      view-as alert-box.
      next.
   end.
   
   find first invrow of invoice where invrow.billcode = "termperiod"
      no-lock no-error.
   if not available invrow then do:
      message "check row:" 
             limsseq 
             invoice.invnum 
             invoice.extinvid 
             ld1credamt
      view-as alert-box.
      next.
   end.

   FIND Customer WHERE Customer.CustNum = Invoice.CustNum NO-LOCK.

   RUN Ar/invbal(invoice.invnum,
              output ldbal).
   
   /*
   if ldlistbal ne ldbal then do:
      message "check inv (bal):"
             limsseq 
             invoice.invnum 
             invoice.extinvid 
             invoice.invamt
             ldbal 
             ldlistbal
      view-as alert-box.
      next.
   end.
   */
   
   run pcreditnote.
   
   j = j + 1.
   disp i j. 
   
   find first taxzone where taxzone.taxzone = invoice.taxzone no-lock.
   
   put stream slog unformatted
     lccli    ";"
     limsseq  ";"
     customer.custnum ";"
     customer.orgid ";"
     lccreditnote ";"
     ldamt ";"
     ldtaxamt ";"
     taxzone.tzname ";"
     ldamt + ldtaxamt skip.

end.

disp i j.

input stream sread close.
output stream slog close.

session:numeric-format = "american".

PROCEDURE pCreditNote:

   DEF VAR ldtCreditDate AS DATE NO-UNDO.
   DEF VAR ldCreditLimit AS DEC  NO-UNDO.
   DEF VAR liCalled      AS INT  NO-UNDO. 
   DEF VAR llUpdate      AS LOG  NO-UNDO. 
   DEF VAR lcCustName    AS CHAR NO-UNDO.
   DEF VAR lcExtInvID    AS CHAR NO-UNDO. 
   DEF VAR lcPrefix      AS CHAR NO-UNDO.
   DEF VAR liInvType     AS INT  NO-UNDO. 
   DEF VAR lcInvGroup    AS CHAR NO-UNDO. 
   DEF VAR ldActStamp    AS DEC  NO-UNDO. 
   DEF VAR lcError       AS CHAR NO-UNDO. 
   DEF VAR ldBalance     AS DEC  NO-UNDO.
   DEF VAR liAccNum      AS INT  NO-UNDO.
   DEF VAR ldTotRefund   AS DEC  NO-UNDO.
   DEF VAR ldRefundAmt   AS DEC  NO-UNDO. 
   DEF VAR liDDCancel    AS INT  NO-UNDO.
   DEF VAR ldRefundReq   AS DEC  NO-UNDO EXTENT 3. 
   DEF VAR liCreated     AS INT  NO-UNDO. 
   DEF VAR liInvRecid    AS INT  NO-UNDO.

   DEF BUFFER bCreditInv  FOR Invoice.
   DEF BUFFER bCreditRow  FOR InvRow.
   DEF BUFFER bCreditASub FOR InvASub.
   DEF BUFFER bCreditCCN  FOR InvCCN.

   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhInvoice    AS HANDLE NO-UNDO.
      DEFINE VARIABLE lhbCreditInv AS HANDLE NO-UNDO.
   END.
   
   FIND CURRENT Invoice EXCLUSIVE-LOCK.
      
   ldtCreditDate = TODAY.
 
   /* credit invoice type according to original invoice */
   CASE Invoice.InvType:
   WHEN 6  THEN liInvType = 8.
   WHEN 7  THEN liInvType = 9.
   WHEN 4  THEN liInvType = 10.
   WHEN 3  THEN liInvType = 11.
   WHEN 12 THEN liInvType = 13.
   OTHERWISE    liInvType = 5.
   END CASE. 

   lcInvGroup = Customer.InvGroup.
   /* get taxzone from original invoice and invgroup through that */
   IF Invoice.TaxZone > "" THEN
   FOR FIRST InvGroup NO-LOCK WHERE
             InvGroup.Brand   = "1" AND
             InvGroup.TaxZone = Invoice.TaxZone:
      lcInvGroup = InvGroup.InvGroup.       
   END.

   liInvRecid = RECID(Invoice).
   lccreditnote = "".
   
   /* external invoice id */   
   lcExtInvID = fGetInvNum(lcInvGroup,
                           liInvType,
                           ldtCreditDate,
                           OUTPUT lcPrefix).

   IF lcExtInvID = "" THEN DO:
      MESSAGE "Invoice sequence is invalid" VIEW-AS ALERT-BOX.
      RETURN.
   END.
         
   ExtInvNum:
   REPEAT:
      /* make sure that field values are visible to other sessions */
      FIND Invoice WHERE RECID(Invoice) = liInvRecid EXCLUSIVE-LOCK.
 
      /* check IF invoice number is already in use */
      IF NOT can-find(FIRST Invoice where
                            Invoice.Brand    = gcBrand AND 
                            Invoice.ExtInvID = lcExtInvID AND
                            RECID(Invoice) NE liInvRecid) 
      THEN LEAVE ExtInvNum.
         
      /* get next-value */
      lcExtInvID = fLocalNextExtID(lcPrefix,
                                   lcExtInvID).
   END.

   CREATE bCreditInv.
   bCreditInv.ExtInvID = lcExtInvID.
      
   BUFFER-COPY Invoice EXCEPT InvNum ExtInvID TO bCreditInv.
   bCreditInv.ChgStamp = fMakeTS().
 
   InvNum:
   REPEAT:

      bCreditInv.InvNum = NEXT-VALUE(IntInvNum) NO-ERROR.
         
      VALIDATE bCreditInv NO-ERROR.
                  
      /* another process has just used the same number */
      IF ERROR-STATUS:ERROR OR bCreditInv.InvNum = 0 THEN NEXT.

      LEAVE.
   END.

   ASSIGN
      bCreditInv.InvDate      = ldtCreditDate
      bCreditInv.DueDate      = ldtCreditDate
      bCreditInv.ClaimState   = 0
      bCreditInv.WInvDisp     = FALSE
      bCreditInv.ExpStamp     = 0
      bCreditInv.PaidAmt      = 0
      bCreditInv.CrInvNum     = Invoice.InvNum
      bCreditInv.DDState      = 0
      bCreditInv.PrintState   = 0
      bCreditInv.InvType      = liInvType
      bCreditInv.InvCfg[1]    = IF Invoice.PrintState = 0 
                                THEN TRUE
                                ELSE Invoice.InvCfg[1]
      bCreditInv.CreditReason = "2011"
      bCreditInv.DeliveryState = 0.
                              
   /* update last used invoice number */
   llUpdate = NOT fUpdateInvNum(lcInvGroup,
                                liInvType,
                                ldtCreditDate,
                                lcExtInvID).


   ldtaxamt = 0.
   FOR EACH InvRow of Invoice exclusive-lock WHERE 
             InvRow.BillCode NE "TERMPERIOD":

      CREATE bCreditRow.
      BUFFER-COPY InvRow TO bCreditRow.
      
      ASSIGN
         bCreditRow.InvNum         = bCreditInv.InvNum
         bCreditRow.InvRow         = NEXT-VALUE(irid)
         /* minutes and qty are not quite true if partial credit and row
            amount has been changed, but there is no way of knowing what
            the real amounts are then */
         bCreditRow.Minutes        = 0
         bCreditRow.Qty            = -1
         /* mark the credit nbr TO the credited line, 
            important in partial crediting */
         InvRow.CreditInvNum       = bCreditInv.InvNum 
         bCreditRow.GrossAmt       = -1 * InvRow.GrossAmt
         bCreditRow.Amt            = -1 * InvRow.Amt.
         
      /*
      ldtaxamt = ldtaxamt + 
                 round(bCreditRow.Amt * bcreditrow.vatperc / 100,2).
      */
      /* in yoigo newest accounts are always used 
      FOR FIRST BillItem NO-LOCK WHERE
                BillItem.Brand = gcBrand AND
                BillItem.BillCode = bCreditRow.BillCode:
         bCreditRow.SlsAcc = fInvRowAccount(Customer.Category,
                                            bCreditInv.VatUsage).
      END.
      */
   END.

   ASSIGN 
      bCreditInv.InterestAmt = 0 
      bCreditInv.AdvPaym     = 0 
      bCreditInv.OverPaym    = 0 
      bCreditInv.VATAmt      = -1 * (Invoice.VatAmt - ld1credtax)
      bCreditInv.InvAmt      = -1 * (Invoice.InvAmt - ld1credamt)
      bCreditInv.CurrAmt     = bCreditInv.InvAmt
      bCreditInv.AmtExclVAT  = bCreditInv.InvAmt - bCreditInv.VATAmt
      bCreditInv.Rounding    = 0
      bCreditInv.VatAmount   = 0
      bCreditInv.VatBasis    = 0
      bCreditInv.VatAmount[1] = bCreditInv.VATAmt
      bCreditInv.VatBasis[1]  = bCreditInv.AmtExclVAT
      ldamt = bCreditInv.AmtExclVAT
      ldtaxamt = bCreditInv.VATAmt.

   IF llDoEvent THEN DO:
      lhInvoice = BUFFER Invoice:HANDLE.
      RUN StarEventInitialize(lhInvoice).
 
      lhbCreditInv = BUFFER bCreditInv:HANDLE.
      RUN StarEventInitialize(lhbCreditInv).
   END.
 
   IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhbCreditInv).


   /* credit invoice memo */
   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = next-value(MemoSeq)
      Memo.Brand     = gcBrand
      Memo.MemoTitle = "Credited"
      Memo.CreUser   = katun
      Memo.HostTable = "Invoice"
      Memo.KeyValue  = STRING(bCreditInv.InvNum)
      Memo.CustNum   = Invoice.CustNum.
      Memo.Memotext  = fGetHdrText(50,Customer.Language) + " " +
                       STRING(Invoice.InvNum) +  
                       ", YTS-1916. Handler: " + katun.

   /* customer balances */
   fCustBal(Customer.CustNum,
            bCreditInv.CLI,  
            "ARBAL",
            bCreditInv.InvAmt).

   fCustCount(Customer.CustNum,
              "UB",
               -1 * bCreditInv.AmtExclVat). 

   /* update original (debit) invoice */
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

   ASSIGN 
      Invoice.CrInvNum     = bCreditInv.InvNum 
      Invoice.CreditReason = "2011"
      lccreditnote = bcreditinv.extinvid.
                           

   /* dd-invoice has not been sent to bank */
   IF Invoice.DDBankAcc > "" AND Invoice.DDState = 0 THEN ASSIGN
      Invoice.DDBankAcc     = ""
      Invoice.ChargeType    = 1
      bCreditInv.DDBankAcc  = ""
      bCreditInv.ChargeType = 1.

   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Brand     = gcBrand
      Memo.MemoTitle = "Credited"
      Memo.CreUser   = katun
      Memo.HostTable = "Invoice"
      Memo.KeyValue  = STRING(Invoice.InvNum)
      Memo.CustNum   = Invoice.CustNum
      Memo.Memotext  = "YTS-1916, credited by " + katun.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice). 

   /* if used nbr could not be updated earlier then try it once more */
   IF NOT llUpdate THEN DO:
      fUpdateInvNum(lcInvGroup,
                    liInvType,
                    ldtCreditDate,  
                    lcExtInvId).
   END.

   RELEASE bCreditInv.

END PROCEDURE.

