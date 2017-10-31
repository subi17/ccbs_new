{Syst/commpaa.i}
gcbrand = "1".
katun = "ari".
{Syst/eventval.i}
{Func/faccper.i}
{Func/fcustbal.i}
{Func/fcustcnt.i}
{Func/finvnum.i}
{Func/finvoiceacc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}
END.


def var i as int no-undo.
def var j as int no-undo.
def var lcline as char no-undo.
def var limsseq as int no-undo.
def var ldamt as dec no-undo.
def var liinv as int no-undo.
def var lccli as char no-undo.
def var ldtaxamt as dec no-undo.
def var lccreditnote as char no-undo.

def buffer breq for msrequest. 

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
input stream sread from /apps/snet/201001/aam_yts1914.log.

def stream slog.
output stream slog to /apps/snet/201001/aam_yts1917.log append.

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
      limsseq = integer(entry(1,lcline,chr(9)))
      lccli   = entry(2,lcline,chr(9))
      ldamt   = decimal(entry(6,lcline,chr(9)))
      no-error.
   
   if error-status:error then next.

   assign
      i = i + 1
      liinv = 0.
   
   for first msrequest no-lock where
             msrequest.msseq = limsseq and
             reqtype = 0 and
             reqstat = 2 and
             actstamp >= 20091201 and
             reqcparam2 = "cont5" and
             lookup(reqcparam1,"cont,cont2,cont4") > 0,
       first breq no-lock where
             breq.origrequest = msrequest.msrequest and
             breq.reqtype = 9 and
             breq.createfees = true,
       first singlefee no-lock where
             singlefee.brand = "1" and
             singlefee.hosttable = "mobsub" and
             singlefee.keyvalue = string(msrequest.msseq) and
             singlefee.billcode = "termperiod" and
             singlefee.billper >= 200912 and
             singlefee.memo[1] begins "postrenove":
      liinv = singlefee.invnum.
      ldamt = singlefee.amt.
   end.

   if liinv = 0 then next.

   find first invoice where invoice.invnum = liinv no-lock no-error.
   if invoice.invtype ne 1 or invoice.invdate ne 1/1/10 or
      invoice.msseq ne limsseq or invoice.vatincl = true
   then do:
      message "check invoice:" liinv
      view-as alert-box.
      next.
   end.
   
   find first invrow of invoice where invrow.billcode = "termperiod"
      no-lock no-error.
   if not available invrow or invrow.amt < ldamt then do:
      message "check row:" 
             limsseq 
             invoice.invnum 
             invoice.extinvid 
             ldamt
      view-as alert-box.
      next.
   end.

   FIND Customer WHERE Customer.CustNum = Invoice.CustNum NO-LOCK.

   run pcreditnote.
   
   j = j + 1.
   disp i  
        j 
        msrequest.cli 
        msrequest.msseq 
        msrequest.actstamp 
        msrequest.reqcparam1 format "x(10)" .
   
   find first taxzone where taxzone.taxzone = invoice.taxzone no-lock.
   
   put stream slog unformatted
     lccli    ";"
     limsseq  ";"
     customer.custnum ";"
     customer.orgid ";"
     lccreditnote ";"
     -1 * ldamt ";"
     ldtaxamt ";"
     taxzone.tzname ";"
     -1 * ldamt + ldtaxamt skip.
     
end.

disp i j.

input stream sread close.
output stream slog close.

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
   bCreditInv.ChgStamp = Func.Common:mMakeTS().
 
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
   FOR FIRST InvRow of Invoice exclusive-lock WHERE 
             InvRow.BillCode = "TERMPERIOD":

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
         bCreditRow.GrossAmt       = -1 * ldamt
         bCreditRow.Amt            = -1 * ldamt.
         
      ldtaxamt = ldtaxamt + 
                 round(bCreditRow.Amt * bcreditrow.vatperc / 100,2).
      
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
      bCreditInv.AmtExclVAT  = -1 * ldamt
      bCreditInv.VATAmt      = ldtaxamt
      bCreditInv.InvAmt      = bCreditInv.AmtExclVAT + bCreditInv.VATAmt
      bCreditInv.CurrAmt     = bCreditInv.InvAmt
      bCreditInv.Rounding    = 0
      bCreditInv.VatAmount   = 0
      bCreditInv.VatBasis    = 0
      bCreditInv.VatAmount[1] = ldtaxamt
      bCreditInv.VatBasis[1]  = -1 * ldamt.

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
      Memo.CreStamp  = Func.Common:mMakeTS()
      Memo.MemoSeq   = next-value(MemoSeq)
      Memo.Brand     = gcBrand
      Memo.MemoTitle = "Credited"
      Memo.CreUser   = katun
      Memo.HostTable = "Invoice"
      Memo.KeyValue  = STRING(bCreditInv.InvNum)
      Memo.CustNum   = Invoice.CustNum.
      Memo.Memotext  = Func.Common:mGetHdrText(50,Customer.Language) + " " +
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
      Memo.CreStamp  = Func.Common:mMakeTS()
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

