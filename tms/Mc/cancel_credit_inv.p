/* cancel_credit_inv.p      17.03.2002/aam 

   remove credit invoice and mark the debit invoice as uncredited and unpaid

   note: contracts must be checked manually if events were released
         (contracts are combined)

         10.04.2002/aam mark invseqs and SingleFees as billed 
         21.05.2002/aam recalculate inv-asub and inv-ccn (creprows) 
         07.06.2002/aam use Invoice.OverPaym for overpayment
         11.10.2002/aam delete also rows of credit invoice
         21.01.2003/aam eventlog 
         19.02.2003/aam prohibit cancelling of partial creditings
         28.08.2003/aam fInt2Date away from where clause of foreach
         15.09.2003/aam brand
         15.02.2005/aam update Invoice.PaymState

*/

{Syst/commpaa.i}
katun = "snet".
gcBrand = "1".

{Func/coinv.i}
{Syst/eventval.i} 
{Func/fcustbal.i}

def buffer bInvoice for Invoice.
def buffer cInvoice for Invoice.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

   DEFINE VARIABLE lhbInvoice AS HANDLE NO-UNDO.
   lhbInvoice = BUFFER bInvoice:HANDLE.
   RUN StarEventInitialize(lhbInvoice).

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).
END.


/* debit invoice */
def var xDebInv   as int  no-undo.
/* credit invoice */
def var xCredInv  as int  no-undo. 

def var xCredDate as date no-undo. 
def var xBal      as dec  no-undo. 
def var xOk       as log  no-undo. 

/* ask both original invoice and credit invoice -> one way to be sure 
   that user knows what is going on */
pause 0.
update gcBrand 
          label "Brand"
          colon 15 skip
       xDebInv 
          label "Debit invoice" 
          format ">>>>>>>9"
          colon 15 skip
       xCredInv 
          label "Credit invoice"
          format ">>>>>>>9"
          colon 15 
       with side-labels overlay row 6 centered
            title " Credit cancellation "
            frame fCredit.
if xDebInv = 0 or xCredInv = 0 then return.

find first Invoice where 
    Invoice.Brand  = gcBrand AND
    Invoice.InvNum = xDebInv no-error.
if not available Invoice then do:
    message "Unknown debit invoice"
    view-as alert-box.
    return.
end.

if Invoice.CrInvNum = 0 then do:
    message "Debit invoice has not been credited"
    view-as alert-box.
    return.
end.

if Invoice.CrInvNum ne xCredInv then do:
    message "Debit invoice has been credited with invoice" 
            Invoice.CrInvNum ", not with" xCredInv
    view-as alert-box.
    return.
end.

find bInvoice where bInvoice.InvNum = Invoice.CrInvNum no-error.
if not available bInvoice then do:
    message "Credit invoice was not found"
    view-as alert-box.
    return.
end.

IF Invoice.InvAmt < 0 THEN DO:
   xOk = FALSE.
   MESSAGE "The amount of debit invoice is negative. Are You sure that"
           "You have entered the invoice numbers correctly?"
   VIEW-AS ALERT-BOX
   QUESTION
   SET xOk.
   IF NOT xOk THEN RETURN.
END. 

IF bInvoice.InvAmt NE -1 * Invoice.InvAmt THEN DO:
   xOk = FALSE.
   MESSAGE "This is a partial credit. If you cancel it then all related"
           "events must be manually checked. Continue ?"
   VIEW-AS ALERT-BOX 
   QUESTION
   BUTTONS YES-NO 
   SET xOk.
   IF NOT xOk THEN RETURN. 
END. 

ELSE DO:
   assign xOk = false. 
   message "Crediting of debit invoice" Invoice.InvNum 
           "(" Invoice.InvAmt ") will be cancelled." skip
           "Proceed with the cancellation ?"
           view-as alert-box
           question
           buttons yes-no
           set xOk.
   if not xOk then return. 
END.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

/* remove credit invoice and it's payments */
for each Payment of bInvoice:
    IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPayment).
    delete Payment.
end.
assign xCredDate = bInvoice.InvDate.

for each InvRow of bInvoice:
    delete InvRow.
end.

FOR EACH SubInvoice OF bInvoice EXCLUSIVE-LOCK:
   DELETE SubInvoice.
END.

/* remove the payment for debit invoice that has been done during 
   the credit process 
*/
for each Payment of Invoice where
    Payment.AccDate  = xCredDate and
    Payment.PaymType = 5:

        IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPayment).

        delete Payment.
end.

/* mark debit invoice as uncredited */
assign Invoice.CrInvNum = 0.
FOR EACH SubInvoice OF Invoice WHERE 
         SubInvoice.CrInvNum = bInvoice.InvNum EXCLUSIVE-LOCK:
   SubInvoice.CrInvNum = 0.
END.

/* are there other creditings on this invoice */
FOR EACH cInvoice NO-LOCK WHERE
         cInvoice.Brand    = gcBrand         AND 
         cInvoice.CustNum  = Invoice.CustNum AND
         cInvoice.CrInvNum = Invoice.InvNum  AND
         cInvoice.InvNum  NE bInvoice.InvNum
BY cInvoice.InvDate DESC:
    Invoice.CrInvNum = cInvoice.InvNum.
    LEAVE.
END. 


/* if calls were released they should be remarked as billed */
FOR EACH SubInvoice OF Invoice NO-LOCK:
   find invseq where 
       invseq.invseq = SubInvoice.invseq no-error.
   if available invseq then assign
      invseq.billed = true
      invseq.invnum = Invoice.InvNum
      InvSeq.SubInvNum = SubInvoice.SubInvNum.
END.      

/* contracts */
xOk = FALSE.
FOR each FFItem NO-LOCK where 
         FFItem.InvNum  = Invoice.InvNum:
  xOk = TRUE.
  LEAVE.
END.

IF NOT xOk THEN
for each InvRow of Invoice no-lock where
         InvRow.RowType = 3,
    each FFItem NO-LOCK where 
         FFItem.InvNum  = Invoice.InvNum     AND
         FFItem.BillCode = InvRow.BillCode   AND
    /* contracts can be combined */
    (IF InvRow.Qty = 1 
     THEN  FFItem.Amt = InvRow.Amt 
     ELSE TRUE):

    IF fInt2date(FFItem.Concerns[1],1) ge InvRow.FromDate and
       fInt2date(FFItem.Concerns[2],2) le InvRow.ToDate
    THEN ASSIGN FFItem.InvNum = Invoice.InvNum
                FFItem.SubInvNum = InvRow.SubInvNum
                FFItem.billed = true.

end.

/* single fees */
xOk = FALSE.
FOR EACH SingleFee NO-LOCK WHERE
         SingleFee.InvNum  = Invoice.InvNuM.
   xOk = TRUE.
   LEAVE.
END.

IF not xOK THEN 
for each InvRow of Invoice no-lock where
    InvRow.RowType = 4:

    /* bitems can also be combined */
    IF InvRow.FFItemNum > 0 THEN 
    FOR FIRST SingleFee where
              SingleFee.Brand = gcBrand AND 
              SingleFee.FMItemId = InvRow.FFItemNum:

        assign SingleFee.billed = true
               SingleFee.InvNum = InvRow.InvNum
               SingleFee.SubInvNum = InvRow.SubInvNum.
    END.

    ELSE DO:
       MESSAGE "Released single fees must be manually marked as billed."
       VIEW-AS ALERT-BOX. 
    END. 
end.               


FIND Customer OF Invoice NO-LOCK.

/* customer's adv.payment marked as used again */
if bInvoice.AdvPaym ne 0 then do:
    fCustBal(Customer.CustNum,
             "",
             "AP",
             -1 * bInvoice.AdvPaym). 
             /* AdvPaym is negative on the bill -> add */
end.
/* overpayment */
if bInvoice.OverPaym ne 0 then do:
    fCustBal(Customer.CustNum,
             "",
             "OP",
             -1 * bInvoice.OverPaym). 
             /* OverPaym is negative on the bill -> add */
end.

for each Invrow OF Invoice exclusive-lock:
   if InvRow.creditinvnum = bInvoice.InvNum
   then InvRow.creditinvnum = 0.
end.


IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhbInvoice).   
delete bInvoice.

/* check the actual payment status */
run invbal.p (Invoice.InvNum, output xbal).
assign Invoice.PaidAmt = Invoice.InvAmt - xbal. 
CASE Invoice.PaidAmt:
WHEN 0.00           THEN Invoice.PaymState = 0.
WHEN Invoice.InvAmt THEN Invoice.PaymState = 2.
OTHERWISE                Invoice.PaymState = 1.
END CASE. 

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).

/* recalculate inv-asub and inv-ccn */
run creprows.p (Invoice.InvNum,0). 

message "Credit invoice has been deleted and debit invoice has been"
        "marked as uncredited. Events have been remarked as billed."
        "Asub- and CCN-counters have been recalculated." 
        "If the events of the debit invoice were released"
        "(marked as unbilled), they should be checked manually,"
        "especially if this was a partial credit invoice."
view-as alert-box. 





