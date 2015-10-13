DEFINE VARIABLE lcOutputFile AS CHARACTER format "x(70)" NO-UNDO. 
DEFINE VARIABLE lcBIName AS CHARACTER NO-UNDO. 
lcOutputFile = "/apps/yoigo/tms_support/billing/test_invoice_row_list.txt".

UPDATE lcOutputFile LABEL "Output file" 
   WITH frame a TITLE " List of all test invoices' invoice rows (type 99) ".

IF NOT lcOutputFile > "" THEN QUIT.

def stream sout.
output stream sout to value(lcOutputFile).

PUT STREAM sout unformatted 
   "ExtInvNumber|CustNum|SubsID|MSISDN|Product|ProductName|From|To|Qty|Amount|VATPerc" SKIP.

FOR EACH invoice NO-LOCK WHERE
         invoice.Brand = "1" AND
         invoice.invtype = 99,
   EACH subinvoice OF invoice NO-LOCK,
   EACH invrow NO-LOCK where
        invrow.invnum = invoice.invnum and
        invrow.SubInvNum = subinvoice.subinvnum:

   FIND BillItem where
        BillItem.Brand    = Invoice.Brand AND
        BillItem.BillCode = InvRow.BillCode
   no-lock no-error.
   IF AVAIL BillItem THEN lcBIName = BillItem.BiName.
   else lcBIName = "!! BLANK !!".

   PUT STREAM SOUT unformatted
      invoice.extinvid "|"
      invoice.custnum "|"
      subinvoice.msseq "|"
      subinvoice.cli "|"
      invrow.billcode "|"
      lcBIName "|"
      InvRow.FromDate "|"
      InvRow.ToDate "|" 
      InvRow.Qty "|" 
      InvRow.Amt "|"  
      InvRow.VatPerc 
      skip.
END.
output stream sout close.

MESSAGE "DONE" VIEW-AS ALERT-BOX.
hide frame a.
pause 0.



