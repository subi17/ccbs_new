DEFINE VARIABLE lcOutputFile AS CHARACTER format "x(70)" NO-UNDO. 
lcOutputFile = "/apps/yoigo/tms_support/billing/test_invoice_list.txt".

UPDATE lcOutputFile LABEL "Output file" 
   WITH frame a TITLE " Subinvoice list of all test invoices (type 99) ".

IF NOT lcOutputFile > "" THEN QUIT.

def stream sout.
output stream sout to value(lcOutputFile).

PUT STREAM sout unformatted 
   "ExtInvNumber|CustNum|SubsID|MSISDN|AmtExclTax|TAXAmt|TOTAL" SKIP.

FOR EACH invoice NO-LOCK WHERE
         invoice.Brand = "1" AND
         invoice.invtype = 99,
   EACH subinvoice OF invoice NO-LOCK:

   PUT STREAM SOUT unformatted
      invoice.extinvid "|"
      invoice.custnum "|"
      subinvoice.msseq "|"
      subinvoice.cli "|"
      subinvoice.amtexclvat "|"
      subinvoice.vatamt "|"
      subinvoice.invamt 
      skip.
END.
output stream sout close.

MESSAGE "DONE" VIEW-AS ALERT-BOX.
hide frame a.
pause 0.



