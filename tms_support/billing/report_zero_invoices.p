DEFINE VARIABLE i AS INTEGER NO-UNDO. 

DEFINE VARIABLE ldeCharges AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeCompensations AS DECIMAL NO-UNDO. 

DEFINE VARIABLE ldaDate AS DATE NO-UNDO. 
ldaDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).

DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFolder AS CHARACTER NO-UNDO. 

DEFINE VARIABLE liTotal AS INTEGER NO-UNDO. 

form
   ldaDate format "99-99-9999" label  "INVOICE DATE.." skip
   lcFile format "x(60)" label   "RESULT FILE..." skip
   lcStatus                label "STATUS........" skip
   liTotal label                "CHECKED......." skip
   i label                      "CASES FOUND..." 
WITH  OVERLAY ROW 8 centered
TITLE "This program generates a report of 0 amount invoices (YOB-457)" side-LABELS
FRAME a.

UPDATE ldadate WITH FRAME a.
IF ldadate EQ ? THEN QUIT.

lcFile = "/apps/yoigo/tms_support/billing/zero_invoices_" + 
   string(year(ldaDate),"9999") +
   string(month(ldadate),"99") + 
   string(day(ldadate),"99") + ".txt".

UPDATE lcFile WITH FRAME a.
IF NOT lcFile > "" THEN QUIT.

def stream sout.
output stream sout to value(lcFile).

put stream sout unformatted "ExtInvNumber;InvCust;TotalOfCharges;TotalOfCompensations;InvoiceTotal" skip. 

lcStatus = "Running".
disp lcFile lcStatus with frame a.

FOR EACH invoice where
   invoice.brand  = "1" and
   invoice.invtype = 1 and
   invoice.invdate = ldaDate NO-LOCK:
   
   liTotal = liTotal + 1.
   if liTotal mod 100 = 0 then do:
      disp liTotal i with frame a.
   end.

   if invoice.invamt > 0 THEN NEXT.

   i = i + 1.

   if invamt < 0 then do:
      MESSAGE "Negative amount"
      invoice.extinvid invoice.invamt VIEW-AS ALERT-BOX ERROR.
      next.
   end.

   assign
      ldeCompensations = 0
      ldeCharges = 0.

   FOR EACH invrow where
            invrow.invnum = invoice.invnum NO-LOCK:

      if invrow.amt >= 0 then ldeCharges = ldeCharges + invrow.amt.
      else ldeCompensations = ldeCompensations + invrow.amt.
   end.

   put stream sout unformatted
      invoice.extinvid ";"
      invoice.custnum ";"
      ldeCharges ";" 
      ldeCompensations ";" 
      invoice.invamt
      skip.

end.  

lcStatus = "Done".
disp liTotal i lcStatus with frame a.
