DEFINE VARIABLE llok AS LOGICAL NO-UNDO. 

def stream sout.
output stream sout to /apps/yoigo/tms_support/billing/reset_first_month_fees.log.

DEFINE VARIABLE ldeFee AS DECIMAL NO-UNDO. 
def buffer bfixedfee for fixedfee.

FOR EACH fixedfee NO-LOCK where
         fixedfee.begperiod = 201110 and
         fixedfee.servicelimitgroup begins "pmf:":

   FIND FIRST FFItem of fixedfee NO-LOCK NO-ERROR.
   if ffitem.billed then next.
   if ffitem.billperiod ne fixedfee.begperiod then next.

   llok = true.

   if index(servicelimitgroup,"contd2") > 0 and
      ffitem.amt ne 35 then assign
         ldeFee = 35
         llok = false.
   if index(servicelimitgroup,"contdata") > 0 and
      ffitem.amt ne 25 then assign
         ldeFee = 25
         llok = false.
   if index(servicelimitgroup,"contd3") > 0 and
      ffitem.amt ne 8 then assign
         ldeFee = 8
         llok = false.
   if index(servicelimitgroup,"mdub2") > 0 and
      ffitem.amt ne 15 then assign
         ldeFee = 15
         llok = false.
   if servicelimitgroup begins "PMF:MDUB:" and
      ffitem.amt ne 8 then assign
         ldeFee = 8
         llok = false.

/*   if index(servicelimitgroup,"contd2") > 0 and
      ffitem.amt ne 35 then llok = false. */

   if not llok then do trans:

      find bfixedfee where
          rowid(bfixedfee) = rowid(fixedfee) EXCLUSIVE-LOCK.

      find current ffitem EXCLUSIVE-LOCK.

      put stream sout unformatted
           bfixedfee.begperiod "|"
           bfixedfee.servicelimitgroup "|"
           ROUND(ffitem.amt,2) "|"
           bfixedfee.custnum "|"
           bfixedfee.keyvalue "|"
           ldefee "|" 
           ffitem.ffitemnum "|"
           bfixedfee.ffnum skip. 

      assign
         ffitem.amt = ldeFee
         bfixedfee.servicelimitgroup = "". 
      
      release ffitem.
      release bfixedfee.

   end.
end.
