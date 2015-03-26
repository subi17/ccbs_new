output to /apps/snet/200806/as_yts838.txt.

def buffer c2 for customer.

FOR EACH customer where
   custnum >= 500000 and
   credate eq 6/18/2008 and 
   (country ne "es" or nationality eq "") NO-LOCK:
   
   FIND FIRST ordercustomer where 
      ordercustomer.custnum = customer.custnum and
      ordercustomer.rowtype = 1 NO-LOCK NO-ERROR.

   FIND FIRST c2 where rowid(c2) = rowid(customer) EXCLUSIVE-LOCK NO-ERROR.

   disp 
      customer.custnum 
      customer.country column-label "current country" 
      ordercustomer.country column-label "fixed country"
      customer.nationality column-label "current nationality"
      ordercustomer.nationality column-label "fixed nationality".

   assign
      c2.country     = ordercustomer.country
      c2.nationality = ordercustomer.nationality.
END.
output close.
