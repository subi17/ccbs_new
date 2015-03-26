def stream sout.
output stream sout to /apps/snet/200801/as_yts445.txt.
FOR EACH customer NO-LOCK:
   
   if 
      customer.custnum eq 233718
      or customer.custnum  eq 239696
      or customer.custnum eq 239680
      or customer.custnum eq 239666 then next.

   FIND FIRST PostCode WHERE
      postcode.country = "ES" AND
      postcode.ZipCode = customer.zipcode NO-LOCK NO-ERROR.
      
   FIND FIRST msowner where msowner.brand = "1"
   and msowner.custnum = customer.custnum and
   msowner.clitype = "tarj3" and 
   msowner.tsbegin < 20080104
   NO-LOCK no-error.
   
   if avail msowner then do:
   
      if not avail postcode then 
      put stream sout unformatted
      customer.zipcode " (not avail) " msowner.custnum " " msowner.tsbegin skip.
      
      else if customer.region ne postcode.region then 
      put stream sout unformatted
      customer.zipcode " " msowner.custnum " " msowner.tsbegin skip.
   end.
   
END.
output stream sout close.
