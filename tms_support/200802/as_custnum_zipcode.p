DEFINE VARIABLE i AS INTEGER NO-UNDO. 
output to /apps/snet/200802/custnum_zipcode.txt.
FOR EACH customer where brand = "1" NO-LOCK.
FIND FIRST postcode NO-LOCK where 
   postcode.country = customer.country and
   postcode.zipcode = customer.zipcode no-error.
   if not avail postcode then do:
      disp customer.custnum customer.zipcode.
      i = i + 1.
   end.   
end.
output close.
disp i.
