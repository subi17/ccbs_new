/*
output to /apps/snet/200906/as_ycm1471_300-301_dump.d.
FOR EACH customer where
   customer.custnum = 301
   or custnum = 300 NO-LOCK:
   export customer.
END.
*/
/*
FOR EACH customer where
   customer.custnum = 301
   or custnum = 300 NO-LOCK:
   disp customer with 2 col.
END.
*/

DEFINE TEMP-TABLE ttCust LIKE Customer.

input from /apps/snet/200906/dummy_cust.d.

repeat:
   create ttCust.
   import ttCust.
   if ttCust.custnum = 300 then do:
      find customer where customer.custnum = 300 EXCLUSIVE-LOCK.
      BUFFER-COPY ttCust EXCEPT ttCust.Custnum TO customer.
   end.
/*   find customer where
    customer.custnum */
end.

