find customer where
   custnum = 300 EXCLUSIVE-LOCK.
/*
def stream sbak.
output stream sbak to /apps/snet/200911/as_custnum300_dummy_bak.d. 
export stream sbak customer.
*/
def stream sin.
input stream sin from /apps/snet/200911/dummy_cust.d.

DEFINE TEMP-TABLE ttCust LIKE Customer.

repeat:
   create ttCust.
   import stream sin ttCust.
   if ttCust.custnum = 300 then
   buffer-copy ttCust except ttCust.custnum to customer.
   leave.
end.

