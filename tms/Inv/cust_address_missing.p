define input parameter icOutputFile as char.
DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
   "RUNNING cust_address_missing.p" skip(1).

def var i as int no-undo.
def var llok as log no-undo.

def var lccustzone as char no-undo.
def var lcorderzone as char no-undo.

for each mobsub no-lock where
         brand = "1" and
         paytype = false,
   first customer no-lock where
         customer.brand = "1" and
         customer.custnum = mobsub.invcust,
   first order no-lock where
         order.msseq = mobsub.msseq,
   first ordercustomer of order no-lock where
         ordercustomer.rowtype = 1 and
         ordercustomer.custid = customer.orgid:

   if customer.zipcode > "00000" then next.
   if ordercustomer.zipcode <= "00000" then next. 

   ok = false.

   FIND Region WHERE Region.Region = customer.Region NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Region THEN lccustZone = "1".
   ELSE lccustZone = Region.TaxZone.
   
   FIND Region WHERE Region.Region = ordercustomer.Region NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Region THEN lcorderZone = "1".
   ELSE lcorderZone = Region.TaxZone.
   
   disp stream sout
      customer.custnum   
      customer.orgid       
      mobsub.cli             
      customer.custname 
      customer.surname2 
      customer.firstname
      customer.address   
      customer.zipcode + " " + customer.postoffice 
      ordercustomer.surname1 
      ordercustomer.surname2 
      ordercustomer.firstname      
      ordercustomer.address        
      ordercustomer.zipcode + " " + ordercustomer.post
         with 1 down frame fstat 
           title "Customer Address Check".
         
   i =  i + 1.
   
   pause 0.
   disp i with 1 down.
end.

if ok then put stream sout unformatted "OK" SKIP(1).
output stream sout close.

