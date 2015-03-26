define input parameter icOutputFile as char.
DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
  "TEST chk_customer_deltype.p" skip(1).

FOR EACH customer WHERE
   customer.brand = "1" NO-LOCK: 

     if customer.deltype ne 1 or customer.chargetype ne 2 then do:
        if ok then do:
         ok = false.
         put stream sout 
            " Customer Del.Type Charge" skip
            " -------- -------- ------" skip.
        end.
        put stream sout customer.custnum " "
             customer.deltype format ">>>>>>>9" " "
             customer.chargetype format ">>>>>9" skip.
     end.
end.

if ok then put stream sout unformatted "OK" skip(1).
else put stream sout unformatted skip(1).
output stream sout close.
