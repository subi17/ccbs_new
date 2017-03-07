input from as_yot1006.input.
{Func/date.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
def stream sout.
output stream sout to as_yot1006.log append.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

import unformatted lcLine.

looppi:
repeat:
   import unformatted lcLine.
   
   find customer where
        customer.brand = "1" and
        customer.orgid = lcline  NO-LOCK.
   
   i = i + 1.
   if i <= 10 then next.
   /*if i > 10 then leave looppi. */

   if customer.deltype ne 10 then do:

      do trans:   

      find current customer EXCLUSIVE-LOCK.

      create Memo.
      assign Memo.Brand     = "1"
             Memo.HostTable = "Customer"
             Memo.KeyValue  = string(Customer.custnum)
             Memo.CustNum   = customer.custnum
             Memo.MemoSeq   = next-value(memoseq)
             Memo.CreUser   = "Qvantel/YOT-1003" 
             Memo.MemoTitle = "Delivery type changed"
             Memo.MemoText  = "Correos nos devuelve la carta. Inhabilitamos el envío de la misma".
             Memo.CreStamp  = fmakets().
   
      put stream sout unformatted 
            lcLine "|" 
            customer.custnum "|"
            customer.deltype "|Changed to 10 " memo.memoseq skip.

      assign customer.deltype = 10.
      release customer.
      release memo.

      end.
   end.
   else  
   put stream sout unformatted 
      lcLine "|" 
      customer.custnum "|"
      customer.deltype "|OK" skip.
   
end.
