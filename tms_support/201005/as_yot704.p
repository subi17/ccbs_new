input from as_yot704.input.
{date.i}

def stream slog.
output stream slog to as_yot704.output append.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

repeat:
   import unformatted lcLine.
   find customer
      where customer.custnum =  int(lcLine) NO-LOCK.
/*
   if customer.deltype = 10 then
      put stream slog unformatted customer.custnum "|" customer.deltype "|OK" skip.
   else do:
*/

   i = i + 1.
   if i <= 1 then next.

   do trans:
      find current customer EXCLUSIVE-LOCK.
      put stream slog unformatted customer.custnum "|" customer.deltype "|10" skip.

      assign customer.deltype = 10.

      CREATE Memo.
      ASSIGN
         Memo.CreStamp  = fMakeTS()
         Memo.Brand     = "1" 
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CustNum   = customer.CustNum
         Memo.HostTable = "Customer"
         Memo.KeyValue  = STRING(Customer.Custnum)
         Memo.CreUser   = "YOT-701"
         Memo.MemoTitle = "Unable to send delivery"
         Memo.MemoText  = "Correos nos devuelve la carta. Inhabilitamos el envío de la misma".

      release customer.
   end.

/*   end. */
end.
