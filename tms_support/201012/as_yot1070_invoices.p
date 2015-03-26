input from as_yot1070_invoices.txt.
output to as_yot1070_extra.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

repeat:
   import unformatted lcLine.
   find invoice where
        invoice.brand = "1" and
        invoice.extinvid = lcLine NO-LOCK.

   find order where
      order.invnum = invoice.invnum NO-LOCK.
   put unformatted order.orderid skip.
end.
