{Syst/testpaa.i}
katun = "ari".
{Func/timestamp.i}

def var lclist as char no-undo.
def var lcerr  as char no-undo.
def var ldamt  as dec  no-undo.
def var i      as int  no-undo.

def stream slog.
output stream slog to /apps/snet/200709/aam_yts126.log.

put stream slog unformatted
  "OrderID"    chr(9)
  "Created"    chr(9)
  "Status"     chr(9)
  "CustomerID" chr(9)
  "Name"       chr(9)
  "Amount"     skip.

for each order no-lock where
         brand = "1",
   first OrderPayment OF Order NO-LOCK where
         orderpayment.method = 2,
   first OrderCustomer of order no-lock:
   
   if order.invnum > 0 and
      can-find(invoice where invoice.invnum = order.invnum)
   then next.
   
   RUN Mc/cashfee.p (order.orderid,
                  2,
                  output lclist,
                  output ldamt,
                  output lcerr).
   if ldamt = 0 then next.

   i = i + 1.
   
   put stream slog unformatted
      order.orderid            chr(9)
      fts2hms(order.crstamp)   chr(9)
      order.statuscode         chr(9)
      ordercustomer.custid     chr(9)
      ordercustomer.firstname + " " + 
      ordercustomer.surname1 + " " +
      ordercustomer.surname2   chr(9)
      ldamt                    skip.

   pause 0.
   disp i with 1 down.
   
   /*   
   disp i order.orderid crstamp statuscode 
        order.invnum
        ldamt format "->>>>>9.99".

   disp lcerr format "x(60)"
        lclist view-as editor size 50 by 5.
   */     
end.
