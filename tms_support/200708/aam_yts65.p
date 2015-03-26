{testpaa.i}
{forderstamp.i}

def var i as int no-undo.
def var j as int no-undo.

def stream slog.
output stream slog to /apps/snet/200708/aam_yts65.log append.

def temp-table ttorder no-undo
   field orderid as int
   field cli     as char
   field msseq   as int
   field custid  as char
   field crstamp as dec
   index custid custid orderid.

def buffer bttorder for ttorder.
def buffer border for order.

for each order no-lock use-index stamp where 
         order.brand = "1" and
         order.crstamp > 20070801 and
         order.statuscode = "2",
   first ordercustomer of order no-lock:

   i = i + 1.
   create ttorder.
   assign ttorder.orderid = order.orderid
          ttorder.cli     = order.cli
          ttorder.msseq   = order.msseq
          ttorder.custid  = ordercustomer.custid
          ttorder.crstamp = order.crstamp.
end.


message i "orders in error queue"
view-as alert-box.

i = 0.

/* doubles 
for each ttorder,
   first border no-lock where
         border.brand   = "1" and
         border.orderid = ttorder.orderid
by ttorder.custid
by ttorder.crstamp:
         
   i = i + 1.
      
   for each bttorder where
            bttorder.custid = ttorder.custid and
            bttorder.cli    = ttorder.cli    and
            bttorder.crstamp >= ttorder.crstamp and
            bttorder.orderid ne ttorder.orderid,
      first order exclusive-lock where
            order.brand        = "1" and
            order.orderid      = bttorder.orderid and
            order.statuscode   = "2"              and
            order.orderchannel = border.orderchannel:
            
      /*
      disp ttorder.crstamp  format ">>>>>>>>9.99999"
           ttorder.orderid 
           order.crstamp order.orderid.      
      */
      
      put stream slog unformatted 
         order.orderid  chr(9)
         order.cli      chr(9)
         order.msseq    chr(9)
         order.statuscode skip.
         
      order.statuscode = "7".
      
      fMarkOrderStamp(Order.OrderID,
                     "Change",
                      0.0).

      j = j + 1.
      CREATE Memo.
      ASSIGN Memo.Brand     = gcBrand
             Memo.HostTable = "Order"
             Memo.KeyValue  = STRING(Order.OrderID)
             Memo.CustNum   = Order.CustNum
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = "ari" 
             Memo.MemoTitle = "Order cancelled"
             Memo.MemoText  = "Double order, YTS-65".
             Memo.CreStamp  = fMakeTS().
   end.

   pause 0.
   disp i j with 1 down.
end.
         
/* old orders */
for each ttorder,
   first order exclusive-lock where
         order.brand   = "1" and
         order.orderid = ttorder.orderid
by ttorder.custid
by ttorder.crstamp:
         
   i = i + 1.
     
   find mobsub where mobsub.cli = order.cli no-lock no-error.
   if not available mobsub then next. 
   
   find customer where customer.custnum = mobsub.agrcust no-lock.
   /*
   disp order.orderid order.cli
        mobsub.activationdate customer.orgid ttorder.custid format "x(12)".
   */     
   
   if customer.orgid = ttorder.custid and order.statuscode = "2" then do: 
        
      put stream slog unformatted 
         order.orderid  chr(9)
         order.cli      chr(9)
         order.msseq    chr(9)
         order.statuscode skip.
         
      order.statuscode = "7".
      
      fMarkOrderStamp(Order.OrderID,
                     "Change",
                      0.0).

      j = j + 1.
      CREATE Memo.
      ASSIGN Memo.Brand     = gcBrand
             Memo.HostTable = "Order"
             Memo.KeyValue  = STRING(Order.OrderID)
             Memo.CustNum   = Order.CustNum
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = "ari" 
             Memo.MemoTitle = "Order cancelled"
             Memo.MemoText  = "Double order, YTS-65".
             Memo.CreStamp  = fMakeTS().
   end.

   pause 0.
   disp i j with 1 down.

end.
*/
/*         
for each ttorder,
   first order exclusive-lock where
         order.brand   = "1" and
         order.orderid = ttorder.orderid
by ttorder.custid
by ttorder.crstamp:
         
   i = i + 1.

   disp order.orderid order.cli
        order.crstamp
        ttorder.custid format "x(12)".
      
   find mobsub where mobsub.cli = order.cli no-lock no-error.
   if not available mobsub then next. 
   
   find customer where customer.custnum = mobsub.agrcust no-lock.
   disp order.orderid order.cli
        mobsub.activationdate customer.orgid ttorder.custid format "x(12)".
   
   if customer.orgid = ttorder.custid and order.statuscode = "2" then do: 
        
      find first border where border.msseq = mobsub.msseq no-lock.
      disp border.crstamp border.orderid.
   end.

   /* 
   pause 0.
   disp i j with 1 down.
   */
end.
*/

for each ttorder,
   first order exclusive-lock where
         order.brand   = "1" and
         order.orderid = ttorder.orderid and
         order.crstamp <= 20070801.31498 and
         order.crstamp > 20070801
by ttorder.custid
by ttorder.crstamp:
         
   i = i + 1.
     
   if order.statuscode = "2" then do: 
        
      put stream slog unformatted 
         order.orderid  chr(9)
         order.cli      chr(9)
         order.msseq    chr(9)
         order.statuscode skip.
         
      order.statuscode = "7".
      
      fMarkOrderStamp(Order.OrderID,
                     "Change",
                      0.0).

      j = j + 1.
      CREATE Memo.
      ASSIGN Memo.Brand     = gcBrand
             Memo.HostTable = "Order"
             Memo.KeyValue  = STRING(Order.OrderID)
             Memo.CustNum   = Order.CustNum
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = "ari" 
             Memo.MemoTitle = "Order cancelled"
             Memo.MemoText  = "Double order, YTS-65".
             Memo.CreStamp  = fMakeTS().
   end.

   pause 0.
   disp i j with 1 down.

end.





