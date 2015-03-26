{testpaa.i}
{forderstamp.i}

def var llclose as log no-undo.

for first order exclusive-lock where
          order.brand  = "1" and 
          order.orderid = 277696:    /* 277700 */
          
   disp cli crstamp orderchannel statuscode invnum 
        mnpstatus  with 2 down.
           
   for each msrequest no-lock where
            msrequest.msseq = order.msseq:
        disp reqtype actstamp
             reqstat memo view-as editor size 50 by 4
             with 1 down.
   end.

   for each solog no-lock where
            solog.msseq = order.msseq:
             
      disp solog.stat solog.commline format "x(20)" with 10 down.
   end.

   if order.statuscode = "7" then leave.
   
   message "Close?" 
   view-as alert-box question
   buttons yes-no
   set llclose.
   
   if not llclose then leave.
   
   order.statuscode = "7".
      
   fMarkOrderStamp(Order.OrderID,
                   "Change",
                   0.0).

   CREATE Memo.
   ASSIGN Memo.Brand     = gcBrand
          Memo.HostTable = "Order"
          Memo.KeyValue  = STRING(Order.OrderID)
          Memo.CustNum   = Order.CustNum
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = "ari" 
          Memo.MemoTitle = "Order cancelled"
          Memo.MemoText  = "Double icc, YTS-93".
          Memo.CreStamp  = fMakeTS().
   
end.
