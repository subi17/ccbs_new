
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 22.06.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Func/date.i}


DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE STREAM sout.
output stream sout to /apps/snet/200806/as_yts820.txt.
def buffer m2 for mnpprocess.

FOR EACH mnpprocess NO-LOCK WHERE
   mnpprocess.statuscode = 7 and
   mnpprocess.createdts >= 20080601:
   
   FIND FIRST order where
      order.brand = "1" and
      order.orderid = mnpprocess.orderid NO-LOCK NO-ERROR.
   
   if order.statuscode ne "7" then do:
      
      FIND FIRST m2 where m2.orderid = mnpprocess.orderid and
         rowid(m2) ne rowid(mnpprocess) NO-LOCK NO-ERROR.
      IF Avail m2 then NEXT.
      i = i + 1.

      put stream sout unformatted order.orderid " " order.statuscode " -> 7"  skip.
    
      find current order EXCLUSIVE-LOCK NO-ERROR.
      Order.StatusCode = "7". 
      
      CREATE Memo.
         ASSIGN
            Memo.CreStamp  = fMakeTS() 
            Memo.Brand     = "1"
            Memo.HostTable = "Order"
            Memo.KeyValue  = STRING(Order.OrderId)
            Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
            Memo.CreUser   = "anttis" 
            Memo.MemoTitle = "Order Closed"
            Memo.MemoText  = "Fixed orders with ACAN mnp status and on going orders status. (YTS-816)".

   END.

END.
disp i.
output stream sout close.
