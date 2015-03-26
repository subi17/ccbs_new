{testpaa.i}
katun = "YOT-612".
{timestamp.i}

DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
END.

def stream sread.
input stream sread from /apps/snet/201003/customer_deltype_10_201003.txt.

def var licustnum as int no-undo.
def var lcline as char no-undo.
def var lcorgid as char no-undo.
def var i as int no-undo.
def var j as int no-undo.


repeat:

   import stream sread unformatted lcline.
   
   assign 
      licustnum = int(entry(1,lcline,chr(9)))
      lcorgid  = entry(2,lcline,chr(9)) no-error.
      
   if error-status:error then next.

   find first customer where customer.custnum = licustnum no-lock no-error.
   if not available customer then next.
   
   if customer.orgid ne lcorgid then do:
      message "check customer:" licustnum lcorgid customer.orgid
      view-as alert-box.
      next.
   end.

   i = i + 1.

   if customer.deltype ne 10 then do:
 
      RUN StarEventSetOldBuffer (lhCustomer).
      find current customer exclusive-lock.
      customer.deltype = 10.
      RUN StarEventMakeModifyEvent (lhCustomer).

      CREATE Memo.
      ASSIGN Memo.Brand     = gcBrand
             Memo.HostTable = "Customer"
             Memo.KeyValue  = string(customer.custnum)
             Memo.CustNum   = customer.custnum
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = katun 
             Memo.MemoTitle = "Información del cliente modificada"
             Memo.MemoText  = "Correos nos devuelve la carta. " + 
                              "Inhabilitamos el envío de la misma". 
             Memo.CreStamp  = fMakeTS().

      j = j + 1.
   end.

   pause 0.
   disp i j with 1 down.
end.

fcleaneventobjects().

