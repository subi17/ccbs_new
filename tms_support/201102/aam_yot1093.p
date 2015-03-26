{testpaa.i}
katun = "Qvantel".
{/home/ari/work/fcreditreq_chk.i}
{timestamp.i}

def var lcline as char no-undo.
def var liorderid as int no-undo.
def var lccli as char no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.
def var lireq as int no-undo.
def var lcerror as char no-undo.
def var ldactstamp as dec no-undo.
def var ldamt as dec no-undo.

def temp-table ttreq no-undo
   field invnum as int
   index invnum invnum.
   
def temp-table ttorder no-undo
   field orderid as int
   index orderid orderid.
   
def stream sread.
input stream sread from 
       /apps/yoigo/tms_support/201008/credit_fraud_orders_201007.txt.

def stream slog.
output stream slog to 
   /apps/yoigo/tms_support/201008/credit_fraud_orders_201007.log append.

function fwlog returns logic
   (icmessage as char):
   
   put stream slog unformatted
      liorderid "|"
      ldamt     "|"
      icmessage skip.
      
end function.

for each msrequest no-lock use-index custnum where
         msrequest.brand = "1" and
         msrequest.custnum = 400 and
         msrequest.reqtype = 22 and
         lookup(string(msrequest.reqstat),"4,9") = 0:
   create ttreq.
   ttreq.invnum = msrequest.reqiparam1.
   
   lireq = lireq + 1.
   if lireq mod 100 = 0 then do:
      pause 0.
      disp "Collect 400" lireq with 1 down frame fcoll.
   end.   
end.

hide frame fcoll no-pause.
         
ldactstamp = fmake2dt(today,time). 

message "start reading"
view-as alert-box.

repeat:
   import stream sread unformatted lcline.

   liorderid = integer(entry(2,lcline,chr(9))) no-error.
   if error-status:error then next.

   i = i + 1.

   pause 0.
   disp i j k with 1 down.
   
   if can-find(first ttorder where ttorder.orderid = liorderid) then next.
   create ttorder.
   assign 
      ttorder.orderid = liorderid
      ldamt = 0.
   
   disp liorderid format ">>>>>>>>>9".

   find first order where
              order.brand = "1" and
              order.orderid = liorderid no-lock no-error.

   if not available order then do:
      fwlog("Invalid orderid").
      next.
   end.

   j = j + 1.
      
   find first invoice where invoice.invnum = order.invnum no-lock no-error.
   if order.invnum = 0 or not available invoice then do:
      fwlog("Cash invoice not available").
      next.
   end.

   if invoice.invtype ne 6 and invoice.invtype ne 7 then do:
      fwlog("Incorrect invoice type").
      next.
   end.
      
   if invoice.invamt = 0 then do:
      fwlog("Invoice amount is 0").
      next.
   end.

   ldamt = invoice.invamt.
   
   if invoice.crinvnum > 0 or 
      can-find(first ttreq where ttreq.invnum = invoice.invnum)
   then do:
      fwlog("Already credited").
      next.
   end.
      
   if invoice.paymstate ne 0 then do:
      fwlog("Invoice paid").
      next.
   end.

   disp order.orderid
        order.orderchannel
        order.mnpstatus
        order.statuscode
        order.crstamp
        invoice.invamt.

   k = k + 1.

   lireq = fFullCreditNoteRequest(invoice.custnum,
                                  invoice.invnum,
                                  "1011",     /* reason: cancelled */
                                  0,          /* no returns */
                                  false,      /* don't release */
                                  ldactstamp, 
                                  false,      /* sms */
                                  "YOT-842",
                                  OUTPUT lcerror).

   if lireq = 0 then do:
      fwlog("Creation of credit request failed").
      next.
   end.
   
   find msrequest where msrequest.msrequest = lireq no-lock.  
   create Memo.
   assign Memo.Brand     = "1"
          Memo.HostTable = "MsRequest"
          Memo.KeyValue  = string(msrequest.msrequest)
          Memo.CustNum   = msrequest.custnum
          Memo.MemoSeq   = next-value(memoseq)
          Memo.CreUser   = msrequest.usercode 
          Memo.MemoTitle = "FRAUD ORDER"
          Memo.MemoText  = "Credited by Yoigo, YOT-842".
          Memo.CreStamp  = fmakets().

   create ttreq.
   ttreq.invnum = invoice.invnum.
   
   fwlog("ok").
   
end.

input stream sread close.
output stream slog close.

disp i j k.

