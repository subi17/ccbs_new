{testpaa.i}
katun = "ari".
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

def temp-table ttreq no-undo
   field invnum as int
   index invnum invnum.
   
def temp-table ttorder no-undo
   field orderid as int
   index orderid orderid.
   
def stream sread.
input stream sread from /apps/snet/200910/NewNumbersTerminated.txt.

def stream slog.
output stream slog to /apps/snet/200910/aam_yot196.log append.

function fwlog returns logic
   (icmessage as char):
   
   put stream slog unformatted
      liorderid "|"
      icmessage skip.
      
end function.

for each msrequest no-lock use-index custnum where
         msrequest.brand = "1" and
         msrequest.custnum = 400 and
         msrequest.reqtype = 22 and
         lookup(string(msrequest.reqstat),"4,9") = 0:
   create ttreq.
   ttreq.invnum = msrequest.reqiparam1.
end.
         
ldactstamp = fmake2dt(today,time). 

message "start reading"
view-as alert-box.

repeat:
   import stream sread unformatted lcline.

   liorderid = integer(trim(entry(1,lcline,chr(9)),"'")) no-error.
   if error-status:error then next.

   i = i + 1.

   pause 0.
   disp i j k with 1 down.
   
   if can-find(first ttorder where ttorder.orderid = liorderid) then next.
   create ttorder.
   ttorder.orderid = liorderid.
   
   disp liorderid format ">>>>>>>>>9".

   find first order where
              order.brand = "1" and
              order.orderid = liorderid no-lock no-error.

   if not available order then do:
      fwlog("Invalid orderid").
      next.
   end.

   j = j + 1.
      
   if order.mnpstatus > 0 then do:
      fwlog("MNP order").
      next.
   end.
      
   /*
   if order.statuscode ne "7" then do:
      fwlog("Wrong order status").
      next.
   end.
   */
   
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

   find first orderdelivery of order no-lock no-error.
   if not available orderdelivery then do:
      /* these are accepted */
      /*
      fwlog("Logistic status not available" + 
            " (" + order.statuscode + "/" + order.orderchannel + ")").
      next.
      */
   end.
   else if lookup(string(orderdelivery.lostatusid),"3,12,13,22") = 0 then do:
      fwlog("Invalid logistic status").
      next.
   end.
       
   /*
   disp order.orderid
        order.orderchannel
        order.mnpstatus
        order.statuscode
        order.crstamp
        invoice.invamt.
   */
   
   k = k + 1.

   lireq = fFullCreditNoteRequest(invoice.custnum,
                                  invoice.invnum,
                                  "1011",     /* reason: cancelled */
                                  0,          /* no returns */
                                  false,      /* don't release */
                                  ldactstamp, 
                                  false,      /* sms */
                                  "YOT-188",
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
          Memo.MemoTitle = "CANCELLED ORDER"
          Memo.MemoText  = "Credited by Yoigo, YOT-188".
          Memo.CreStamp  = fmakets().
   
   create ttreq.
   ttreq.invnum = invoice.invnum.
   
   fwlog("ok").

end.

input stream sread close.
output stream slog close.

disp i j k.

