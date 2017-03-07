{Syst/testpaa.i}
katun = "ari".
{/home/ari/work/fcreditreq_chk.i}
{Func/timestamp.i}

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
   
def stream sread.
input stream sread from /apps/snet/200907/CreditNotes.csv.

def stream slog.
output stream slog to /apps/snet/200907/aam_ycm1651.log append.

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
         
ldactstamp = fmake2dt(today,time + 600). 

repeat:
   import stream sread unformatted lcline.

   liorderid = integer(trim(entry(1,lcline,chr(9)),"'")) no-error.
   if error-status:error then next.

   lccli = trim(entry(4,lcline,chr(9)),"'").
   
   i = i + 1.

   pause 0.
   disp i j k with 1 down.
   
   /* 
   disp liorderid format ">>>>>>>>>9"
        lccli format "x(12)".
   */
   find first order where
              order.brand = "1" and
              order.orderid = liorderid no-lock no-error.

   if not available order or order.cli ne lccli then do:
      fwlog("Invalid orderid").
      next.
   end.

   j = j + 1.
      
   if order.orderchannel = "pos" then do:
      fwlog("POS channel").
      next.
   end.
      
   if order.mnpstatus ne 8 then do:
      fwlog("Wrong MNP status").
      next.
   end.
      
   if order.statuscode ne "7" then do:
      fwlog("Wrong order status").
      next.
   end.

   if order.crstamp < 20080101 then do:
      fwlog("Older than 2008").
      next.
   end.

   find first orderpayment of order no-lock.
   if not available orderpayment or orderpayment.method ne 1 then do:
      fwlog("Not payment on delivery").
      next.
   end.
      
   find first invoice where invoice.invnum = order.invnum no-lock no-error.
   if order.invnum = 0 or not available invoice then do:
      fwlog("Cash invoice not available").
      next.
   end.

   if invoice.invtype ne 6 then do:
      fwlog("Incorrect invoice type").
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
      
   /*
   disp order.orderid
        order.orderchannel
        order.mnpstatus
        order.statuscode
        order.crstamp.
   disp orderpayment.method.
   */
   
   k = k + 1.
   if k mod 10000 = 0 then do:
      ldactstamp = fmake2dt(today,integer(time + (k / 10000) * 3600)).
      /*
      message "new batch:" k fts2hms(ldactstamp)
      view-as alert-box.
      */
      pause 0.
      disp fts2hms(ldactstamp) format "x(20)" with 1 down.
   end.

   lireq = fFullCreditNoteRequest(invoice.custnum,
                                  invoice.invnum,
                                  "1012",     /* reason: failed mnp */
                                  0,          /* no returns */
                                  false,      /* don't release */
                                  ldactstamp, 
                                  false,      /* sms */
                                  "",
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
          Memo.MemoTitle = "CLOSED ORDER (ACAN)"
          Memo.MemoText  = "Credited by Yoigo, YCM-1649".
          Memo.CreStamp  = fmakets().
    
   create ttreq.
   ttreq.invnum = invoice.invnum.
   
   fwlog("ok").

end.

input stream sread close.
output stream slog close.

disp i j k.

