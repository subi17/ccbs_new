def var i as int no-undo.
def var j as int no-undo.

def buffer binv for invoice.

def stream slog.
output stream slog to /apps/snet/200911/refund_request_status.txt.

put stream slog unformatted
   "Customer"   chr(9)
   "Invoice"    chr(9)
   "Amount"     chr(9)
   "Credited"   chr(9)
   "Refund Status" skip.

do i = 1 to 20:

   if i = 2 or i = 4 or i = 9 then next.
   
   for each msrequest no-lock where
            brand = "1" and
            reqtype = 23 and
            reqstat = i:
            
       j = j + 1.
       pause 0.
       disp i j with 1 down.
       
       /*
       disp actstamp reqstat format ">9" msseq custnum.     
       */
       find invoice where invoice.invnum = MsRequest.ReqIParam1 no-lock.
       find first binv where binv.invnum = invoice.crinvnum no-lock.
       
       put stream slog unformatted
          msrequest.custnum chr(9)
          invoice.extinvid  chr(9)
          msrequest.reqdparam1 chr(9)
          binv.invdate  chr(9)
          msrequest.reqstatus skip.
   end.
end.


