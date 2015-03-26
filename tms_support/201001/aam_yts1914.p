def var i as int no-undo.

def buffer breq for msrequest.

def stream slog.
output stream slog to /apps/snet/201001/aam_yts1914.log.

put stream slog unformatted
   "Subscr.ID"  chr(9)
   "MSISDN"     chr(9)
   "Customer"   chr(9)
   "Old Type"   chr(9)
   "Contract"   chr(9)
   "Penalty Fee" chr(9)
   "Billed"      skip.
   
for each msrequest no-lock where
         brand = "1" and
         reqtype = 0 and
         reqstat = 2 and
         actstamp >= 20091201 and
         reqcparam2 = "cont5" and
         lookup(reqcparam1,"cont,cont2,cont4") > 0,
   first breq no-lock where
         breq.origrequest = msrequest.msrequest and
         breq.reqtype = 9 and
         breq.createfees = true,
   first singlefee no-lock where
         singlefee.brand = "1" and
         singlefee.hosttable = "mobsub" and
         singlefee.keyvalue = string(msrequest.msseq) and
         singlefee.billcode = "termperiod" and
         singlefee.billper >= 200912 and
         singlefee.amt > 0:

   i = i + 1.

   /*
   disp i  
        msrequest.cli 
        msrequest.msseq 
        msrequest.actstamp 
        msrequest.reqcparam1 format "x(10)" 
        breq.reqcparam3 format "x(10)".
   */
   
   put stream slog unformatted
     msrequest.msseq  chr(9)
     msrequest.cli    chr(9)
     msrequest.custnum chr(9)
     msrequest.reqcparam1 chr(9)
     breq.reqcparam3 chr(9)
     singlefee.amt  chr(9)
     string(singlefee.invnum > 0,"Yes/No") skip.
     
end.