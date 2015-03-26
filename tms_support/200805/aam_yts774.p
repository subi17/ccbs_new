{timestamp.i}

def var i as int no-undo.

def buffer binv for invoice.

def stream slog.
output stream slog to /apps/snet/200805/aam_yts774.log append.

for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate >= 5/21/8 and
         invtype >= 6 and invtype <= 7 and
         paymstate > 0 and
         crinvnum = 0,
   first payment exclusive-lock where
         payment.invnum = invoice.invnum and
         payment.paymsrc = "br":

   i = i + 1. 


   find binv where recid(binv) = recid(invoice) exclusive-lock.
   
   put stream slog unformatted
      invoice.invnum "|"
      invoice.paidamt "|"
      payment.voucher "|"
      payment.paymamt "|"
      payment.posting[1] skip.
      
   assign payment.paymamt = 0
          payment.totamt  = 0
          payment.posting = 0
          binv.paidamt    = 0
          binv.paymstate  = 0
          binv.paymdate   = ?.
   
   /*
   disp invdate invamt paidamt paymstate 
        can-find(first payment where payment.invnum = invoice.invnum)
        fts2hms(invoice.chgstamp) format "x(20)"
        invoice.custnum.
        
         .
   */      
   pause 0.
   disp i with 1 down.
   
end.

