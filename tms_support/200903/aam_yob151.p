{timestamp.i}

for each msrequest no-lock where
         brand = "1" and
         reqtype = 0 and
         reqstat = 2 and
         actstamp > 20090301 and
         actstamp < 20090302 and
         reqcparam1 = "contrd1",
   first mobsub no-lock where
         mobsub.msseq = msrequest.msseq,
   first invoice no-lock use-index cli where
         invoice.brand = "1" and
         invoice.cli = msrequest.cli and
         invoice.invdate = 3/1/9 and
         invoice.invtype = 1:
        
   disp msrequest.cli 
        mobsub.activationdate 
        fts2hms(msrequest.donestamp) format "x(19)"
        fts2hms(invoice.chgstamp) format "x(19)".
end.        
    
