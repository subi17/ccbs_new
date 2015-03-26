DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeAmount like invrow.amt.
     DEFINE VARIABLE ldeAmtWithTax like invrow.amt.
DEFINE VARIABLE ldVATAmt AS DECIMAL NO-UNDO. 

def stream sout.
output stream sout to yob_354.txt.

{date.i}

put stream sout unformatted "MSISDN|Old clitype|New Clitype|STC timestamp|number of CDRs|CDR euro amount|ext. invoice number (if invoiced)|Invoice row amount (inc.tax)|Invoice row amount (excl.tax)" skip.
FOR EACH msrequest where
         msrequest.brand = "1" and
         msrequest.reqtype = 0 and
         msrequest.reqstatus = 2 and
         msrequest.actstamp >= 20110201 and
         msrequest.actstamp < 20110228 and
         msrequest.reqcparam1 begins "CONTRD" and
         msrequest.reqcparam2 begins "CONTRD" NO-LOCK:
     
     release invseq.
     release invoice.
     release invrow.

     i = 0.
     ldeAmount = 0.
     FOR EACH mobcdr where
               mobcdr.cli = msrequest.cli and
               mobcdr.datest >= 2/1/2011 and
               mobcdr.datest < 2/2/2011 and
               mobcdr.billcode = "14100001" NO-LOCK:
  
           i = i + 1.
           find invseq where
                invseq.invseq = mobcdr.invseq NO-LOCK.

           ldeAmount = mobcdr.amount + ldeAmount.
     END.

     IF NOT AVAIL invseq then next.

     ldeAmtWithTax = 0.
     if invseq.billed then do:
        find invoice where
            invoice.invnum = invseq.invnum NO-LOCK.
        find invrow where
             invrow.invnum = invoice.invnum and
             invrow.billcode = "14100001" and
             invrow.cli = msrequest.cli NO-LOCK.
        
         /* if VAT is included then remove it */
         IF InvRow.VATPerc > 0 AND InvRow.Amt NE 0 THEN DO:

               IF Invoice.VatIncl = false THEN
                ldVatAmt = ROUND(InvRow.Amt * InvRow.VatPerc / 100,2).
               else disp "foo".

            ldeAmtWithTax = InvRow.amt + ldVATAmt.
         END.

     end. 

     put stream sout unformatted 
         msrequest.cli "|"
         msrequest.reqcparam1 "|"
         msrequest.reqcparam2 "|"
         fts2hms(msrequest.donestamp) "|"
         i "|"
         ldeAmount "|"
         (if avail invoice then invoice.extinvid else "") "|"
          ldeAmtWithTax "|"
         (if avail invrow then invrow.amt else 0) skip.

end.

disp i.
