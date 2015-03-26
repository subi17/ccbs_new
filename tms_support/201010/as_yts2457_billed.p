def stream slog.
output stream slog to as_yts2457_billed.txt.
DEFINE VARIABLE lcMsisdn AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMsSeq AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttLasku
   FIELD cli like subinvoice.cli
   FIELD msseq like subinvoice.msseq
   FIELD qty as int
   FIELD invamt like invoice.invamt
INDEX msseq IS PRIMARY UNIQUE msseq. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
looppi:
FOR EACH invoice where
         invoice.brand = "1" and
         invoice.invdate >= 11/1/2009 and
         invoice.invtype = 1 and
         invoice.crinvnum = 0 NO-LOCK:

   lcMsSeq = "".
   lcMsisdn = "".

   j = j + 1.
   if j mod 10000 = 0 then do:
      disp j invoice.invdate.
      pause 0.
   end.

   FOR EACH subinvoice of invoice NO-LOCK:

      FOR EACH invrow of subinvoice where
         invrow.billcode = "13104020" NO-LOCK:
         
         find first ttLasku where
            ttLasku.msseq = subinvoice.msseq EXCLUSIVE-LOCK no-error.
         
         IF NOT AVAIL ttLasku then do:
            create ttLasku.
            assign
               ttLasku.msseq = subinvoice.msseq
               ttLasku.cli = subinvoice.cli.
         end.
         ttLasku.qty = ttLasku.qty + 1.
         ttLasku.invamt = ttLasku.invamt + invrow.amt.
         
      end.
   end.

   FOR EACH ttLasku NO-LOCK:
      
      put stream slog unformatted 
         invoice.custnum "|"
         ttLasku.cli "|"
         ttLasku.msseq "|" 
         "13104020" "|"
         invoice.invdate format "99-99-9999" "|"
         invoice.extinvid "|"
         ttLasku.qty "|"
         ttLasku.invamt skip.
   end.
   empty temp-table ttLasku.

end.
