DEFINE TEMP-TABLE ttFATs
field clitype AS CHAR label "SubType"
FIELD amt AS dec format ">>>>>>9.99" label "Amount"
field count as int label "Count"
INDEX i IS PRIMARY UNIQUE clitype. 

DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcClitype AS CHARACTER NO-UNDO. 
FOR EACH fatime where
   fatime.brand = "1" and
   fatime.ftgrp = "bono8cp" NO-LOCK:

   if fatime.invnum = 0 then next.
   
   find first msowner NO-LOCK where
        msowner.msseq = fatime.msseq and 
        msowner.tsbegin < 20110101 and
        msowner.tsend > 20101201 no-error.

   IF NOT AVAIL msowner then next.

   j = j + 1.
   if j mod 1000 = 0 then disp j.
   pause 0.

   find invoice where
        invoice.invnum = fatime.invnum  NO-LOCK.

   if invoice.invdate eq 2/1/2011 then next.

   if invoice.invdate ne 1/1/2011 or
      invoice.invtype ne 1 then do:
      MESSAGE "foo" invoice.invdate invoice.extinvid invoice.invtype 
      VIEW-AS ALERT-BOX.
      next.
   end.

   find first ttFATs where
              ttFATs.clitype = lcClitype EXCLUSIVE-LOCK no-error.
   IF NOT AVAIL ttFATs then do:
      create ttFATs.
      ttFATs.clitype = lcClitype.
   end.
   
   assign
      ttFATs.amt   = ttFATs.amt + fatime.used
      ttFATs.count = ttFATs.count + 1.

end.

FOR EACH ttFATs NO-LOCK:
   disp ttFATs.
END.
