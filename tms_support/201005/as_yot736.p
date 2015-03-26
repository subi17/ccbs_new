input from as_yot736.input.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
def stream slog.
output stream slog to /store/riftp/doc1/copy/yot_736.txt.
DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO. 

repeat:
   
   import unformatted lcLine.
   
   find invoice where
      invoice.brand = "1" AND
      invoice.extinvid = lcLine NO-LOCK.

   if month(invoice.invdate) = 1 then
      liPeriod = (YEAR(invoice.invdate) - 1) * 100 + 12.
   else liPeriod = year(invoice.invdate) * 100 + month(invoice.invdate) - 1.

   put stream slog unformatted invoice.cli chr(9) liPeriod skip.
end.
