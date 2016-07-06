DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

def stream sout.
output stream sout to dccli_migration.txt.

FOR EACH dccli NO-LOCK use-index PerContractID:

   i = i + 1.

   if i mod 1000 = 0 THEN DO:
      disp dccli.msseq dccli.percontractid i j with frame a.
      pause 5 no-message.
   END.

   if dccli.percontractid eq ? or
      dccli.percontractid <= 0 then do:
      put stream sout unformatted
         dccli.msseq ";"
         dccli.percontractid ";"
         "ERROR:Empty percontracid" skip.
      next.
   end.

   FIND FIRST dccli_new NO-LOCK where
      dccli_new.percontractid = dccli.percontractid no-error.

   IF AVAIL dccli_new then do:
      put stream sout unformatted
         dccli.msseq ";"
         dccli.percontractid ";ERROR:Record migrated or exists with the same percontracid" skip.
       next.
   end.

   create dccli_new.
   buffer-copy dccli to dccli_new.

end.

