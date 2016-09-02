DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO. 

def stream sout.
output stream sout to dccli_migration.txt append.

FOR EACH dccli NO-LOCK use-index PerContractID:

   i = i + 1.

   if i mod 2000 = 0 THEN DO:
      disp dccli.msseq dccli.percontractid i j with frame a.
      pause 6 no-message.
   END.

   if dccli.percontractid eq ? or
      dccli.percontractid <= 0 then do:
      put stream sout unformatted
         dccli.msseq ";"
         dccli.percontractid ";"
         "ERROR:Empty percontracid" skip.
      j = j + 1.
      next.
   end.

   FIND FIRST dccli_new NO-LOCK where
      dccli_new.percontractid = dccli.percontractid no-error.

   IF AVAIL dccli_new then do:

      BUFFER-COMPARE dccli_new TO
                     dccli SAVE RESULT IN llSameValues.

      if llSameValues then do:
         put stream sout unformatted
            dccli.msseq ";"
            dccli.percontractid ";SKIPPED:Record already migrated" skip.
      end.
      else do:
         put stream sout unformatted
            dccli.msseq ";"
            dccli.percontractid "ERROR;Other record with the same percontracid" skip.
            j = j + 1.
      end.


      next.
   end.

   do trans:
      create dccli_new.
      buffer-copy dccli to dccli_new.
      release dccli_new.
   end.

end.

