def stream slog.
output stream slog to /apps/snet/200708/aam_yts90.log append.

find termmobsub where cli = "615575627" exclusive-lock no-error.

if available termmobsub then do:

   disp termmobsub.msseq termmobsub.msstat termmobsub.custnum 
        termmobsub.clitype.

   create mobsub.
   buffer-copy termmobsub to mobsub.

   export stream slog termmobsub.
   delete termmobsub.

   for each msowner exclusive-lock where
            msowner.cli = mobsub.cli
   by msowner.tsend desc:
      msowner.tsend = 99999999.99999.
      disp tsbeg tsend msseq custnum invcust.
      leave.
   end.
end.

