/* RUN AFTER YDR-2568-tms-stcs-to-la-de-casa branch deployment */ 
def stream sout.
output stream sout to yts_11096.log.

FOR EACH clitype no-LOCK where
   clitype.tarifftype = 2:

   FOR EACH mobsub EXCLUSIVE-LOCK where
            mobsub.brand = "1" and
            mobsub.clitype = clitype.clitype and
            mobsub.imsi = "":

      if mobsub.msstatus eq 16 then next.

      if mobsub.msstatus ne 17 then do:
         put stream sout unformatted
            mobsub.msseq ";"
            mobsub.cli ";"
            mobsub.imsi ";"
            mobsub.clitype ";"
            mobsub.msstatus skip.
         mobsub.msstatus = 17.
      end.
   end.

end.
