output to /apps/snet/200909/as_yob126.txt.
FOR EACH mobsub where
   mobsub.brand = "1" and
   mobsub.clitype = "cont2" NO-LOCK:
   put unformatted mobsub.msseq " " mobsub.cli " " 
                   mobsub.custnum " " mobsub.msstatus skip.
END.
