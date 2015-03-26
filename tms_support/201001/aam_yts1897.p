{testpaa.i}
katun = "ari".

def var i as int no-undo.

for each mobsub no-lock where 
        brand = "1" and
        clitype = "contrd2",
   each mservicelimit no-lock where 
        mservicelimit.msseq = mobsub.msseq,
  FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
        ServiceLimit.SLSeq     = MServiceLimit.SLSeq and
        servicelimit.groupcode = "contd2act",
  first servicelcounter no-lock where
        servicelcounter.msseq = mobsub.msseq and
        servicelcounter.period = 201001 and
        servicelcounter.slseq = mservicelimit.slseq:

    if servicelcounter.amt / (1024 * 1024) < 3072 then next.

       i = i + 1.
       pause 0.
       disp i format ">>>9" mobsub.cli.
       disp mservicelimit except msseq slseq dialtype inclamt inclunit.
       disp servicelimit.groupcode
            servicelcounter.amt / (1024 * 1024) column-label "Counter".

   run cli_rate.p (mobsub.cli,
                   1/1/10,
                   1/31/10,
                   true).
end.

disp i.