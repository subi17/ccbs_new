
def var licount as int no-undo.
def var llheader as log no-undo.

output to "/apps/yoigo/tms_support/testing/update_contf20d_limits_terminated.txt".

for each termmobsub where
         termmobsub.brand = "1" and
         termmobsub.clitype = "contf" no-lock,
   first msowner where
         msowner.msseq   = termmobsub.msseq and
         msowner.tsend  >= 20130301 and
         msowner.clitype = termmobsub.clitype:

    llheader = true.
    status default string(licount).

    for each mservicelimit where
             mservicelimit.msseq  = msowner.msseq and
             mservicelimit.slseq = 43 and
             mservicelimit.dialtype = 7 and
             mservicelimit.endts >= 20130301 exclusive-lock:

       if llheader then do:
          put unformatted msowner.cli + "|" + string(msowner.msseq) + "|".
          llheader = false.
       end.

       put unformatted string(mservicelimit.dialtype) + "|" +
                       string(mservicelimit.inclamt)  + "|".

       mservicelimit.inclamt = 1024.

       put unformatted string(mservicelimit.inclamt) + "|".
    end.

    if llheader = false then do:
       licount = licount + 1.
       put unformatted skip.
    end.
end.

output close.