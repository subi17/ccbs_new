output to "/apps/yoigo/tms_support/billing/yda_169_unused_fat_migrated.txt".
def var litransperiod as int no-undo.
def var liOldtransperiod as int no-undo.
def var llsimulate as log no-undo init false.
def var licount as int no-undo.
def buffer bfatime for fatime.
def buffer bbfatime for fatime.

loop:
for each mobsub where not mobsub.paytype no-lock:
    for each fatime where 
             fatime.Brand = "1" and
             fatime.cli = mobsub.cli and
             fatime.msseq = mobsub.msseq and
             fatime.invnum = 0 and
             fatime.TransPeriod > 0 no-lock:
       status default fatime.cli.
       find first bfatime where
                  bfatime.brand = "1" and 
                  bfatime.fatnum = FATime.OrigFat no-lock no-error.
       if available bfatime AND bfatime.Period = fatime.TransPeriod then do:
          find first invoice where 
                     invoice.invnum = bfatime.invnum no-lock no-error.
          if avail invoice then do:
             liOldtransperiod = fatime.TransPeriod.
             litransperiod = YEAR(invoice.fromdate) * 100 + month(invoice.fromdate).
             if not llsimulate then do:
                find first bbfatime where rowid(bbfatime) = rowid(fatime) exclusive-lock.
                if avail bbfatime then bbfatime.TransPeriod = litransperiod.
             end. /* if not llsimulate then do: */
             
             put unformatted string(mobsub.custnum) + "|" + mobsub.cli + "|" +
                             fatime.FTGrp + "|" + string(fatime.period) + "|" +
                             string(liOldtransperiod) + "|" + string(invoice.invnum) +
                             "|" + string(litransperiod) skip.
          end.
       end.   
    end.
end.
output close.
