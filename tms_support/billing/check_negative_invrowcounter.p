output to "/apps/yoigo/tms_support/billing/log/check_negative_invrowcounter.log".

def var i as int no-undo.
def var j as int no-undo.

for each msowner where msowner.tsend >= 20150801 no-lock,
    each invrowcounter where
         invrowcounter.msseq = msowner.msseq and
         invrowcounter.todate = 08/31/2015 no-lock:

   i = i + 1.

   if invrowcounter.amount < 0 then do:
      put unformatted invrowcounter.msseq "|"
                      invrowcounter.cli "|"
                      invrowcounter.billcode "|"
                      invrowcounter.ccn "|"
                      invrowcounter.invcust skip.
      j = j + 1.
   end.

   status default string(i) + "|" + string(j).

end.
disp i j.