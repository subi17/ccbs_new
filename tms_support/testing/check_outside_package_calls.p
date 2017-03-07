{Func/timestamp.i}
def var lltrue as log no-undo.
def var ldeTime as dec no-undo.
def var ldFromDate as date no-undo.
def var lcfile as char no-undo.
ldFromDate = DATE(MONTH(Today),1,YEAR(Today)).

lcfile = "/apps/yoigo/tms_support/testing/outside_package_call_" + string(time) + ".txt".

output to value(lcfile).

for each mobsub no-lock where mobsub.paytype = false:
   FIND FIRST MServiceLimit NO-LOCK WHERE
              MServiceLimit.MsSeq   = mobsub.MsSeq AND
              MServiceLimit.dialtype = 7 and
              MServiceLimit.FromTs  <=  fMakeTS() and
              MServiceLimit.EndTS  >= 99999999.99999 no-error.
   if not avail MServiceLimit then next.
   lltrue = false.            

   status default mobsub.cli.

   for each mobcdr where 
            mobcdr.cli = mobsub.cli and
            mobcdr.datest >= ldFromDate  and
            mobcdr.datest <= today and
            mobcdr.eventtype = "gprs" no-lock:
      IF Mobcdr.errorcode NE 0         THEN NEXT.
      IF MobCDR.BillCode NE "14100001" THEN NEXT.
      ldeTime  = fMake2Dt(Mobcdr.datest, Mobcdr.TimeStart).
      if ldeTime >= MServiceLimit.FromTs then do:
         lltrue = true.
         leave.
      end.
   end.
   if lltrue then put unformatted mobsub.cli + "|" + mobsub.clitype skip.
end.   
output close.
