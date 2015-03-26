
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 19.10.07
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

define temp-table ttMsRange no-undo
   field  cli as integer 
   index cli cli ascending.

DEFINE VARIABLE deCliStart AS DECIMAL NO-UNDO.
DEFINE VARIABLE deCliPrev AS DECIMAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE lfirst AS LOGICAL NO-UNDO init true.

FOR EACH msisdn no-lock where
   brand = "1" and
   validto = 99999999.99999 and
   (statuscode = 0 or statuscode = 1)and
   custnum = 0 and 
   msseq = ? and
   cli begins "622":
   find first msisdnnumber where msisdnnumber.cli = msisdn.cli and (rank eq 0/* or rank eq 1*/) no-lock no-error.
   if not avail msisdnnumber then next.
   create ttMsRange.
   ttMsRange.cli = integer(msisdn.cli).
   i = i + 1.
   if i mod 10000 = 0 then display i.
end.

def stream excel.
OUTPUT STREAM excel TO "/apps/snet/200711/free_range_gold_excluded3.txt".
for each ttMsRange use-index cli:
   if lfirst then do: 
      deCliStart = ttMsRange.cli.
      deCliPrev  = ttMsRange.cli.
      lfirst = false.
   end.   
   if deCliPrev - ttMsRange.cli = - 1 then do:
      deCliPrev = ttMsRange.cli.
      next.
   end.
   else do:
      PUT STREAM excel UNFORMATTED deCliStart "-" deCliPrev 
      " " (deCliPrev - deCliStart + 1) SKIP.
      deCliStart = ttMsRange.cli.
      deCliPrev  = ttMsRange.cli.
   end.   
end.

find last ttMsRange use-index cli.
if deCliStart ne ttMsRange.cli then 
   PUT STREAM excel UNFORMATTED deCliStart "-" deCliPrev 
   " " (deCliPrev - deCliStart + 1) SKIP.

OUTPUT STREAM excel CLOSE.

