{Func/date.i}
def stream sread.
def stream slog.
input stream sread from "/apps/snet/200711/preactiv_msisdn.txt".
output stream slog to /apps/snet/200711/as_ycm95.log.
def var lcline as char no-undo.

DEFINE VARIABLE nowi AS DECIMAL NO-UNDO.
nowi = fMakeTS().
repeat: 
   import stream sread unformatted lcline.
   find msisdn where 
      msisdn.pos = "preactivated" and
      msisdn.statuscode = 1 and
      msisdn.cli = lcLine /*and
      msisdn.validto > nowi*/
      NO-LOCK no-error.
   if avail msisdn then
      PUT STREAM slog UNFORMATTED lcline SKIP.
end.

input STREAM sread CLOSE.
output STREAM slog CLOSE.
