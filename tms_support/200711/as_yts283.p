{roamtariff.i}

{Syst/testpaa.i}
katun = "anttis".

def stream sread.
input stream sread from /apps/snet/200711/as_yts283.txt.

def stream slog.
output stream slog to /apps/snet/200711/as_yts283.log.

def var lcline    as char no-undo.
def var lcPlmn    as char  no-undo.
def var lcOldStock as char no-undo.
def var i         as int  no-undo.
def var ldtDate   as date no-undo.
def var lcFileSeq as char no-undo.
def var lcdate    as char no-undo.
repeat:

   import stream sread unformatted lcline.
   /*yyyy-mm-dd*/ 
   assign 
      lcdate = entry(1,lcline, " ")
      ldtDate = DATE(
         int(substring(lcDate,6,2)),
         int(substring(lcDate,9,2)),
         int(substring(lcDate,1,4)))
      lcPlmn = SUBSTRING(entry(2,lcline," "), 8, 5)
      lcFileSeq = SUBSTRING(entry(2,lcline," "), 13, 5)
      i     = i + 1.
   
   find first roamoper where roamoper.plmn = lcPlmn no-lock no-error.
   if not avail roamoper then do:
      MESSAGE "Not found: " +  lcplmn VIEW-AS ALERT-BOX.
      next.
   END.   
   
   RUN tms_support/200711/as_yts283_tapfilecr.p(lcPlmn,ldtDate - 1,ldtDate - 1,"",FALSE,lcFileSeq).

put stream slog unformatted
       lcPlmn           chr(9)
       INT(lcFileSeq)        chr(9)
       ldtDate          skip.
end.



