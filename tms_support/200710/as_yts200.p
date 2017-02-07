{roamtariff.i}

/*
FOR EACH roamgprs where
   datestart >= 9/6/2007 and
   datestart <= 9/18/2007 and
   eventtype = "GPRS" and
  
    NO-LOCK:
   
    fRoamTariff(
      false,
      buffer roamcdr,
      buffer roamgprs).
   
END.
*/


{Syst/testpaa.i}
katun = "anttis".

def stream sread.
input stream sread from /apps/snet/200710/yts200_2.txt.

def stream slog.
output stream slog to /apps/snet/200710/yts200.log.

def var lcline    as char no-undo.
def var lcPlmn    as char  no-undo.
def var lcOldStock as char no-undo.
def var i         as int  no-undo.
def var ldtDate   as date no-undo.
def var lcFileSeq as char no-undo.
repeat:

   import stream sread unformatted lcline.
   
   assign 
      ldtDate = DATE(entry(1,lcline, chr(ASC("TAB"))))
      lcPlmn = SUBSTRING(entry(2,lcline," "), 8, 5)
      lcFileSeq = SUBSTRING(entry(2,lcline," "), 13, 5)
      i     = i + 1.
   
   find first roamoper where roamoper.plmn = lcPlmn no-lock no-error.
   if not avail roamoper then do:
      MESSAGE "Not found: " +  lcplmn VIEW-AS ALERT-BOX.
      next.
   END.   
   
   RUN tms_support/200710/tapfilecr_yoi443.p(lcPlmn,ldtDate - 1,ldtDate - 1,"",FALSE,lcFileSeq).

put stream slog unformatted
       lcPlmn           chr(9)
       INT(lcFileSeq)        chr(9)
       ldtDate          skip.

end.



