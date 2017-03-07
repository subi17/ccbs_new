DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeStamp AS DECIMAL NO-UNDO. 
{Func/date.i}

def stream slog.
output stream slog to as_yts2457_unbilled_october.txt.
FOR EACH mobcdr where 
   datest >= 10/1/2010 and
   errorcode = 0 and
   rateccn = 96 NO-LOCK use-index date:
   
   if mobcdr.cli eq mobcdr.gsmbnr then next.
   
   ldeStamp = fhms2ts(mobcdr.datest, string(mobcdr.timestart,"HH:MM:SS")).

   find first msowner where
      msowner.cli = mobcdr.gsmbnr and
      msowner.tsend > ldeStamp and
      msowner.tsbegin < ldeStamp NO-LOCK no-error.

   if avail msowner then do:
      put stream slog unformatted recid(mobcdr) "|SWITCH" skip.
      pause 0.
   end.
   else put stream slog unformatted recid(mobcdr) "|ERROR" skip.
   
   disp mobcdr.rateccn mobcdr.cli mobcdr.gsmbnr datest timestart.
   pause 0.

end.
