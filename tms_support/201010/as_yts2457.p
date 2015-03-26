DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream slog.
output stream slog to as_yts2457.txt.
FOR EACH mobcdr where 
   datest >= 9/2/2010 and
   datest < 10/1/2010 and
   errorcode = 1001 and
   rateccn = 96 NO-LOCK use-index date:

   if mobcdr.cli eq mobcdr.gsmbnr then next.
   
   put stream slog unformatted recid(mobcdr) skip.

   disp mobcdr.rateccn mobcdr.cli mobcdr.gsmbnr datest timestart.
   pause 0.

end.
