etime(true).
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream sout.
output stream sout to as_yts2457_96_to_error.txt.

def buffer bmobcdr for mobcdr.

FOR EACH mobcdr where
   spocmt = 96 NO-LOCK use-index spocmt:

   if mobcdr.errorcode ne 0 then next.

   find invseq where
        invseq.invseq = mobcdr.invseq NO-LOCK.

   if invseq.billed eq true then next.
   if mobcdr.roamingind = 1 then next.

   if mobcdr.rateccn ne mobcdr.spocmt then do:
      MESSAGE rateccn spocmt VIEW-AS ALERT-BOX.
      next.
   end.

   i  = i + 1.
   if i mod 10 = 0 then disp i.
   pause 0.
   
   find bmobcdr where
      recid(bmobcdr) eq recid(mobcdr) EXCLUSIVE-LOCK.

   put stream sout unformatted
      recid(bmobcdr) "|" 
      bmobcdr.invseq "|"
      mobcdr.cli "|"
      mobcdr.gsmbnr "|"
      mobcdr.datest "|" 
      mobcdr.timestart "|"
      mobcdr.amount  skip. 
               
   assign
      bmobcdr.errorcode = 9013 
      bmobcdr.invseq = 0.  

   release bmobcdr.
   
end.
disp i etime.
