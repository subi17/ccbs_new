output to yts2294_unbilled_prepaid_to_error.txt.

def buffer bmobcdr for mobcdr.
FOR EACH mobcdr where
   mobcdr.datest  < 2/1/2010 and
   mobcdr.billcode begins "PRE" NO-LOCK:
   
   find invseq where
        invseq.invseq = mobcdr.invseq NO-LOCK.
   if invseq.billed then next.
   if errorcode ne 0 then next.

   find bmobcdr where
       recid(bmobcdr) eq recid(mobcdr) EXCLUSIVE-LOCK.
   
   put unformatted 
      bmobcdr.eventtype "|"
      bmobcdr.cli "|" 
      bmobcdr.gsmbnr "|" 
      bmobcdr.datest "|" 
      string(bmobcdr.timestart,"HH:MM:SS") "|" 
      bmobcdr.billcode "|" 
      bmobcdr.amount "|" 
      bmobcdr.errorcode "|" 
      bmobcdr.invseq "|" 
      recid(bmobcdr) skip.
   
   assign
      bmobcdr.errorcode = 1020
      bmobcdr.invseq = 0.

   release bmobcdr.

end. 

