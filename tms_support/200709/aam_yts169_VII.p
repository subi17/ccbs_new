{Syst/testpaa.i}
katun = "ari".

def var i as int no-undo.
def var j as int no-undo.

def temp-table ttcli no-undo
    field cli as char
    index cli cli.
    
for each invseq no-lock where
         invseq.billed = false and
         invseq.msseq = 0 and
         invseq.todate > 6/1/7:
         
   if invseq.fromdate < 6/1/7 then next.
   
   i = i + 1.
   
   pause 0.
   disp i j with 1 down.
   
   empty temp-table ttcli.
   
   for each mobcdr exclusive-lock where
            mobcdr.invcust = invseq.custnum and
            mobcdr.invseq  = invseq.invseq:
      /*
      mobcdr.invseq = 0.      
      */
      find first ttcli where ttcli.cli = mobcdr.cli no-error.
      if not available ttcli then do:
         create ttcli.
         ttcli.cli = mobcdr.cli.
      end.
      j = j + 1.
      
   end.

   for each ttcli:
      run cli_rate.p (ttcli.cli,
                      invseq.fromdate,
                      invseq.todate,
                      true).
   end.
   
end.

disp i j.

