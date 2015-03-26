{testpaa.i}
katun = "ari".


def var ldadate as date no-undo.
def var i as int no-undo.
def var j as int no-undo.

def temp-table ttcli no-undo
   field cli as char
   index cli cli.
   

do ldadate = 3/1/10 to 3/23/10:
  
    for each mobcdr no-lock where   
             mobcdr.datest = ldadate and
             mobcdr.gsmbnr = "11888":
       i = i + 1.
    
       if mobcdr.errorcode = 0 then do:
          find first invseq where invseq.invseq = mobcdr.invseq no-lock.
          if invseq.billed then next.
       end.
       
       if can-find(first ttcli where ttcli.cli = mobcdr.cli) then next.
       
       create ttcli.
       ttcli.cli = mobcdr.cli.
       j = j + 1.
    end.
    
    pause 0.
    disp ldadate i j with 1 down frame a.
end.

for each ttcli:

   disp ttcli.cli format "x(12)" with frame b 30 down no-box.
   down with frame b.

   run cli_rate.p (ttcli.cli,
                   3/1/10,
                   3/31/10,
                   true).

end.
         