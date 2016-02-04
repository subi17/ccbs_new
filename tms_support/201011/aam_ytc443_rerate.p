{Syst/testpaa.i}
katun = "qvantel".

def var i as int no-undo.
def var j as int no-undo.
def var lhrate as handle no-undo.

def temp-table ttcli no-undo
   field cli as char
   index cli cli.

def stream slog.

RUN Rate/cli_ratep.p persistent set lhrate.
run pinitializererate in lhrate.

for each mobcdr no-lock use-index date where
         mobcdr.datest = 11/23/10 and
         mobcdr.timest >= 75600 and
         mobcdr.spocmt = 93 and
         mobcdr.errorcode = 8001:
         
   i = i + 1.

   pause 0.
   disp i j with 1 down.
   
   if can-find(first ttcli where ttcli.cli = mobcdr.cli) then next.
   create ttcli.
   ttcli.cli = mobcdr.cli.
   j = j + 1.
end.

for each mobcdr no-lock use-index date where
         mobcdr.datest = 11/24/10 and
         mobcdr.timest < 3600 and
         mobcdr.spocmt = 93 and
         mobcdr.errorcode = 8001:
         
   i = i + 1.

   pause 0.
   disp i j with 1 down.
   
   if can-find(first ttcli where ttcli.cli = mobcdr.cli) then next.
   create ttcli.
   ttcli.cli = mobcdr.cli.
   j = j + 1.
end.

message j "subscriptions"
view-as alert-box.
  
         
 
output stream slog to /apps/yoigo/tms_support/201011/aam_ytc443_rated.log
    append.
    
i = 0.
for each ttcli:

    i = i + 1.
    pause 0.
    put screen row 18 "Rerate " + ttcli.cli + " qty: " + string(i).
    
    if i < 25000 or i > 30000 then next.

    run prunrerate in lhrate (ttcli.cli,
                    11/1/10,
                    11/30/10,
                    true).
                    
    put stream slog unformatted
       ttcli.cli skip.
end.

output stream slog close.
