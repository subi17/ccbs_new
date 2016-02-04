{Syst/testpaa.i}
katun = "ari".

def var i as int no-undo.
def var lccli as char no-undo.
def var ldadate as date no-undo.

def stream slog.

def temp-table ttcli no-undo
   field cli as char
   index cli cli.

do ldadate = 11/1/10 to 11/30/10:

   for each mobcdr no-lock use-index gsmbnr where
            mobcdr.datest = ldadate and
            mobcdr.gsmbnr = "622ff":

      i = i + 1.
      pause 0.
      disp i ldadate with 1 down.
      
      find first ttcli where ttcli.cli = mobcdr.cli no-error.
      if not available ttcli then do:
         create ttcli.
         ttcli.cli = mobcdr.cli.
      end.
   end.
end.

output stream slog to /apps/yoigo/tms_support/201011/aam_yot1032_rated.log
    append.
    
i = 0.
for each ttcli:

    i = i + 1.
    pause 0.
    put screen row 18 "Rerate " + ttcli.cli + " qty: " + string(i).

    RUN Rate/cli_rate.p (ttcli.cli,
                    11/1/10,
                    11/30/10,
                    true).
                    
    put stream slog unformatted
       ttcli.cli skip.
end.

output stream slog close.



