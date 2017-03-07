{Func/timestamp.i}

def stream slog.

def var ldtdate as date no-undo.
def var i       as int  no-undo.
def var ldfrom  as dec  no-undo.
def var ldto    as dec  no-undo.
def var lccli   as char no-undo.

pause 0.
update ldtdate 
   format "99-99-99"
   label "Denial Date"
   help "Date on which billing denials were active"
   with overlay row 10 centered title " DENIALS " side-labels frame fcrit.
hide frame fcrit no-pause.

if ldtdate = ? then return.

output stream slog to value("/tmp/denybill_" + 
                            string(ldtdate,"999999") + 
                            ".txt").

put stream slog unformatted
   "MSISDN"             chr(9)
   "Subscription ID"    chr(9)
   "Customer"           chr(9)
   "Code"               chr(9)
   "Valid From"         skip.

assign
  ldfrom = fmake2dt(ldtdate,0)
  ldto   = fmake2dt(ldtdate,86399).

for each limit no-lock where
         limit.tmruleseq = 0 and
         limit.limitid   = 0 and
         limit.limittype = 3 and
         limit.todate >= ldtdate and
         limit.fromdate <= ldtdate
by limit.custnum:
         
   i = i + 1.
   if i < 100 or i mod 100 = 0 then do:
      pause 0.
      disp i with 1 down.
   end.
   
   lccli = "".
   
   find first mobsub where mobsub.msseq = limit.msseq no-lock no-error.
   if available mobsub and mobsub.invcust = limit.custnum then 
      lccli = mobsub.cli.
      
   else for first msowner no-lock where
                  msowner.msseq = limit.msseq and
                  msowner.tsend >= ldto and
                  msowner.tsbeg <= ldfrom and
                  msowner.invcust = limit.custnum:
      lccli = msowner.cli.            
   end.

   if lccli = "" then do:
      find first termmobsub where termmobsub.msseq = limit.msseq 
      no-lock no-error.
      if available termmobsub and termmobsub.invcust = limit.custnum then 
         lccli = termmobsub.cli.
   end.
            
   put stream slog unformatted
      lccli            chr(9)
      limit.msseq      chr(9)
      limit.custnum    chr(9)
      limit.limitamt   chr(9)
      limit.fromdate   skip.
      
end.

disp i.

output stream slog close.


