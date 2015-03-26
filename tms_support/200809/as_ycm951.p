DEFINE VARIABLE liDate AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

def buffer bufsim for sim.

def stream sout.
output stream sout to /apps/snet/200809/as_ycm951.txt.
output to /apps/snet/200809/as_ycm951_sims_deleted.d.

FOR EACH SIM NO-LOCK WHERE
   SIM.Brand = "1":
   liDate = int(substring(SIM.ICC,9,2) + substring(SIM.ICC,7,2)).

   if lidate < 804 and sim.simstat = 1 then do:
      i = i + 1.
      find bufsim where rowid(bufsim) eq rowid(sim) EXCLUSIVE-LOCK NO-ERROR.
      put stream sout unformatted sim.icc skip.
      export sim.
      delete bufsim.
   end.
END.

output close.
output stream sout close.

disp i.

