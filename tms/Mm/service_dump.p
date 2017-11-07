{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "cron".

{Syst/eventlog.i}

fELog("DAILY","ServiceDumpStarted").
RUN Mm/servicedump.p.
fELog("DAILY","ServiceDumpStopped").

quit.
