{Syst/commpaa.i}
gcbrand = "1".
katun = "cron".

{Syst/eventlog.i}

fELog("DAILY","ServiceDumpStarted").
RUN Mm/servicedump.p.
fELog("DAILY","ServiceDumpStopped").

quit.
