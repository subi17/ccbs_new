{Syst/commpaa.i}
{Func/timestamp.i}
gcbrand = "1".
katun = "cron".

{Func/lib/eventlog.i}

fELog("DAILY","ServiceDumpStarted").
RUN Mm/servicedump.
fELog("DAILY","ServiceDumpStopped").

quit.
