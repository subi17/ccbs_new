{commpaa.i}
{timestamp.i}
gcbrand = "1".
katun = "cron".

{eventlog.i}

fELog("DAILY","ServiceDumpStarted").
run servicedump.
fELog("DAILY","ServiceDumpStopped").

quit.
