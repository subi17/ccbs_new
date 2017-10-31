{Syst/commpaa.i}
Syst.CUICommon:gcBrand = "1".
Syst.CUICommon:katun = "cron".

{Syst/eventlog.i}

fELog("DAILY","ServiceDumpStarted").
RUN Mm/servicedump.p.
fELog("DAILY","ServiceDumpStopped").

quit.
