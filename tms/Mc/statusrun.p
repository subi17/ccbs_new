{Syst/commpaa.i}
{Func/timestamp.i}
gcbrand = "1".
katun = "cron".

{Syst/eventlog.i}

fELog("STAT","CdrCountStarted").
RUN cdrcntdump.
fELog("STAT","CdrCountStopped").

fELog("STAT","WebSessionCountStarted").
RUN Mc/websessioncount.p.
fELog("STAT","WebSessionCountStopped").

fELog("STAT","AreaStatusStarted").
RUN Syst/areastatus.p.
fELog("STAT","AreaStatusStopped").

fELog("STAT","CdrCountStarted").
RUN Mm/msstatcount.p.
fELog("STAT","CdrCountStopped").

QUIT.
