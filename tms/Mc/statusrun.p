{Syst/commpaa.i}
{Func/timestamp.i}
gcbrand = "1".
katun = "cron".

{Func/lib/eventlog.i}

fELog("STAT","CdrCountStarted").
RUN cdrcntdump.
fELog("STAT","CdrCountStopped").

fELog("STAT","WebSessionCountStarted").
RUN websessioncount.
fELog("STAT","WebSessionCountStopped").

fELog("STAT","AreaStatusStarted").
RUN areastatus.
fELog("STAT","AreaStatusStopped").

fELog("STAT","CdrCountStarted").
RUN msstatcount.
fELog("STAT","CdrCountStopped").

QUIT.
