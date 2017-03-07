/* -----------------------------------------------
  MODULE .......: DUMP.P
  FUNCTION .....: RUN modules AT 00.01 every morning
  APPLICATION ..: Dump files for shark
  AUTHOR .......: kl
  CREATED ......: xx.xx.xx
  MODIFIED .....: 
  VERSION ......: Yoigo
------------------------------------------------------ */

{Syst/commpaa.i}
gcbrand = "1".
katun = "cron".

{Syst/eventlog.i}

def var period  as int    no-undo.
DEF VAR liQty   AS INT    NO-UNDO.

fELog("DAILYDUMP","DatadumpStarted").
RUN Mm/dumpall.p.
fELog("DAILYDUMP","DatadumpStopped").

quit.
