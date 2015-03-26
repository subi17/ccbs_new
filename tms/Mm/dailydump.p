/* -----------------------------------------------
  MODULE .......: DUMP.P
  FUNCTION .....: RUN modules AT 00.01 every morning
  APPLICATION ..: Dump files for shark
  AUTHOR .......: kl
  CREATED ......: xx.xx.xx
  MODIFIED .....: 
  VERSION ......: Yoigo
------------------------------------------------------ */

{commpaa.i}
gcbrand = "1".
katun = "cron".

{eventlog.i}

def var period  as int    no-undo.
DEF VAR liQty   AS INT    NO-UNDO.

fELog("DAILYDUMP","DatadumpStarted").
run dumpall.
fELog("DAILYDUMP","DatadumpStopped").

quit.
