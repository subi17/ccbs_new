/* -----------------------------------------------
  MODULE .......: HIGHPSENDER
  FUNCTION .....: RUN modules AT 01.00 every night for each invgroup
  APPLICATION ..: jl
  AUTHOR .......: jl
  CREATED ......: 31.01.06
  MODIFIED .....: 
  Version ......: M15
------------------------------------------------------ */

{commpaa.i}
gcbrand = "1".
katun = "cron".

{eventlog.i}

fELog("HIGHSPENDER","AdvPaymStarted").
run highspendnew ("ADVPAYM").
fELog("HIGHSPENDER","AdvPaymStopped").

quit.
