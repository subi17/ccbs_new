/* -----------------------------------------------
  MODULE .......: HIGHPSENDER
  FUNCTION .....: RUN modules AT 01.00 every night for each invgroup
  APPLICATION ..: jl
  AUTHOR .......: jl
  CREATED ......: 31.01.06
  MODIFIED .....: 
  Version ......: M15
------------------------------------------------------ */

{Syst/commpaa.i}
Syst.CUICommon:gcBrand = "1".
katun = "cron".

{Syst/eventlog.i}

fELog("HIGHSPENDER","AdvPaymStarted").
RUN Mm/highspendnew.p ("ADVPAYM").
fELog("HIGHSPENDER","AdvPaymStopped").

quit.
