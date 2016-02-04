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
gcbrand = "1".
katun = "cron".

{Func/lib/eventlog.i}

DEFINE VARIABLE lcInvGroup AS CHARACTER NO-UNDO.

/* cron gives this as '-param' for mpre */
lcInvGroup = SESSION:PARAMETER. 



fELog("HIGHSPENDER", lcInvGroup + "Started").
RUN Mm/highspendnew (lcInvGroup).
fELog("HIGHSPENDER", lcInvGroup + "Stopped").

quit.
