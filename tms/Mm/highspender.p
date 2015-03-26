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

DEFINE VARIABLE lcInvGroup AS CHARACTER NO-UNDO.

/* cron gives this as '-param' for mpre */
lcInvGroup = SESSION:PARAMETER. 



fELog("HIGHSPENDER", lcInvGroup + "Started").
run highspendnew (lcInvGroup).
fELog("HIGHSPENDER", lcInvGroup + "Stopped").

quit.
