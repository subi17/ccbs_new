/* -----------------------------------------------
  MODULE .......: DAILYDUMP3.P
  FUNCTION .....: RUN modules AT 00.01 every morning
  APPLICATION ..: Dump files for shark
  AUTHOR .......: kl
  CREATED ......: xx.xx.xx
  MODIFIED .....: 04.12.05 jl  invoice moved from dailydump2 to this file
                  25.03.06 mvi Changed InvoiceDump to take input param
                               ilOnlyNew
  Version ......: M15
------------------------------------------------------ */

{Syst/commpaa.i}
gcbrand = "1".
katun = "cron".

{Func/lib/eventlog.i}

def var period  as int    no-undo.
DEF VAR liQty   AS INT    NO-UNDO.

fELog("DAILYDUMP","InvoiceDumpStarted").

IF WEEKDAY(TODAY) = 1 THEN DO:
   /* All invoices on Sunday */
   RUN invoicedump(FALSE).
END.
ELSE IF WEEKDAY(TODAY) NE 2 THEN DO:
   /* only new invoices on Tuesday-Saturday */
   RUN invoicedump(TRUE).
END.

fELog("DAILYDUMP","InvoiceDumpStopped").

quit.
