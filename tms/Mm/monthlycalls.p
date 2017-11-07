/* -----------------------------------------------
  MODULE .......: MONTHLYCALLS.P
  FUNCTION .....: Dump last months calls at the months 1st day 1:00 
  APPLICATION ..: 
  AUTHOR .......: jl
  CREATED ......: jl/25.11.2005 
  MODIFIED .....: jl/29.11.2005 Monthly calls moved here from mdaily
                  
------------------------------------------------------ */

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "cron".

{Syst/eventlog.i}

DEFINE VARIABLE period  AS INTEGER NO-UNDO.

period = YEAR(TODAY - 5) * 100 + MONTH(TODAY - 5).
   
   fELog("MONTHLY","MonthlyCallDumpStarted").
   RUN monthlycalldump(period).
   fELog("MONTHLY","MonthlyCallDumpStopped").

quit.
