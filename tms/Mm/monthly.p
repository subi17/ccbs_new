/* -----------------------------------------------
  MODULE .......: MONTHLY.P
  FUNCTION .....: Open subscription (status 7 -> 4) every month 1st day at 1:00
  APPLICATION ..: TMS
  AUTHOR .......: JP
  CREATED ......: 
  MODIFIED .....: jl/29.11.2005 Monthly calls/fees moved to monthlyfees.p and
                  monthlycalls.p
                  28.03.07/aam  tax reports
                  
------------------------------------------------------ */

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "cron".

{Syst/eventlog.i}

/* delete old counters */
RUN Syst/tmcdelete.p (TODAY).

quit.
