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

{commpaa.i}
gcbrand = "1".
katun = "cron".

{eventlog.i}

/* delete old counters */
RUN tmcdelete (TODAY).

quit.
