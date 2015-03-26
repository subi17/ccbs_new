/* -----------------------------------------------
   MODULE .......: HSENDREPORT.P
   FUNCTION .....: RUN module at DAILYNOON.P
   APPLICATION ..: JL
   AUTHOR .......: JL
   CREATED ......: 12.01.06
   MODIFIED .....: 
   VERSION.......: M2.0
------------------------------------------------------ */
{commpaa.i}
{eventlog.i}
{timestamp.i}

gcbrand = "1".
katun = "cron".
             
run highusagerep(INPUT fMake2Dt(INPUT today - 90, INPUT 0),"",0).

