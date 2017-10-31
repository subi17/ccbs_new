/* -----------------------------------------------
   MODULE .......: HSENDREPORT.P
   FUNCTION .....: RUN module at DAILYNOON.P
   APPLICATION ..: JL
   AUTHOR .......: JL
   CREATED ......: 12.01.06
   MODIFIED .....: 
   VERSION.......: M2.0
------------------------------------------------------ */
{Syst/commpaa.i}
{Syst/eventlog.i}

gcbrand = "1".
katun = "cron".
             
RUN Mm/highusagerep.p(INPUT Func.Common:mMake2DT(INPUT today - 90, INPUT 0),"",0).

