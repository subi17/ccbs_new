/* -----------------------------------------------
  MODULE .......: Addline discount cron job
  FUNCTION .....: 
  APPLICATION ..: 
  AUTHOR .......: Chanchal Sharma
  CREATED ......: 28.07.17
  MODIFIED .....: 
  Version ......:
------------------------------------------------------ */

{Syst/commpaa.i}
gcbrand = "1".
katun = "cron".

{Syst/eventlog.i}

fELog("ADDLINEDISC", "Cron to give additional line discount started").
RUN Mm/addlinedisccron.p.
fELog("ADDLINEDISC", "Cron to give additional line discount Stopped").

QUIT.



