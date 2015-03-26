
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 24.06.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
output to /apps/snet/200806/ycm-653.txt. 
FOR EACH mobsub NO-LOCK:
   put unformatted mobsub.cli "|" mobsub.imsi "|" (if mobsub.paytype then "Prepaid" ELSE "PostPaid")  skip.
END.
output close.
