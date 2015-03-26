
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 29.03.10
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

output to tmsuser.d.
FOR EACH tmsuser NO-LOCK:
   export tmsuser.
end.
