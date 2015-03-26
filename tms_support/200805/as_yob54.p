
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 27.05.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
katun = "anttis".
{fcustdata.i}
def stream sout.
output stream sout to /apps/snet/200805/yob54.log.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcInvGroup AS CHARACTER NO-UNDO.

put stream sout unformatted "custnum|wrong invgroup|correct invgroup" skip.
FOR EACH Customer NO-LOCK
  /* i = 1 to 10000*/ .
   lcInvGroup = fDefInvGroup(Customer.region).
   IF Customer.InvGroup NE lcInvGroup THEN
      put stream sout unformatted 
         Customer.custnum "|"
         customer.invgroup "|"
         lcInvGroup 
 skip.
END.

output stream sout close.
