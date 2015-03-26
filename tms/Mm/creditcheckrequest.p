/* ----------------------------------------------------------------------
  MODULE .......: creditcheckrequest.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......: 
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{msreqfunc.i}

DEFINE INPUT PARAMETER iiReqId AS INTEGER   NO-UNDO.

DEF VAR ocResult      AS CHAR NO-UNDO.

FIND MsRequest WHERE 
     MsRequest.MsRequest = iiReqId AND
     MsRequest.Brand     = gcBrand
NO-LOCK NO-ERROR.

IF NOT AVAIL MsRequest THEN RETURN "ERROR, request lost!".

/* Mark request as handled */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR: St. update failed.".  
     

