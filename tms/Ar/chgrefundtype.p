/* ----------------------------------------------------------------------
  MODULE .......: chgrefundtype.p
  TASK .........: change refund type to request
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 16.11.07
  CHANGED ......:
  Version ......: yoigo
----------------------------------------------------------------------- */
{Func/msreqfunc.i}
{Func/fuserright.i}

DEFINE INPUT PARAMETER iiMsRequest  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiFromStatus AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiToStatus   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER icCparam     AS CHARACTER NO-UNDO.

DEF VAR llOk AS LOG  NO-UNDO.

/* eventlog not needed here, msrequest.p takes care of it */

FIND MsRequest WHERE MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 23 THEN DO:
   MESSAGE "Unknown request"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF MsRequest.ReqDParam2 NE 2 THEN DO:
   MESSAGE "Refund type is not 'Payment to bank'"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* has user got priviliges */
IF fTokenRights(katun,"CCSUPER") NE "RW" THEN DO:
   MESSAGE "You are not authorized to use this function"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

llOk = FALSE.

MESSAGE "Change type of refund from 'payment to bank' to" 
        "'transfer to advance payment' and approve this request,"
        "i.e. change status to " STRING(iiToStatus) + "?"
VIEW-AS ALERT-BOX QUESTION
BUTTONS YES-NO
SET llOk.

IF NOT llOk THEN RETURN.


FIND CURRENT MsRequest EXCLUSIVE-LOCK.
/* refund type */
MsRequest.ReqDParam2 = 1.

fReqStatus(iiToStatus,"").


