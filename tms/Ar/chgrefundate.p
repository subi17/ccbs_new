/* ----------------------------------------------------------------------
  MODULE .......: chgrefundate.p
  TASK .........: change refund date to request
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 27.11.07
  CHANGED ......:
  Version ......: yoigo
----------------------------------------------------------------------- */
{Func/msreqfunc.i}
{Func/fuserright.i}

DEFINE INPUT PARAMETER iiMsRequest  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiFromStatus AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iiToStatus   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER icCparam     AS CHARACTER NO-UNDO.

DEF VAR llOk        AS LOG  NO-UNDO.
DEF VAR ldtPaymDate AS DATE NO-UNDO.

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

ldtPaymDate = MsRequest.ReqDtParam1.

ehto = 9.
RUN Syst/ufkey.p.

REPEAT ON ENDKEY UNDO, LEAVE:

   PAUSE 0.
   UPDATE 
      ldtPaymDate 
          FORMAT "99-99-99"
          HELP "Refund payment date"
          LABEL "Refund Date"
          VALIDATE(INPUT ldtPaymDate > TODAY,
                   "Earliest payment date is tomorrow")
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " REFUND DATE "
           FRAME fPaymDate.
   LEAVE.
END.

llOk = FALSE.
        
IF ldtPaymDate NE ? AND ldtPaymDate NE MsRequest.ReqDtParam1 THEN DO:

   MESSAGE "Change refund payment date and accept request for file"
           "creation, i.e. update status to " STRING(iiToStatus) + "?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   SET llOk.

END.

HIDE FRAME fPaymDate NO-PAUSE.

IF NOT llOk THEN RETURN.

FIND CURRENT MsRequest EXCLUSIVE-LOCK.
/* refund payment date */
MsRequest.ReqDtParam1 = ldtPaymDate.

fReqStatus(iiToStatus,"").


