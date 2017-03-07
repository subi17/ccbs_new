{Func/timestamp.i}

DEF VAR liMsRequest AS INTEGER NO-UNDO.

UPDATE liMsRequest LABEL "Request ID" FORMAT ">>>>>>>9".

FIND MsRequest WHERE MsRequest.MsRequest = liMsRequest EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL MsRequest THEN MESSAGE "Request " liMsRequest " not found".
ELSE DO:
   ASSIGN MsRequest.ActStamp = fMakeTS().
   IF MsRequest.ReqType = 10 or
      MsRequest.ReqType = 0 THEN
         MsRequest.ReqDParam1 = fMakeTS().
   FIND CURRENT MsRequest NO-LOCK.
   MESSAGE "Request " liMsRequest "activation time is set to current time" 
   VIEW-AS ALERT-BOX.
   RELEASE MsRequest.
END.

