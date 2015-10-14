{timestamp.i}

DEF VAR liMsRequest AS INTEGER NO-UNDO.
DEF VAR ldaDate AS DATE NO-UNDO INIT TODAY.
DEF VAR ldeActstamp AS DEC NO-UNDO.

UPDATE liMsRequest LABEL "Request ID" FORMAT ">>>>>>>>9"
       ldaDate LABEL "Act. Date" format "99-99-9999".

FIND MsRequest WHERE MsRequest.MsRequest = liMsRequest EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL MsRequest THEN MESSAGE "Request " liMsRequest " not found".
ELSE DO:

   IF ldaDate NE TODAY THEN
     ldeActstamp = fHMS2TS(ldaDate,"00:00:00").
   ELSE ldeActstamp = fmakets().

   ASSIGN MsRequest.ActStamp = ldeActstamp.

   IF MsRequest.ReqType = 10 or
      MsRequest.ReqType = 0 THEN DO:
         MsRequest.ReqDParam1 = ldeActstamp.
   END.
   FIND CURRENT MsRequest NO-LOCK.
   MESSAGE "Request " liMsRequest "activation time changed" VIEW-AS ALERT-BOX.
   RELEASE MsRequest.
END.
