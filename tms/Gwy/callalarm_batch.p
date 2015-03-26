{commpaa.i}
gcBrand = "1".
{heartbeat.i}

FUNCTION fLogOut RETURNS LOGICAL:

   DEFINE VARIABLE llReturn  AS LOGICAL NO-UNDO.

   llReturn = TRUE.

   IF TIME > 43200 THEN DO:
      IF 86400 - TIME <= 60 THEN llReturn = FALSE.
   END.
   ELSE DO:
      IF 43200 - TIME <= 60 THEN llReturn = FALSE.
   END.

   RETURN llReturn.

END FUNCTION.

DEFINE VARIABLE lcNagios AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLock   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhSMSC   AS HANDLE    NO-UNDO.
DEFINE VARIABLE liPause  AS INT       NO-UNDO INIT 5.
DEF VAR liNode AS INT NO-UNDO INIT 1. 

IF NUM-ENTRIES(SESSION:PARAMETER) EQ 1 THEN DO:
   liNode = INT(ENTRY(1,SESSION:PARAMETER)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN QUIT.
   IF liNode < 1 OR liNode > 4 THEN QUIT.
END.

/* SMSC nodes 1-2 and 3-4 use different protocols */
IF liNode EQ 1 OR liNode EQ 2 THEN
   RUN smscgwy_smpp PERSISTENT SET lhSMSC.
ELSE IF liNode EQ 3 OR liNode EQ 4 THEN
   RUN smscgwy_smpp PERSISTENT SET lhSMSC.
ELSE QUIT.

RUN pInitialize IN lhSMSC (liNode).

ASSIGN
   lcNagios = "callalarm:CallAlarm SMS " + STRING(liNode)
   lcLock   = "/apps/tms/lock/callalarm_batch_" + STRING(liNode) + ".lock".
                                         
UNIX SILENT VALUE("touch " + lcLock).

fKeepAlive(lcNagios).

LOOP:
DO WHILE TRUE:
   
   RUN pSMSCGWY IN lhSMSC ("CallAlarm","","").

   /* if nothing to do then slow down a bit */
   IF RETURN-VALUE = "Empty queue" THEN 
      liPause = MIN(liPause + 7,90).
   ELSE liPause = 7. 

   PAUSE liPause NO-MESSAGE.

   fKeepAlive(lcNagios).

   IF SEARCH(lcLock) = ? THEN LEAVE LOOP.

END.

RUN pFinalize IN lhSMSC.

DELETE OBJECT lhSMSC.

QUIT.
