TRIGGER PROCEDURE FOR REPLICATION-WRITE OF RequestType OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(RequestType) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(RequestType)
      Common.RepLog.TableName = "RequestType"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(RequestType.ReqType)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(RequestType) THEN DO: */
ELSE DO:
   BUFFER-COMPARE RequestType USING
      ReqType
      ReqName
   TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(RequestType)
         Common.RepLog.TableName = "RequestType"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(Oldbuf.ReqType)
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
