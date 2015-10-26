TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DMS OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(DMS) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DMS)
      Common.RepLog.TableName = "DMS"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(DMS.DMSID)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(DMS) THEN DO: */
ELSE DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DMS)
      Common.RepLog.TableName = "DMS"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.KeyValue  = STRING(Oldbuf.DMSID)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
