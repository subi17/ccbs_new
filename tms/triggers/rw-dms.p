TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DMS OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(DMS) THEN DO:
   CREATE OrderCanal.RepLog.
   ASSIGN
      OrderCanal.RepLog.RecordId  = RECID(DMS)
      OrderCanal.RepLog.TableName = "DMS"
      OrderCanal.RepLog.EventType = "CREATE"
      OrderCanal.RepLog.KeyValue  = STRING(DMS.DMSID)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(DMS) THEN DO: */
ELSE DO:
   CREATE OrderCanal.RepLog.
   ASSIGN
      OrderCanal.RepLog.RecordId  = RECID(DMS)
      OrderCanal.RepLog.TableName = "DMS"
      OrderCanal.RepLog.EventType = "MODIFY"
      OrderCanal.RepLog.KeyValue  = STRING(Oldbuf.DMSID)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
