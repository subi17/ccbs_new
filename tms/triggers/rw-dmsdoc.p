TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DMSDoc OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(DMSDoc) THEN DO:
   CREATE OrderCanal.RepLog.
   ASSIGN
      OrderCanal.RepLog.RecordId  = RECID(DMSDoc)
      OrderCanal.RepLog.TableName = "DMSDoc"
      OrderCanal.RepLog.EventType = "CREATE"
      OrderCanal.RepLog.KeyValue  = STRING(DMSDoc.DMSID)          + CHR(255) +
                                DMSDoc.DocTypeID              + CHR(255) +
                                STRING(DMSDoc.DocStatusTS)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(DMSDoc) THEN DO: */
ELSE DO:
   CREATE OrderCanal.RepLog.
   ASSIGN
      OrderCanal.RepLog.RecordId  = RECID(DMSDoc)
      OrderCanal.RepLog.TableName = "DMSDoc"
      OrderCanal.RepLog.EventType = "MODIFY"
      OrderCanal.RepLog.KeyValue  = STRING(Oldbuf.DMSID)          + CHR(255) +
                                Oldbuf.DocTypeID              + CHR(255) +
                                STRING(Oldbuf.DocStatusTS)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
