TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DMSDoc OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(DMSDoc) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DMSDoc)
      Common.RepLog.TableName = "DMSDoc"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(DMSDoc.DMSID)          + CHR(255) +
                                DMSDoc.DocTypeID              + CHR(255) +
                                STRING(DMSDoc.DocStatusTS)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(DMSDoc) THEN DO: */
ELSE DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DMSDoc)
      Common.RepLog.TableName = "DMSDoc"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.KeyValue  = STRING(Oldbuf.DMSID)          + CHR(255) +
                                Oldbuf.DocTypeID              + CHR(255) +
                                STRING(Oldbuf.DocStatusTS)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
