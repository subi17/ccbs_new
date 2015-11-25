TRIGGER PROCEDURE FOR REPLICATION-WRITE OF TopupSchemeRow OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(TopupSchemeRow) THEN DO:
   CREATE OrderCanal.RepLog.
   ASSIGN
      OrderCanal.RepLog.RecordId  = RECID(TopupSchemeRow)
      OrderCanal.RepLog.TableName = "TopupSchemeRow"
      OrderCanal.RepLog.EventType = "CREATE"
      OrderCanal.RepLog.KeyValue  = STRING(TopupSchemeRow.TopupSchemeRowID)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(TopupSchemeRow) THEN DO: */
ELSE DO:
   CREATE OrderCanal.RepLog.
   ASSIGN
      OrderCanal.RepLog.RecordId  = RECID(TopupSchemeRow)
      OrderCanal.RepLog.TableName = "TopupSchemeRow"
      OrderCanal.RepLog.EventType = "MODIFY"
      OrderCanal.RepLog.KeyValue  = STRING(Oldbuf.TopupSchemeRowID)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */

