TRIGGER PROCEDURE FOR REPLICATION-WRITE OF FMItem OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(FMItem) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(FMItem)
      Common.RepLog.TableName = "FMItem"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = FMItem.FeeModel
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(FMItem) THEN DO: */
ELSE DO:
   BUFFER-COMPARE FMItem USING FeeModel BillCode Amount FirstMonthBR TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(FMItem)
         Common.RepLog.TableName = "FMItem"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = Oldbuf.FeeModel
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
