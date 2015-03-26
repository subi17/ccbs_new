TRIGGER PROCEDURE FOR REPLICATION-WRITE OF BillItem OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(BillItem) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(BillItem)
      Common.RepLog.TableName = "BillItem"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = BillItem.BillCode
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(BillItem) THEN DO: */
ELSE DO:
   BUFFER-COMPARE BillItem USING BillCode BIGroup BIName TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(BillItem)
         Common.RepLog.TableName = "BillItem"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = Oldbuf.BillCode
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
