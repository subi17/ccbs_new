TRIGGER PROCEDURE FOR REPLICATION-WRITE OF BItemGroup OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(BItemGroup) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(BItemGroup)
      Common.RepLog.TableName = "BItemGroup"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = BItemGroup.BIGroup
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(BItemGroup) THEN DO: */
ELSE DO:
   BUFFER-COMPARE BItemGroup USING BIGroup BIGName TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(BItemGroup)
         Common.RepLog.TableName = "BItemGroup"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = Oldbuf.BIGroup
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
