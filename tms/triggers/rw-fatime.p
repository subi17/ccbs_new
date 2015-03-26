TRIGGER PROCEDURE FOR REPLICATION-WRITE OF FATime OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(FATime) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(FATime)
      Common.RepLog.TableName = "FATime"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(FATime.FATNum)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(FATime) THEN DO: */
ELSE DO:
   BUFFER-COMPARE FATime USING
      FATNum
      FTGrp
      Period
      Amt 
      CLI
      MsSeq
      CustNum
      Used
      InvNum
      TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(FATime)
         Common.RepLog.TableName = "FATime"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(Oldbuf.FATNum)
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
