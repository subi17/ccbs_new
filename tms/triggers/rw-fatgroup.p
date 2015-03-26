TRIGGER PROCEDURE FOR REPLICATION-WRITE OF FATGroup OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(FATGroup) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(FATGroup)
      Common.RepLog.TableName = "FATGroup"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = FATGroup.FtGrp
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(FATGroup) THEN DO: */
ELSE DO:
   BUFFER-COMPARE FATGroup USING
      FTGrp
      FtgName
      BillCode
      Amt
      TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(FATGroup)
         Common.RepLog.TableName = "FATGroup"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = Oldbuf.FtGrp
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
