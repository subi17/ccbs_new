TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DPTarget OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(DPTarget) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DPTarget)
      Common.RepLog.TableName = "DPTarget"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(DPTarget.DPId) + CHR(255) +
                                DPTarget.TargetTable  + CHR(255) +
                                DPTarget.TargetKey    + CHR(255) +
                                STRING(DPTarget.ValidTo)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(DPTarget) THEN DO: */
ELSE DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DPTarget)
      Common.RepLog.TableName = "DPTarget"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.KeyValue  = STRING(Oldbuf.DPId) + CHR(255) +
                                Oldbuf.TargetTable  + CHR(255) +
                                Oldbuf.TargetKey    + CHR(255) +
                                STRING(Oldbuf.ValidTo)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
