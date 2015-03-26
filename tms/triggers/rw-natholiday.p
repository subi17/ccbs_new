TRIGGER PROCEDURE FOR REPLICATION-WRITE OF NatHoliday OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(NatHoliday) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(NatHoliday)
      Common.RepLog.TableName = "NatHoliday"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(NatHoliday.Holiday)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(NatHoliday) THEN DO: */
ELSE DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(NatHoliday)
      Common.RepLog.TableName = "NatHoliday"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.KeyValue  = STRING(Oldbuf.Holiday)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
