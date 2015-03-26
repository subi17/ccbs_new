TRIGGER PROCEDURE FOR REPLICATION-DELETE OF NatHoliday.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(NatHoliday)
   Common.RepLog.TableName = "NatHoliday"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(NatHoliday.Holiday)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
