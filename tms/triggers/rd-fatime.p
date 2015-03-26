TRIGGER PROCEDURE FOR REPLICATION-DELETE OF FATime.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(FATime)
   Common.RepLog.TableName = "FATime"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(FATime.FATNum)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
