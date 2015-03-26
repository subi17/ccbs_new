TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MServiceLimit.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(MServiceLimit)
   Common.RepLog.TableName = "MServiceLimit"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(MServiceLimit.MSID)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
