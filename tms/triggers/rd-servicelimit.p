TRIGGER PROCEDURE FOR REPLICATION-DELETE OF ServiceLimit.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(ServiceLimit)
   Common.RepLog.TableName = "ServiceLimit"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(ServiceLimit.SlSeq)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
