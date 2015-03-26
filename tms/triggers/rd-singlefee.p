TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SingleFee.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(SingleFee)
   Common.RepLog.TableName = "SingleFee"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(SingleFee.FMItemId)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
