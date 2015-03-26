TRIGGER PROCEDURE FOR REPLICATION-DELETE OF FMItem.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(FMItem)
   Common.RepLog.TableName = "FMItem"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = FMItem.FeeModel
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
