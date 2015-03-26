TRIGGER PROCEDURE FOR REPLICATION-DELETE OF FixedFee.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(FixedFee)
   Common.RepLog.TableName = "FixedFee"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(FixedFee.FFNum)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
