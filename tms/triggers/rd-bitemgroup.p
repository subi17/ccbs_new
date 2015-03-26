TRIGGER PROCEDURE FOR REPLICATION-DELETE OF BItemGroup.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(BItemGroup)
   Common.RepLog.TableName = "BItemGroup"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = BItemGroup.BIGroup
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
