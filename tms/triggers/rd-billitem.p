TRIGGER PROCEDURE FOR REPLICATION-DELETE OF BillItem.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(BillItem)
   Common.RepLog.TableName = "BillItem"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = BillItem.BillCode
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
