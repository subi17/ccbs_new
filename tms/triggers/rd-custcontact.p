TRIGGER PROCEDURE FOR REPLICATION-DELETE OF CustContact.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(CustContact)
   Common.RepLog.TableName = "CustContact"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(CustContact.CustNum)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).

