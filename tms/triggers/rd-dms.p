TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DMS.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(DMS)
   Common.RepLog.TableName = "DMS"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(DMS.DMSID)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
