TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DMS.

CREATE OrderCanal.RepLog.
ASSIGN
   OrderCanal.RepLog.RecordId  = RECID(DMS)
   OrderCanal.RepLog.TableName = "DMS"
   OrderCanal.RepLog.EventType = "DELETE"
   OrderCanal.RepLog.KeyValue  = STRING(DMS.DMSID)
   OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
