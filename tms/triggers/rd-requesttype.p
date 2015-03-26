TRIGGER PROCEDURE FOR REPLICATION-DELETE OF RequestType.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(RequestType)
   Common.RepLog.TableName = "RequestType"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(RequestType.ReqType)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
