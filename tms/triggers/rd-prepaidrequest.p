TRIGGER PROCEDURE FOR REPLICATION-DELETE OF PrepaidRequest.

CREATE OrderCanal.RepLog.
ASSIGN
   OrderCanal.RepLog.RecordId  = RECID(PrepaidRequest)
   OrderCanal.RepLog.TableName = "PrepaidRequest"
   OrderCanal.RepLog.EventType = "DELETE"
   OrderCanal.RepLog.KeyValue  = STRING(PrepaidRequest.PPRequest)
   OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
