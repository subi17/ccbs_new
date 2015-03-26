TRIGGER PROCEDURE FOR REPLICATION-DELETE OF ServPac.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RecordId  = RECID(ServPac)
   Mobile.RepLog.TableName = "ServPac"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.KeyValue  = ServPac.ServPac
   Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
