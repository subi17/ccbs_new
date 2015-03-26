TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DCCLI.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RecordId  = RECID(DCCLI)
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.KeyValue  = STRING(DCCLI.PerContractID)
   Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
