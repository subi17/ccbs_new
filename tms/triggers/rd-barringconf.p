TRIGGER PROCEDURE FOR REPLICATION-DELETE OF BarringConf.

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.RecordId  = RECID(BarringConf)
   Ordercanal.RepLog.TableName = "BarringConf"
   Ordercanal.RepLog.EventType = "DELETE"
   Ordercanal.RepLog.KeyValue  = STRING(BarringConf.BarringCode)
   Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).



