TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MobSub.

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.RecordId  = RECID(MobSub)
   Ordercanal.RepLog.TableName = "MobSub"
   Ordercanal.RepLog.EventType = "DELETE"
   Ordercanal.RepLog.KeyValue  = STRING(MobSub.MsSeq)
   Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
