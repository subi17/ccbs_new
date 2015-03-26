TRIGGER PROCEDURE FOR REPLICATION-DELETE OF FATGroup.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(FATGroup)
   Common.RepLog.TableName = "FATGroup"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = FATGroup.FtGrp 
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
