TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DPTarget.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(DPTarget)
   Common.RepLog.TableName = "DPTarget"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(DPTarget.DPId) + CHR(255) +
                             DPTarget.TargetTable  + CHR(255) +
                             DPTarget.TargetKey    + CHR(255) +
                             STRING(DPTarget.ValidTo) 
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
