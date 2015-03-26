TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DiscountPlan.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(DiscountPlan)
   Common.RepLog.TableName = "DiscountPlan"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(DiscountPlan.DPId)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
