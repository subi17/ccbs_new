TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DayCampaign.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RecordId  = RECID(DayCampaign)
   Mobile.RepLog.TableName = "DayCampaign"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.KeyValue  = DayCampaign.DCEvent
   Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
