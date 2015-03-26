TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DayCampaign OLD BUFFER Oldbuf.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RecordId  = RECID(DayCampaign)
   Mobile.RepLog.TableName = "DayCampaign"
   Mobile.RepLog.EventType = (IF NEW(DayCampaign) THEN "CREATE" ELSE "MODIFY")
   Mobile.RepLog.KeyValue  = (IF NEW(DayCampaign) THEN DayCampaign.DCEvent
                              ELSE Oldbuf.DCEvent)
   Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
