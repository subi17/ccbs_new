TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLimit OLD BUFFER Oldbuf.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(MServiceLimit)
   Common.RepLog.TableName = "MServiceLimit"
   Common.RepLog.EventType = (IF NEW(MServiceLimit) THEN "CREATE" ELSE "MODIFY")
   Common.RepLog.KeyValue  = STRING((IF NEW(MServiceLimit) 
                                     THEN MServiceLimit.MSID
                                     ELSE Oldbuf.MSID))
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
