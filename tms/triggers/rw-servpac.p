TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ServPac OLD BUFFER Oldbuf.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RecordId  = RECID(ServPac)
   Mobile.RepLog.TableName = "ServPac"
   Mobile.RepLog.EventType = (IF NEW(ServPac) THEN "CREATE" ELSE "MODIFY")
   Mobile.RepLog.KeyValue  = (IF NEW(ServPac) THEN ServPac.ServPac
                              ELSE Oldbuf.ServPac)
   Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
