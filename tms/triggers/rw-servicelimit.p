TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ServiceLimit OLD BUFFER Oldbuf.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(ServiceLimit)
   Common.RepLog.TableName = "ServiceLimit"
   Common.RepLog.EventType = (IF NEW(ServiceLimit) THEN "CREATE" ELSE "MODIFY")
   Common.RepLog.KeyValue  = STRING((IF NEW(ServiceLimit)
                                     THEN ServiceLimit.SlSeq
                                     ELSE Oldbuf.SlSeq))
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
