TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SingleFee OLD BUFFER Oldbuf.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(SingleFee)
   Common.RepLog.TableName = "SingleFee"
   Common.RepLog.EventType = (IF NEW(SingleFee) THEN "CREATE" ELSE "MODIFY")
   Common.RepLog.KeyValue  = STRING((IF NEW(SingleFee) THEN SingleFee.FMItemId
                                     ELSE Oldbuf.FMItemId))
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
