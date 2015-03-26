TRIGGER PROCEDURE FOR REPLICATION-WRITE OF FixedFee OLD BUFFER Oldbuf.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(FixedFee)
   Common.RepLog.TableName = "FixedFee"
   Common.RepLog.EventType = (IF NEW(FixedFee) THEN "CREATE" ELSE "MODIFY")
   Common.RepLog.KeyValue  = STRING((IF NEW(FixedFee) THEN FixedFee.FFNum
                                     ELSE Oldbuf.FFNum))
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
