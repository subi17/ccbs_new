TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DCCLI OLD BUFFER Oldbuf.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RecordId  = RECID(DCCLI)
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = (IF NEW(DCCLI) THEN "CREATE" ELSE "MODIFY")
   Mobile.RepLog.KeyValue  = STRING((IF NEW(DCCLI) THEN DCCLI.PerContractID
                                     ELSE Oldbuf.PerContractID))
   Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
