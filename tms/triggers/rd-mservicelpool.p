TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MServiceLPool.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(MServiceLPool)
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).

IF MServiceLPool.CustNum > 0 THEN
   ASSIGN Common.RepLog.TableName = "CustomerMServiceLPool"
          Common.RepLog.KeyValue  = STRING(MServiceLPool.CustNum) + CHR(255) +
                                    STRING(MServiceLPool.SLSeq)   + CHR(255) +
                                    STRING(MServiceLPool.FromTS).
ELSE
   ASSIGN Common.RepLog.TableName = "MServiceLPool"
          Common.RepLog.KeyValue  = STRING(MServiceLPool.MsSeq) + CHR(255) +
                                    STRING(MServiceLPool.SLSeq) + CHR(255) +
                                    STRING(MServiceLPool.FromTS).

