TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLPool OLD BUFFER Oldbuf.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(MServiceLPool)
   Common.RepLog.EventType = (IF NEW(MServiceLPool) THEN "CREATE" ELSE "MODIFY")
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).

IF NEW(MServiceLPool) THEN DO:
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
END.
ELSE DO:
   IF Oldbuf.CustNum > 0 THEN
      ASSIGN Common.RepLog.TableName = "CustomerMServiceLPool"
             Common.RepLog.KeyValue  = STRING(Oldbuf.CustNum) + CHR(255) +
                                       STRING(Oldbuf.SLSeq)   + CHR(255) +
                                       STRING(Oldbuf.FromTS).
   ELSE
      ASSIGN Common.RepLog.TableName = "MServiceLPool"
             Common.RepLog.KeyValue  = STRING(Oldbuf.MsSeq) + CHR(255) +
                                       STRING(Oldbuf.SLSeq) + CHR(255) +
                                       STRING(Oldbuf.FromTS).
END.
