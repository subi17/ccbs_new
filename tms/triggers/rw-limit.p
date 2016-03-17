TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Limit OLD BUFFER Oldbuf.

{Syst/tmsconst.i}

IF Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT} THEN RETURN.

IF NEW(Limit) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(Limit)
      Common.RepLog.TableName = "Limit"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(Limit.CustNum)    + CHR(255) +
                                STRING(Limit.MsSeq)      + CHR(255) +
                                STRING(Limit.LimitType)  + CHR(255) +
                                STRING(Limit.FromDate)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END.
ELSE DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(Limit)
      Common.RepLog.TableName = "Limit"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.KeyValue  = STRING(OldBuf.CustNum)   + CHR(255) +
                                STRING(Oldbuf.MsSeq)     + CHR(255) +
                                STRING(Oldbuf.LimitType) + CHR(255) +
                                STRING(Oldbuf.FromDate)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END.
