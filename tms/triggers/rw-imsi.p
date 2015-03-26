TRIGGER PROCEDURE FOR REPLICATION-WRITE OF IMSI OLD BUFFER Oldbuf.

IF NEW(IMSI) THEN DO:
   CREATE Mobile.RepLog.
   ASSIGN
      Mobile.RepLog.RecordId  = RECID(IMSI)
      Mobile.RepLog.TableName = "IMSI"
      Mobile.RepLog.EventType = "CREATE"
      Mobile.RepLog.KeyValue  = IMSI.IMSI
      Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END.
