TRIGGER PROCEDURE FOR REPLICATION-DELETE OF Barring.

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.RecordId  = RECID(Barring)
   Ordercanal.RepLog.TableName = "BarringConf"
   Ordercanal.RepLog.EventType = "DELETE"
   Ordercanal.RepLog.KeyValue  = STRING(Barring.MsSeq) + CHR(255) + 
                                 Barring.BarringCode + CHR(255) +
                                 Barring.BarringStatus + CHR(255) +
                                 STRING(Barring.EventTS) 
   Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).



