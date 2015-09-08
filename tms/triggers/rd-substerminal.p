TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SubsTerminal.

CREATE OrderCanal.RepLog.
ASSIGN
   OrderCanal.RepLog.RecordId  = RECID(SubsTerminal)
   OrderCanal.RepLog.TableName = "SubsTerminal"
   OrderCanal.RepLog.EventType = "DELETE"
   OrderCanal.RepLog.KeyValue  = STRING(SubsTerminal.TerminalID)
   OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
