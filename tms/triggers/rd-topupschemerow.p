TRIGGER PROCEDURE FOR REPLICATION-DELETE OF TopupSchemeRow.

CREATE OrderCanal.RepLog.
ASSIGN
   OrderCanal.RepLog.RecordId  = RECID(TopupSchemeRow)
   OrderCanal.RepLog.TableName = "TopupSchemeRow"
   OrderCanal.RepLog.EventType = "DELETE"
   OrderCanal.RepLog.KeyValue  = STRING(TopupSchemeRow.TopupSchemeRowID)
   OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).

