TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DMSDoc.

CREATE OrderCanal.RepLog.
ASSIGN
   OrderCanal.RepLog.RecordId  = RECID(DMSDoc)
   OrderCanal.RepLog.TableName = "DMSDoc"
   OrderCanal.RepLog.EventType = "DELETE"
   OrderCanal.RepLog.KeyValue  = STRING(DMSDoc.DMSID)          + CHR(255) +
                             DMSDoc.DocTypeID              + CHR(255) +
                             STRING(DMSDoc.DocStatusTS)
   OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
