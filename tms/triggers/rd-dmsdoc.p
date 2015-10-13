TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DMSDoc.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(DMSDoc)
   Common.RepLog.TableName = "DMSDoc"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(DMSDoc.DMSID)          + CHR(255) +
                             DMSDoc.DocTypeID              + CHR(255) +
                             STRING(DMSDoc.DocStatusTS)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
