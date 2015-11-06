TRIGGER PROCEDURE FOR REPLICATION-DELETE OF TermReturn.

CREATE ordercanal.RepLog.
ASSIGN
   ordercanal.RepLog.RecordId  = RECID(TermReturn)
   ordercanal.RepLog.TableName = "TermReturn"
   ordercanal.RepLog.EventType = "DELETE"
   ordercanal.RepLog.KeyValue  = TermReturn.IMEI + CHR(255) +
                                 STRING(TermReturn.ReturnTS)
   ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
