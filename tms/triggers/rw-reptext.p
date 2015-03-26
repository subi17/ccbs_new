TRIGGER PROCEDURE FOR REPLICATION-WRITE OF RepText OLD BUFFER Oldbuf.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(RepText)
   Common.RepLog.TableName = "RepText"
   Common.RepLog.EventType = (IF NEW(RepText) THEN "CREATE" ELSE "MODIFY")
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).

IF NEW(RepText) THEN
   Common.RepLog.KeyValue  = STRING(RepText.TextType) + CHR(255) +
                             RepText.LinkCode         + CHR(255) +
                             STRING(RepText.Language) + CHR(255) +
                             STRING(RepText.ToDate).
ELSE
   Common.RepLog.KeyValue  = STRING(Oldbuf.TextType) + CHR(255) +
                             Oldbuf.LinkCode         + CHR(255) +
                             STRING(Oldbuf.Language) + CHR(255) +
                             STRING(Oldbuf.ToDate).
