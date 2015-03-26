TRIGGER PROCEDURE FOR REPLICATION-DELETE OF Invoice.

/* Only require for Service Invoice */

IF Invoice.InvType = 1 THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(Invoice)
      Common.RepLog.TableName = "Invoice"
      Common.RepLog.EventType = "DELETE"
      Common.RepLog.KeyValue  = STRING(Invoice.InvNum)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF Invoice.InvType = 1 THEN DO: */
