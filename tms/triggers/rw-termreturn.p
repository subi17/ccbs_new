TRIGGER PROCEDURE FOR REPLICATION-WRITE OF TermReturn OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(TermReturn) THEN DO:
   CREATE ordercanal.RepLog.
   ASSIGN
      ordercanal.RepLog.RecordId  = RECID(TermReturn)
      ordercanal.RepLog.TableName = "TermReturn"
      ordercanal.RepLog.EventType = "CREATE"
      ordercanal.RepLog.KeyValue  = TermReturn.IMEI + CHR(255) +
                                    STRING(TermReturn.ReturnTS)            
      ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(TermReturn) THEN DO: */
ELSE DO:
   CREATE ordercanal.RepLog.
   ASSIGN
      ordercanal.RepLog.RecordId  = RECID(TermReturn)
      ordercanal.RepLog.TableName = "TermReturn"
      ordercanal.RepLog.EventType = "MODIFY"
      ordercanal.RepLog.KeyValue  = Oldbuf.IMEI    + CHR(255) +
                                    STRING(Oldbuf.ReturnTS)
      ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
