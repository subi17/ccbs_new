TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DPMember OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(DPMember) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DPMember)
      Common.RepLog.TableName = "DPMember"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(DPMember.DPId) + CHR(255) +
                                DPMember.HostTable    + CHR(255) +
                                DPMember.KeyValue     + CHR(255) +
                                STRING(DPMember.ValidTo)            
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(DPMember) THEN DO: */
ELSE DO:

   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DPMember)
      Common.RepLog.TableName = "DPMember"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.KeyValue  = STRING(Oldbuf.DPId) + CHR(255) +
                                Oldbuf.HostTable    + CHR(255) +
                                Oldbuf.KeyValue     + CHR(255) +
                                STRING(Oldbuf.ValidTo)
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* ELSE DO: */
