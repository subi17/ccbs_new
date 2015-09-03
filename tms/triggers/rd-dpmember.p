TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DPMember.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RecordId  = RECID(DPMember)
   Common.RepLog.TableName = "DPMember"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.KeyValue  = STRING(DPMember.DPId) + CHR(255) +
                             DPMember.HostTable    + CHR(255) +
                             DPMember.KeyValue     + CHR(255) +
                             STRING(DPMember.ValidTo)
   Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
