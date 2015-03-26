TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SubSer.

{tmsconst.i}

IF LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) > 0
THEN DO:

   CREATE Mobile.RepLog.
   ASSIGN
      Mobile.RepLog.RecordId  = RECID(SubSer)
      Mobile.RepLog.TableName = "SubSer"
      Mobile.RepLog.EventType = "DELETE"
      Mobile.RepLog.KeyValue  = STRING(SubSer.MsSeq) + CHR(255) + SubSer.ServCom +
                                CHR(255) + STRING(SubSer.SSDate).
      Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END.
