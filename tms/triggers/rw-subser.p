TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SubSer OLD BUFFER Oldbuf.

{tmsconst.i}

IF LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) > 0
THEN DO:

   CREATE Mobile.RepLog.
   ASSIGN
      Mobile.RepLog.RecordId  = RECID(SubSer)
      Mobile.RepLog.TableName = "SubSer"
      Mobile.RepLog.EventType = (IF NEW(SubSer) THEN "CREATE" ELSE "MODIFY")
      Mobile.RepLog.EventTS   = DATETIME(TODAY,MTIME).

   IF NEW(SubSer) THEN
      Mobile.RepLog.KeyValue  = STRING(SubSer.MsSeq) + CHR(255) + SubSer.ServCom +
                                CHR(255) + STRING(SubSer.SSDate).
   ELSE
      Mobile.RepLog.KeyValue  = STRING(Oldbuf.MsSeq) + CHR(255) + Oldbuf.ServCom +
                                CHR(255) + STRING(Oldbuf.SSDate).
END.
