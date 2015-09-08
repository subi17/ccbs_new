TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SubsTerminal OLD BUFFER Oldbuf.

CREATE OrderCanal.RepLog.
ASSIGN
   OrderCanal.RepLog.RecordId  = RECID(SubsTerminal)
   OrderCanal.RepLog.TableName = "SubsTerminal"
   OrderCanal.RepLog.EventType = (IF NEW(SubsTerminal) THEN "CREATE" ELSE "MODIFY")
   OrderCanal.RepLog.KeyValue  = STRING((IF NEW(SubsTerminal) THEN 
                                        SubsTerminal.TerminalId
                                     ELSE Oldbuf.TerminalId))
   OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
