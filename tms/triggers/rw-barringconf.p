TRIGGER PROCEDURE FOR REPLICATION-WRITE OF BarringConf OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(BarringConf) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(BarringConf)
      Ordercanal.RepLog.TableName = "BarringConf"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  = STRING(BarringConf.BarringCode)
      Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW  THEN DO: */
ELSE DO:
   BUFFER-COMPARE BarringConf 
      USING BarringGroup 
            BarringCode 
            BarringStatus TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(BarringConf)
         Ordercanal.RepLog.TableName = "BarringConf"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  = STRING(Oldbuf.BarringCode)
         Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */

