TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Barring OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(Barring) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(Barring)
      Ordercanal.RepLog.TableName = "Barring"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  =  STRING(Barring.MsSeq) + CHR(255) +
                                     Barring.BarringCode + CHR(255) +
                                     Barring.BarringStatus + CHR(255) +
                                     STRING(Barring.EventTS)
      Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW  THEN DO: */
ELSE DO:
   BUFFER-COMPARE Barring 
      USING MsSeq 
            BarringCode 
            BarringStatus
            UserCode
            EventTS TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(Barring)
         Ordercanal.RepLog.TableName = "Barring"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  =  STRING(Barring.MsSeq) + CHR(255) +
                                        Barring.BarringCode + CHR(255) +
                                        Barring.BarringStatus + CHR(255) +
                                        STRING(Barring.EventTS)
         Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */

