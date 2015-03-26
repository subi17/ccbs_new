TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MobSub OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(MobSub) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(MobSub)
      Ordercanal.RepLog.TableName = "MobSub"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  = STRING(MobSub.MsSeq)
      Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(MobSub) THEN DO: */
ELSE DO:
   BUFFER-COMPARE MobSub USING MsSeq CustNum CLI CLIType TariffBundle TariffActDate PayType
   ActivationTS MultiSimType MultiSimID IMSI BarrCode TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(MobSub)
         Ordercanal.RepLog.TableName = "MobSub"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  = STRING(Oldbuf.MsSeq)
         Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
