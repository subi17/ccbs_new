TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DiscountPlan OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(DiscountPlan) THEN DO:
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RecordId  = RECID(DiscountPlan)
      Common.RepLog.TableName = "DiscountPlan"
      Common.RepLog.EventType = "CREATE"
      Common.RepLog.KeyValue  = STRING(DiscountPlan.DPId)                       
      Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(DiscountPlan) THEN DO: */
ELSE DO:
   BUFFER-COMPARE DiscountPlan USING DPId ValidFrom ValidTo DPRuleID DPName DPUnit BillCode ValidPeriods
                                  TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(DiscountPlan)
         Common.RepLog.TableName = "DiscountPlan"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(Oldbuf.DPId)
         Common.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
