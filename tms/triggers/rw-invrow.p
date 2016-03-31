TRIGGER PROCEDURE FOR REPLICATION-WRITE OF InvRow OLD BUFFER oldInvRow.

{HPD/HPDConst.i}

&IF {&INVROW_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llInvoiceInHPD AS LOGICAL INITIAL FALSE NO-UNDO.

FOR FIRST Invoice FIELDS (InvNum InvType InvCfg[1]) NO-LOCK WHERE
   Invoice.InvNum = InvRow.InvNum AND
   Invoice.InvType = 1            AND
   InvCfg[1]       = FALSE:
   
   llInvoiceInHPD = TRUE.
      
END.

/* We won't send new InvRow if invoice is not in HPD */
IF NOT llInvoiceInHPD
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RowID     = STRING(ROWID(InvRow))
   Common.RepLog.TableName = "InvRow"
   Common.RepLog.EventType = (IF NEW(InvRow)
                              THEN "CREATE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF NOT NEW(InvRow)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.
      
   BUFFER-COMPARE InvRow USING
      InvNum
      SubInvNum
      InvRowNum
   TO oldInvRow SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "InvRow"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldInvRow . {&HPDKeyDelimiter} InvNum SubInvNum InvRowNum}
         .
   END.
END.

&ENDIF