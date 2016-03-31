TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SubInvoice OLD BUFFER oldSubInvoice.

{HPD/HPDConst.i}

&IF {&SUBINVOICE_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llInvoiceInHPD AS LOGICAL INITIAL FALSE NO-UNDO.

FOR FIRST Invoice FIELDS (InvNum InvType InvCfg[1]) NO-LOCK WHERE
   Invoice.InvNum = SubInvoice.InvNum AND
   Invoice.InvType = 1                AND
   InvCfg[1]       = FALSE:
   
   llInvoiceInHPD = TRUE.
      
END.

/* We won't send new subinvoice if invoice is not in HPD */
IF NOT llInvoiceInHPD
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "SubInvoice"
   Common.RepLog.EventType = (IF NEW(SubInvoice)
                               THEN "CREATE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i SubInvoice . {&HPDKeyDelimiter} InvNum SubInvNum}
ELSE Common.RepLog.RowID    = STRING(ROWID(SubInvoice)).


IF NOT NEW(SubInvoice)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.
      
   BUFFER-COMPARE SubInvoice USING
      InvNum
      SubInvNum
   TO oldSubInvoice SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "SubInvoice"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldSubInvoice . {&HPDKeyDelimiter} InvNum SubInvNum}
         .
   END.
END.

&ENDIF