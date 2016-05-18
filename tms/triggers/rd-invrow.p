TRIGGER PROCEDURE FOR REPLICATION-DELETE OF InvRow.

{HPD/HPDConst.i}

&IF {&INVROW_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW InvRow
THEN RETURN.

DEFINE VARIABLE llInvoiceInHPD AS LOGICAL INITIAL FALSE NO-UNDO.

FOR FIRST Invoice FIELDS (InvNum InvType InvCfg[1]) NO-LOCK WHERE
   Invoice.InvNum = InvRow.InvNum AND
   Invoice.InvType = 1                AND
   InvCfg[1]       = FALSE:
   
   llInvoiceInHPD = TRUE.
      
END.

IF NOT llInvoiceInHPD
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "InvRow"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i InvRow . {&HPDKeyDelimiter} InvNum SubInvNum InvRowNum}
   .

&ENDIF