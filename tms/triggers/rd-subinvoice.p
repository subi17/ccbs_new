TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SubInvoice.

{HPD/HPDConst.i}

&IF {&SUBINVOICE_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW SubInvoice
THEN RETURN.

DEFINE VARIABLE llInvoiceInHPD AS LOGICAL INITIAL FALSE NO-UNDO.

FOR FIRST Invoice FIELDS (InvNum InvType InvCfg[1]) NO-LOCK WHERE
   Invoice.InvNum = SubInvoice.InvNum AND
   Invoice.InvType = 1                AND
   InvCfg[1]       = FALSE:
   
   llInvoiceInHPD = TRUE.
      
END.

IF NOT llInvoiceInHPD
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "SubInvoice"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i SubInvoice . {&HPDKeyDelimiter} InvNum SubInvNum}
   .

&ENDIF