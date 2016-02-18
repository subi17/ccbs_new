TRIGGER PROCEDURE FOR REPLICATION-DELETE OF Invoice.

{HPD/HPDConst.i}

&IF {&INVOICE_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW Invoice
THEN RETURN.

/* Only require for Service Invoice */
IF Invoice.InvType NE 1
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Invoice"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = STRING(Invoice.InvNum)
   .

&ENDIF