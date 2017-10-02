TRIGGER PROCEDURE FOR REPLICATION-DELETE OF Invoice.

{HPD/HPDTrigger.i}

&IF {&INVOICE_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW Invoice
THEN RETURN.

/* Only require for printed Service Invoice */
IF Invoice.InvCfg[1] OR Invoice.InvType NE 1
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName  = "Invoice"
   Common.RepLog.EventType  = "DELETE"
   Common.RepLog.EventTime  = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER Invoice:HANDLE)
   Common.RepLog.KeyValue  = STRING(Invoice.InvNum)
   .

&ENDIF