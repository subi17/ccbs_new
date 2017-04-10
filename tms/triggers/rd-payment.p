TRIGGER PROCEDURE FOR REPLICATION-DELETE OF Payment.

{HPD/HPDConst.i}

&IF {&PAYMENT_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW Payment
THEN RETURN.

/* Only require for printed Service Payment */
IF LOOKUP(STRING(Payment.PaymType),"1,8") = 0 OR Payment.InvDate < 06/01/2013
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName  = "Payment"
   Common.RepLog.EventType  = "DELETE"
   Common.RepLog.EventTime  = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER Payment:HANDLE)
   Common.RepLog.KeyValue  = STRING(Payment.Voucher)
   .

&ENDIF