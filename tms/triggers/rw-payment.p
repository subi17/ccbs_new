TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Payment OLD BUFFER oldPayment.

{HPD/HPDConst.i}

&IF {&PAYMENT_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llShouldBeOnHPD AS LOGICAL NO-UNDO.
DEFINE VARIABLE llIsOnHPD       AS LOGICAL NO-UNDO.

/* If this is a new Payment and
   Payment is not printed or Payment type is not Service Payment,
   we won't send the information */
llShouldBeOnHPD = ( LOOKUP(STRING(Payment.PaymType),"1,8") > 1 AND Payment.InvDate >= 06/01/2013 ).
   
IF NEW(Payment) AND NOT llShouldBeOnHPD 
THEN RETURN.

IF NOT NEW(Payment)
THEN llIsOnHPD = ( LOOKUP(STRING(oldPayment.PaymType),"1,8") > 1 AND oldPayment.InvDate >= 06/01/2013 ).

IF llIsOnHPD = FALSE AND llShouldBeOnHPD = FALSE
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Payment"
   Common.RepLog.EventType = (IF NEW(Payment)
                              THEN "CREATE"
                              ELSE IF llIsOnHPD AND NOT llShouldBeOnHPD 
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER Payment:HANDLE)
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = STRING(Payment.Voucher).
ELSE Common.RepLog.RowID    = STRING(ROWID(Payment)).


IF (NOT NEW(Payment)) AND
   oldPayment.Voucher <> Payment.Voucher
THEN DO:   
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.TableName = "Payment"
      Common.RepLog.EventType = "DELETE"
      Common.RepLog.EventTime = NOW
      Common.RepLog.TenantName = fRepLogTenantName(BUFFER oldPayment:HANDLE)
      Common.RepLog.KeyValue  = STRING(oldPayment.Voucher)
      .
END.

&ENDIF