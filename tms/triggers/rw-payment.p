TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Payment OLD BUFFER oldPayment.

{HPD/HPDConst.i}

&IF {&PAYMENT_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new Payment and
   Payment is not printed or Payment type is not Service Payment,
   we won't send the information */ 
IF NEW(Payment) AND ( LOOKUP(STRING(Payment.PaymType),"1,8") = 0 OR Payment.InvDate < 06/01/2013 )
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Payment"
   Common.RepLog.EventType = (IF NEW(Payment)
                              THEN "CREATE"
                              ELSE IF LOOKUP(STRING(Payment.PaymType),"1,8") = 0 OR Payment.InvDate < 06/01/2013
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
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
      Common.RepLog.KeyValue  = STRING(oldPayment.Voucher)
      .
END.

&ENDIF