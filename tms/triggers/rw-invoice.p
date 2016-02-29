TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Invoice OLD BUFFER oldInvoice.

{HPD/HPDConst.i}

&IF {&INVOICE_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new invoice and
   invoice is not printed or invoice type is not Service Invoice,
   we won't send the information */ 
IF NEW(Invoice) AND ( Invoice.InvCfg[1] OR Invoice.InvType NE 1 )
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Invoice"
   Common.RepLog.EventType = (IF NEW(Invoice)
                               THEN "CREATE"
                               ELSE IF Invoice.InvCfg[1] OR Invoice.InvType NE 1
                               THEN "DELETE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = STRING(Invoice.InvNum).
ELSE Common.RepLog.RowID    = STRING(ROWID(Invoice)).


IF (NOT NEW(Invoice)) AND
   oldInvoice.InvNum <> Invoice.InvNum
THEN DO:   
   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.TableName = "Invoice"
      Common.RepLog.EventType = "DELETE"
      Common.RepLog.EventTime = NOW
      Common.RepLog.KeyValue  = STRING(oldInvoice.InvNum)
      .
END.

&ENDIF