TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Invoice OLD BUFFER oldInvoice.

{HPD/HPDConst.i}

&IF {&INVOICE_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new invoice and
   invoice is not printed or invoice type is not Service Invoice,
   we won't send the information */ 
IF NEW(Invoice) AND ( Invoice.InvCfg[1] OR Invoice.InvType NE 1 )
THEN RETURN.

IF NOT NEW(Invoice) AND
   ( Invoice.InvCfg[1] OR Invoice.InvType NE 1 ) AND
   ( oldInvoice.InvCfg[1] OR oldInvoice.InvType NE 1 )
THEN RETURN.    


FUNCTION fProcessInvRandSubIn RETURNS LOGICAL
   (iiInvNum AS INTEGER,
    icEventType AS CHARACTER):
       
   FOR EACH InvRow FIELDS (InvNum) NO-LOCK WHERE InvRow.InvNum = iiInvNum:
      
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RowID     = STRING(ROWID(InvRow))
         Common.RepLog.TableName = "InvRow"
         Common.RepLog.EventType = icEventType
         Common.RepLog.EventTime = NOW
         .      
   
   END.

   FOR EACH SubInvoice FIELDS (InvNum SubInvNum) NO-LOCK WHERE SubInvoice.InvNum = iiInvNum:
      
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RowID     = STRING(ROWID(SubInvoice))
         Common.RepLog.TableName = "SubInvoice"
         Common.RepLog.EventType = icEventType
         Common.RepLog.EventTime = NOW
         .      
   
   END.

END FUNCTION.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Invoice"
   Common.RepLog.EventType = (IF NEW(Invoice)
                               THEN "CREATE"
                               ELSE IF (Invoice.InvCfg[1] OR Invoice.InvType NE 1) AND
                                       (oldInvoice.InvCfg[1] = FALSE AND oldInvoice.InvType = 1) 
                               THEN "DELETE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN DO:
   Common.RepLog.KeyValue = STRING(Invoice.InvNum).
   fProcessInvRandSubIn(Invoice.InvNum, "DELETE").
END.
ELSE Common.RepLog.RowID    = STRING(ROWID(Invoice)).

IF Common.RepLog.EventType = "MODIFY" AND
   (oldInvoice.InvCfg[1] OR oldInvoice.InvType NE 1) AND
   Invoice.InvCfg[1] = FALSE AND Invoice.InvType = 1
THEN fProcessInvRandSubIn(Invoice.InvNum, "MODIFY").

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