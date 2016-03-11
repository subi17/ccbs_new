TRIGGER PROCEDURE FOR REPLICATION-WRITE OF PrepCDR OLD BUFFER oldPrepCDR.

{HPD/HPDConst.i}

&IF {&PREPCDR_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new error cdr, we won't send the data */ 
IF NEW(PrepCDR) AND PrepCDR.ErrorCode > 0
THEN RETURN.

/* If this is an old cdr which is changed to error status
   will will send it as delete type */
CREATE mcdr.RepLog.
ASSIGN
   mcdr.RepLog.TableName = "PrepCDR"
   mcdr.RepLog.EventType = (IF NEW(PrepCDR)
                            THEN "CREATE"
                            ELSE IF PrepCDR.ErrorCode > 0
                            THEN "DELETE"
                            ELSE "MODIFY") 
   mcdr.RepLog.EventTime = NOW
   .

IF mcdr.RepLog.EventType = "DELETE"
THEN mcdr.RepLog.KeyValue = {HPD/keyvalue.i PrepCDR . {&HPDKeyDelimiter} MsSeq DtlSeq DateSt}.
ELSE mcdr.RepLog.RowID    = STRING(ROWID(PrepCDR)).
   
IF NOT NEW(PrepCDR)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE PrepCDR USING
      MSCID
      DtlSeq
      DateSt
   TO oldPrepCDR SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE mcdr.RepLog.
      ASSIGN
         mcdr.RepLog.TableName = "PrepCDR"
         mcdr.RepLog.EventType = "DELETE"
         mcdr.RepLog.EventTime = NOW
         mcdr.RepLog.KeyValue  = {HPD/keyvalue.i oldPrepCDR . {&HPDKeyDelimiter} MsSeq DtlSeq DateSt}
         .
   END.
END.

&ENDIF