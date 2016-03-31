TRIGGER PROCEDURE FOR REPLICATION-WRITE OF PrepEDR OLD BUFFER oldPrepEDR.

{HPD/HPDConst.i}

&IF {&PREPEDR_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new error cdr, we won't send the data */ 
IF NEW(PrepEDR) AND PrepEDR.ErrorCode > 0
THEN RETURN.

/* If this is an error prepedr, we won't send the data */ 
IF NOT NEW(PrepEDR) AND PrepEDR.ErrorCode > 0 AND oldPrepEDR.ErrorCode > 0
THEN RETURN.

/* If this is an old cdr which is changed to error status
   will will send it as delete type */
CREATE mcdr.RepLog.
ASSIGN
   mcdr.RepLog.TableName = "PrepEDR"
   mcdr.RepLog.EventType = (IF NEW(PrepEDR)
                            THEN "CREATE"
                            ELSE IF PrepEDR.ErrorCode > 0 AND oldPrepEDR.ErrorCode = 0
                            THEN "DELETE"
                            ELSE "MODIFY") 
   mcdr.RepLog.EventTime = NOW
   .

IF mcdr.RepLog.EventType  = "DELETE"
THEN mcdr.RepLog.KeyValue = "FEES" + {&HPDKeyDelimiter} +
                            STRING(PrepEDR.DtlSeq) + {&HPDKeyDelimiter} +
                            STRING(PrepEDR.DateSt)
ELSE mcdr.RepLog.RowID    = STRING(ROWID(PrepEDR)).
   
IF NOT NEW(PrepEDR)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE PrepEDR USING
      MSCID
      DtlSeq
      DateSt
   TO oldPrepEDR SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE mcdr.RepLog.
      ASSIGN
         mcdr.RepLog.TableName = "PrepEDR"
         mcdr.RepLog.EventType = "DELETE"
         mcdr.RepLog.EventTime = NOW
         mcdr.RepLog.KeyValue  = "FEES" + {&HPDKeyDelimiter} +
                                 STRING(oldPrepEDR.DtlSeq) + {&HPDKeyDelimiter} +
                                 STRING(oldPrepEDR.DateSt)
         .
   END.
END.

&ENDIF