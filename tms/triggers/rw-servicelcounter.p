TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ServiceLCounter OLD BUFFER oldServiceLCounter.

{HPD/HPDConst.i}

&IF {&SERVICELCOUNTER_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/servicelcounter.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RowID     = STRING(ROWID(ServiceLCounter))
   Common.RepLog.TableName = "ServiceLCounter"
   Common.RepLog.EventType = (IF NEW(ServiceLCounter)
                               THEN "CREATE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .


IF NOT NEW(ServiceLCounter)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.
      
   BUFFER-COMPARE ServiceLCounter USING
      MsSeq CustNum SlSeq Period
   TO oldServiceLCounter SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE billing.RepLog.
      ASSIGN
         billing.RepLog.TableName = "ServiceLCounter"
         billing.RepLog.EventType = "DELETE"
         billing.RepLog.EventTime = NOW
         billing.RepLog.KeyValue  = IF oldServiceLCounter.MsSeq = 0
                                    THEN {HPD/keyvalue.i oldServiceLCounter . {&HPDKeyDelimiter} CustNum SlSeq Period}
                                    ELSE {HPD/keyvalue.i oldServiceLCounter . {&HPDKeyDelimiter} MsSeq SlSeq Period}
         .
   END.
END.

&ENDIF