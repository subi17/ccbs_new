TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ServiceLCounter OLD BUFFER oldServiceLCounter.

{HPD/HPDConst.i}

&IF {&SERVICELCOUNTER_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llShouldBeOnHPD AS LOGICAL NO-UNDO.
DEFINE VARIABLE llWasOnHPD      AS LOGICAL NO-UNDO.

llShouldBeOnHPD = LOOKUP(STRING(ServiceLCounter.SlSeq),"{&DSS_SLSEQS}") > 0.

IF NEW(ServiceLCounter) AND NOT llShouldBeOnHPD
THEN RETURN.

IF NOT NEW(ServiceLCounter)
THEN llWasOnHPD = LOOKUP(STRING(oldServiceLCounter.SlSeq),"{&DSS_SLSEQS}") > 0.

IF llShouldBeOnHPD = FALSE AND llWasOnHPD = FALSE
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "ServiceLCounter"
   Common.RepLog.EventType = (IF NEW(ServiceLCounter)
                               THEN "CREATE"
                               ELSE IF llWasOnHPD AND NOT llShouldBeOnHPD
                               THEN "DELETE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE"
THEN Common.RepLog.KeyValue = IF ServiceLCounter.MsSeq = 0
                              THEN {HPD/keyvalue.i ServiceLCounter . {&HPDKeyDelimiter} CustNum SlSeq Period}
                              ELSE {HPD/keyvalue.i ServiceLCounter . {&HPDKeyDelimiter} MsSeq SlSeq Period}.
ELSE Common.RepLog.RowID    = STRING(ROWID(ServiceLCounter)).


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