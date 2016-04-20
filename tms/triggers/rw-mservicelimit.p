TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLimit OLD BUFFER oldMServiceLimit.

{HPD/HPDConst.i}

&IF {&MSERVICELIMIT_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/mservicelimit.i}

DEFINE VARIABLE llShouldBeOnHPD   AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE llWasOnHPD        AS LOGICAL INITIAL FALSE NO-UNDO.

llShouldBeOnHPD = fCheckHPDStatus(MServiceLimit.MsSeq,
                                  MServiceLimit.CustNum,
                                  MServiceLimit.EndTS).

IF NEW(MServiceLimit) AND llShouldBeOnHPD = FALSE
THEN RETURN.

IF NOT NEW(MServiceLimit) AND
   (MServiceLimit.MsSeq   <> oldMServiceLimit.MsSeq OR
    MServiceLimit.CustNum <> oldMServiceLimit.CustNum OR
    MServiceLimit.EndTS   <> oldMServiceLimit.EndTS)
THEN llWasOnHPD = fCheckHPDStatus(oldMServiceLimit.MsSeq,
                                  oldMServiceLimit.CustNum,
                                  oldMServiceLimit.EndTS).
ELSE llWasOnHPD = llShouldBeOnHPD.

IF llWasOnHPD = FALSE AND llShouldBeOnHPD = FALSE
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLimit"
   Common.RepLog.EventType = (IF NEW(MServiceLimit)
                              THEN "CREATE"
                              ELSE IF llWasOnHPD = TRUE AND llShouldBeOnHPD = FALSE
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i MServiceLimit . {&HPDKeyDelimiter} MSID}.
ELSE Common.RepLog.RowID    = STRING(ROWID(MServiceLimit)).

IF NOT NEW(MServiceLimit)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE MServiceLimit USING
      MSID
   TO oldMServiceLimit SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "MServiceLimit"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldMServiceLimit . {&HPDKeyDelimiter} MSID}
         .
   END.
END.

&ENDIF
