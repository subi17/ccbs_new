TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLPool OLD BUFFER oldMServiceLPool.

{HPD/HPDConst.i}

&IF {&MSERVICELPOOL_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/mservicelimit.i}

DEFINE VARIABLE llShouldBeOnHPD   AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE llWasOnHPD        AS LOGICAL INITIAL FALSE NO-UNDO.

llShouldBeOnHPD = fCheckHPDStatus(MServiceLPool.MsSeq,
                                  MServiceLPool.CustNum,
                                  MServiceLPool.EndTS).

IF NEW(MServiceLPool) AND llShouldBeOnHPD = FALSE
THEN RETURN.

IF NOT NEW(MServiceLPool) AND
   (MServiceLPool.MsSeq   <> oldMServiceLPool.MsSeq OR
    MServiceLPool.CustNum <> oldMServiceLPool.CustNum OR
    MServiceLPool.EndTS   <> oldMServiceLPool.EndTS)
THEN llWasOnHPD = fCheckHPDStatus(oldMServiceLPool.MsSeq,
                                  oldMServiceLPool.CustNum,
                                  oldMServiceLPool.EndTS).
ELSE llWasOnHPD = llShouldBeOnHPD.

IF llWasOnHPD = FALSE AND llShouldBeOnHPD = FALSE
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLPool"
   Common.RepLog.EventType = (IF NEW(MServiceLPool)
                              THEN "CREATE"
                              ELSE IF llWasOnHPD = TRUE AND llShouldBeOnHPD = FALSE
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i MServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}.
ELSE Common.RepLog.RowID    = STRING(ROWID(MServiceLPool)).

IF NOT NEW(MServiceLPool)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE MServiceLPool USING
      CustNum MsSeq SLSeq EndTS
   TO oldMServiceLPool SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "MServiceLPool"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldMServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}
         .
   END.
END.

&ENDIF