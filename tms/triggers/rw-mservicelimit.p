TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLimit OLD BUFFER oldMServiceLimit.

{HPD/HPDConst.i}

&IF {&MSERVICELIMIT_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llShouldBeOnHPD   AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE llWasOnHPD        AS LOGICAL INITIAL FALSE NO-UNDO.

FUNCTION fCheckHPDStatus RETURNS LOGICAL
   (iiMSSeq AS INTEGER,
    iiSLSeq AS INTEGER,
    iiCustNum AS INTEGER):
       
   FOR FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
      MobSub.MsSeq = iiMsSeq:
   
      IF iiCustNum > 0 AND
         CAN-FIND(FIRST ServiceLimit NO-LOCK WHERE
            ServiceLimit.SlSeq = iiSlSeq AND
            ServiceLimit.GroupCode BEGINS "DSS")
      THEN RETURN TRUE.
      ELSE IF iiCustNum = 0 OR iiCustNum = ?
      THEN RETURN TRUE.
   END.
   
   RETURN FALSE.
END.

llShouldBeOnHPD = fCheckHPDStatus(MServiceLimit.MsSeq,
                                  MServiceLimit.SlSeq,
                                  MServiceLimit.CustNum).

IF NEW(MServiceLimit) AND llShouldBeOnHPD = FALSE
THEN RETURN.

IF NOT NEW(MServiceLimit) AND
   (MServiceLimit.MsSeq   <> oldMServiceLimit.MsSeq OR
    MServiceLimit.SlSeq   <> oldMServiceLimit.SlSeq
    MServiceLimit.CustNum <> oldMServiceLimit.CustNum)
THEN llWasOnHPD = fCheckHPDStatus(oldMServiceLimit.MsSeq,
                                  oldMServiceLimit.SlSeq,
                                  oldMServiceLimit.CustNum).
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
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i MServiceLimit . {&HPDKeyDelimiter} MsSeq DialType SLSeq EndTS}.
ELSE Common.RepLog.RowID    = STRING(ROWID(MServiceLimit)).

IF NOT NEW(MServiceLimit)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE MServiceLimit USING
      MsSeq DialType SLSeq EndTS
   TO oldMServiceLimit SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "MServiceLimit"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldMServiceLimit . {&HPDKeyDelimiter} MsSeq DialType SLSeq EndTS}
         .
   END.
END.

&ENDIF