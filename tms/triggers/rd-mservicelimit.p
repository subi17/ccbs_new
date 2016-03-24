TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MServiceLimit.

{HPD/HPDConst.i}

&IF {&MSERVICELIMIT_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW MServiceLimit
THEN RETURN.

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

llWasOnHPD = fCheckHPDStatus(MServiceLimit.MsSeq,
                             MServiceLimit.SlSeq,
                             MServiceLimit.CustNum).

IF NOT llWasOnHPD
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLimit"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i MServiceLimit . {&HPDKeyDelimiter} MsSeq DialType SLSeq EndTS}
   .

&ENDIF