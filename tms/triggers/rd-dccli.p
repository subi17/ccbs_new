TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DCCLI.

{HPD/HPDConst.i}

&IF {&DCCLI_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW DCCLI
THEN RETURN.

IF DCCLI.Brand NE "1"
THEN RETURN.

DEFINE VARIABLE llFixedFeeOK AS LOGICAL INITIAL TRUE NO-UNDO.

IF DCCLI.DCEvent = "RVTERM12"
THEN DO:      
   llFixedFeeOK = FALSE.
   FOR
      EACH FixedFee FIELDS (Brand HostTable KeyValue SourceTable SourceKey) NO-LOCK WHERE
         FixedFee.Brand       = "1"                 AND
         FixedFee.HostTable   = "MobSub"            AND
         FixedFee.KeyValue    = STRING(DCCLI.MSSeq) AND
         FixedFee.SourceTable = "DCCLI":
            
      llFixedFeeOK = TRUE.
      
      IF FixedFee.SourceKey NE STRING(DCCLI.PerContractID)
      THEN DO:
         llFixedFeeOK = FALSE.
         LEAVE.
      END.
   END.
END.

IF NOT llFixedFeeOK
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.EventTime = NOW
   Mobile.RepLog.KeyValue  = {HPD/keyvalue.i DCCLI . {&HPDKeyDelimiter} PerContractID} 
   .

&ENDIF