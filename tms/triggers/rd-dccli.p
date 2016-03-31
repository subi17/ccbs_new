TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DCCLI.

{HPD/HPDConst.i}

&IF {&DCCLI_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW DCCLI
THEN RETURN.

{triggers/dccli.i}

DEFINE VARIABLE llFixedFeeOK AS LOGICAL INITIAL TRUE NO-UNDO.

IF DCCLI.DCEvent = "RVTERM12"
THEN llFixedFeeOK = fCheckFixedFee(STRING(DCCLI.MSSeq), STRING(DCCLI.PerContractID)). 

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