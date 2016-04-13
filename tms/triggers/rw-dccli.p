TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DCCLI OLD BUFFER oldDCCLI.

{HPD/HPDConst.i}

&IF {&DCCLI_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/check_mobsub.i DCCLI MsSeq}
{triggers/dccli.i}

DEFINE VARIABLE llFixedFeeOK    AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE llFixedFeeWasOK AS LOGICAL INITIAL TRUE NO-UNDO.

IF DCCLI.DCEvent = "RVTERM12"
THEN llFixedFeeOK = fCheckFixedFee(STRING(DCCLI.MSSeq), STRING(DCCLI.PerContractID)). 

IF NEW(DCCLI) AND NOT llFixedFeeOK
THEN RETURN.

IF NOT NEW(DCCLI) AND
   oldDCCLI.DCEvent = "RVTERM12"
THEN DO:
   IF (DCCLI.MSSeq <> oldDCCLI.MSSeq OR
       DCCLI.PerContractID <> oldDCCLI.PerContractID)
   THEN llFixedFeeWasOK = fCheckFixedFee(STRING(oldDCCLI.MSSeq), STRING(oldDCCLI.PerContractID)). 
   ELSE llFixedFeeWasOK = llFixedFeeOK.
END.

IF llFixedFeeWasOK = FALSE AND llFixedFeeOK = FALSE
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = (IF NEW(DCCLI)
                              THEN "CREATE"
                              ELSE IF (llMobSubWasAvailable AND llMobSubIsAvailable = FALSE) OR (llFixedFeeOK = FALSE AND llFixedFeeWasOK)
                              THEN "DELETE"
                              ELSE "MODIFY")
   Mobile.RepLog.EventTime = NOW
   .

IF Mobile.RepLog.EventType = "DELETE" 
THEN Mobile.RepLog.KeyValue = {HPD/keyvalue.i DCCLI . {&HPDKeyDelimiter} PerContractID}.
ELSE Mobile.RepLog.RowID    = STRING(ROWID(DCCLI)).

IF NOT NEW(DCCLI)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE DCCLI USING
      PerContractID
   TO oldDCCLI SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Mobile.RepLog.
      ASSIGN
         Mobile.RepLog.TableName = "DCCLI"
         Mobile.RepLog.EventType = "DELETE"
         Mobile.RepLog.EventTime = NOW
         Mobile.RepLog.KeyValue  = {HPD/keyvalue.i oldDCCLI . {&HPDKeyDelimiter} PerContractID}
         .
   END.
END.

&ENDIF