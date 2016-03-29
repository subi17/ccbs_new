TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DCCLI OLD BUFFER oldDCCLI.

{HPD/HPDConst.i}

&IF {&DCCLI_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/check_mobsub.i DCCLI MsSeq}

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

IF NEW(DCCLI) AND NOT llFixedFeeOK
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = (IF NEW(DCCLI)
                              THEN "CREATE"
                              ELSE IF (llMobSubWasAvailable AND llMobSubIsAvailable = FALSE) OR NOT llFixedFeeOK
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