TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DCCLI OLD BUFFER oldDCCLI.

{HPD/HPDConst.i}
   
IF NEW(DCCLI) THEN DO:
   IF DCCLI.percontractid > 0 AND
      NOT CAN-FIND(FIRST DCCLI_new NO-LOCK WHERE
                         DCCLI_new.percontractid = DCCLI.percontractid) THEN DO:
      CREATE DCCLI_new.
      BUFFER-COPY DCCLI TO DCCLI_new.
   END.
END.
ELSE IF DCCLI.PerContractID > 0 THEN DO:
   FIND DCCLI_new EXCLUSIVE-LOCK WHERE
        DCCLI_new.percontractid = DCCLI.percontractid no-error.
   IF AVAIL DCCLI_new THEN
      BUFFER-COPY DCCLI TO DCCLI_new NO-ERROR.
END.


&IF {&DCCLI_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/check_mobsub.i DCCLI MsSeq}

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = (IF NEW(DCCLI)
                              THEN "CREATE"
                              ELSE IF llMobSubWasAvailable AND llMobSubIsAvailable = FALSE
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
