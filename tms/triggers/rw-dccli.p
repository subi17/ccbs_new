TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DCCLI OLD BUFFER oldDCCLI.

{HPD/HPDConst.i}
   
&IF {&DCCLI_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/check_mobsub.i DCCLI MsSeq}

{triggers/replog_tenantname.i}

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = (IF NEW(DCCLI)
                              THEN "CREATE"
                              ELSE IF llMobSubWasAvailable AND llMobSubIsAvailable = FALSE
                              THEN "DELETE"
                              ELSE "MODIFY")
   Mobile.RepLog.EventTime = NOW
   Mobile.RepLog.TenantName = fRepLogTenantName(BUFFER DCCLI:HANDLE)
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
         Mobile.RepLog.TableName  = "DCCLI"
         Mobile.RepLog.EventType  = "DELETE"
         Mobile.RepLog.EventTime  = NOW
         Mobile.RepLog.TenantName = fRepLogTenantName(BUFFER oldDCCLI:HANDLE)
         Mobile.RepLog.KeyValue   = {HPD/keyvalue.i oldDCCLI . {&HPDKeyDelimiter} PerContractID}
         .
   END.
END.

&ENDIF
