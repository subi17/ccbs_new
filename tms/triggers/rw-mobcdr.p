TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MobCDR OLD BUFFER oldMobCDR.

{HPD/HPDConst.i}

&IF {&MOBCDR_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new error cdr, we won't send the data */ 
IF NEW(MobCDR) AND MobCDR.ErrorCode > 0
THEN RETURN.

/* If this is an error cdr, we won't send the data */ 
IF NOT NEW(MobCDR) AND MobCDR.ErrorCode > 0 AND oldMobCDR.ErrorCode > 0
THEN RETURN.

{triggers/replog_tenantname.i}

/* If this is an old cdr which is changed to error status
   will will send it as delete type */
CREATE mcdr.RepLog.
ASSIGN
   mcdr.RepLog.TableName  = "MobCDR"
   mcdr.RepLog.EventType  = (IF NEW(MobCDR)
                            THEN "CREATE"
                            ELSE IF MobCDR.ErrorCode > 0 AND oldMobCDR.ErrorCode = 0
                            THEN "DELETE"
                            ELSE "MODIFY") 
   mcdr.RepLog.EventTime  = NOW
   mcdr.RepLog.TenantName = fRepLogTenantName(BUFFER MobCDR:HANDLE)
   .

IF mcdr.RepLog.EventType = "DELETE"
THEN mcdr.RepLog.KeyValue = {HPD/keyvalue.i MobCDR . {&HPDKeyDelimiter} MsSeq DtlSeq DateSt}.
ELSE mcdr.RepLog.RowID    = STRING(ROWID(MobCDR)).
   
IF NOT NEW(MobCDR)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE MobCDR USING
      MsSeq
      DtlSeq
      DateSt
   TO oldMobCDR SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE mcdr.RepLog.
      ASSIGN
         mcdr.RepLog.TableName = "MobCDR"
         mcdr.RepLog.EventType = "DELETE"
         mcdr.RepLog.EventTime = NOW
         mcdr.RepLog.TenantName = fRepLogTenantName(BUFFER oldMobCDR:HANDLE)
         mcdr.RepLog.KeyValue  = {HPD/keyvalue.i oldMobCDR . {&HPDKeyDelimiter} MsSeq DtlSeq DateSt}
         .
   END.
END.

&ENDIF