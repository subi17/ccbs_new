TRIGGER PROCEDURE FOR REPLICATION-WRITE OF FixedFee OLD BUFFER oldFixedFee.

{HPD/HPDConst.i}

&IF {&FIXEDFEE_WRITE_TRIGGER_ACTIVE} &THEN

/* If hosttable is not mobsub, we won't send the information */ 
IF NEW (FixedFee) AND FixedFee.HostTable NE "MobSub"
THEN RETURN.

IF NOT NEW(FixedFee) AND FixedFee.HostTable NE "MobSub" AND oldFixedFee.HostTable NE "MobSub"
THEN RETURN.

{triggers/check_mobsub.i FixedFee KeyValue}

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "FixedFee"
   Common.RepLog.EventType = (IF NEW(FixedFee)
                              THEN "CREATE"
                              ELSE IF (llMobSubWasAvailable AND llMobSubIsAvailable = FALSE) OR
                                      (FixedFee.HostTable NE "MobSub" AND oldFixedFee.HostTable = "MobSub")
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER FixedFee:HANDLE)
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i FixedFee . {&HPDKeyDelimiter} FFNum}.
ELSE Common.RepLog.RowID    = STRING(ROWID(FixedFee)).

IF NOT NEW(FixedFee)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE FixedFee USING
      FFNum
   TO oldFixedFee SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName  = "FixedFee"
         Common.RepLog.EventType  = "DELETE"
         Common.RepLog.EventTime  = NOW
         Common.RepLog.TenantName = fRepLogTenantName(BUFFER oldFixedFee:HANDLE)
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldFixedFee . {&HPDKeyDelimiter} FFNum}
         .
   END.
END.

&ENDIF