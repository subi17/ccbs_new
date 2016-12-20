TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DPMember OLD BUFFER oldDPMember.

{HPD/HPDConst.i}

&IF {&DPMEMBER_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llShouldBeOnHPD   AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE llWasOnHPD        AS LOGICAL INITIAL FALSE NO-UNDO.

IF DPMember.HostTable  = "MobSub"         AND
   DPMember.ValidFrom <= DPMember.ValidTo AND
   DPMember.ValidTo   >= ADD-INTERVAL(DATE(MONTH(TODAY),1,YEAR(TODAY)), -6, "months")
THEN llShouldBeOnHPD = TRUE.

IF NEW(DPMember) AND llShouldBeOnHPD = FALSE
THEN RETURN.

IF NOT NEW(DPMember) AND
   oldDPMember.HostTable = "MobSub"             AND
   oldDPMember.ValidFrom <= oldDPMember.ValidTo AND
   oldDPMember.ValidTo   >= ADD-INTERVAL(DATE(MONTH(TODAY),1,YEAR(TODAY)), -6, "months")
THEN llWasOnHPD = TRUE.

IF llWasOnHPD = FALSE AND llShouldBeOnHPD = FALSE
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RowID     = STRING(ROWID(DPMember))
   Common.RepLog.TableName = "DPMember"
   Common.RepLog.EventType = (IF NEW(DPMember)
                              THEN "CREATE"
                              ELSE IF llWasOnHPD = TRUE AND llShouldBeOnHPD = FALSE
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE"
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i DPMember . {&HPDKeyDelimiter} KeyValue DPMemberId}.
ELSE Common.RepLog.RowID    = STRING(ROWID(DPMember)).

IF NOT NEW(DPMember)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE DPMember USING
      KeyValue DPMemberId
   TO oldDPMember SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
   CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "DPMember"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldDPMember . {&HPDKeyDelimiter} KeyValue DPMemberId}
         .
   END.
END.

&ENDIF