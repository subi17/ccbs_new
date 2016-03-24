TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLPool OLD BUFFER oldMServiceLPool.

{HPD/HPDConst.i}

&IF {&MSERVICELPOOL_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llMobSubIsAvailable AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE llMobSubWasAvailable AS LOGICAL INITIAL FALSE NO-UNDO.

FUNCTION fCheckMobSub RETURNS LOGICAL
   (iiMsSeq AS INTEGER):

   FOR FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
      MobSub.MsSeq = iiMsSeq:
      RETURN TRUE.
   END.

   RETURN FALSE.

END.

llMobSubIsAvailable = fCheckMobSub(MServiceLPool.MsSeq).

IF NEW(MServiceLPool) AND llMobSubIsAvailable = FALSE
THEN RETURN.

IF (NOT NEW(MServiceLPool)) AND MServiceLPool.MsSeq <> oldMServiceLPool.MsSeq
THEN llMobSubWasAvailable = fCheckMobSub(oldMServiceLPool.MsSeq).
ELSE llMobSubWasAvailable = llMobSubIsAvailable.

IF llMobSubIsAvailable = FALSE AND llMobSubWasAvailable = FALSE
THEN RETURN.

DEFINE BUFFER lbMServiceLPool FOR MServiceLPool.

/* We will send only the newest one */
FOR
   FIRST lbMServiceLPool FIELDS (MsSeq SLSeq EndTS) NO-LOCK USE-INDEX MsSeq WHERE
      lbMServiceLPool.MsSeq = MServiceLPool.MsSeq  AND
      lbMServiceLPool.SlSeq = MServiceLPool.SlSeq:

   IF lbMServiceLPool.EndTS > MServiceLPool.EndTS
   THEN RETURN.
END.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLPool"
   Common.RepLog.EventType = (IF NEW(MServiceLPool)
                              THEN "CREATE"
                              ELSE IF llMobSubWasAvailable AND llMobIsSubAvailable = FALSE
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i MServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}.
ELSE Common.RepLog.RowID    = STRING(ROWID(MServiceLPool)).

IF NOT NEW(MServiceLPool)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE MServiceLPool USING
      CustNum MsSeq SLSeq
   TO oldMServiceLPool SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "MServiceLPool"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldMServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}
         .
   END.
END.

&ENDIF