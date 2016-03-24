TRIGGER PROCEDURE FOR REPLICATION-WRITE OF DCCLI OLD BUFFER oldDCCLI.

{HPD/HPDConst.i}

&IF {&DCCLI_WRITE_TRIGGER_ACTIVE} &THEN

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

llMobSubIsAvailable = fCheckMobSub(DCCLI.MsSeq).

IF NEW(DCCLI) AND llMobSubIsAvailable = FALSE
THEN RETURN.

IF (NOT NEW(DCCLI)) AND DCCLI.MsSeq <> oldDCCLI.MsSeq
THEN llMobSubWasAvailable = fCheckMobSub(oldDCCLI.MsSeq).
ELSE llMobSubWasAvailable = llMobSubIsAvailable.

IF llMobSubIsAvailable = FALSE AND llMobSubWasAvailable = FALSE
THEN RETURN.

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
THEN Mobile.RepLog.KeyValue = STRING(DCCLI.PerContractID).
ELSE Mobile.RepLog.RowID    = STRING(ROWID(DCCLI)).

IF NOT NEW(DCCLI)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE DCCLI USING
      CustNum MsSeq SLSeq
   TO oldDCCLI SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Mobile.RepLog.
      ASSIGN
         Mobile.RepLog.TableName = "DCCLI"
         Mobile.RepLog.EventType = "DELETE"
         Mobile.RepLog.EventTime = NOW
         Mobile.RepLog.KeyValue  = STRING(oldDCCLI.PerContractID)
         .
   END.
END.

&ENDIF