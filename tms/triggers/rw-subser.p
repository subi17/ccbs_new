TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SubSer OLD BUFFER oldSubSer.

{Syst/tmsconst.i}
{HPD/HPDConst.i}

&IF {&SUBSER_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/check_mobsub.i SubSer MsSeq}

DEFINE VARIABLE llIsHPDService  AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE llWasHPDService AS LOGICAL INITIAL FALSE NO-UNDO.

IF LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) > 0
THEN llIsHPDService = TRUE.

/* If this is a new SubSer and SubSer servcom is not hpd service,
   we won't send the information */ 
IF NEW(SubSer) AND llIsHPDService = FALSE
THEN RETURN.

IF NOT NEW(SubSer) AND oldSubSer.ServCom <> SubSer.ServCom
THEN llWasHPDService = LOOKUP(oldSubSer.ServCom,{&HPD_SERVICES}) > 0.
ELSE llWasHPDService = llIsHPDService.

IF llWasHPDService = FALSE AND llIsHPDService = FALSE
THEN RETURN.

DEFINE BUFFER lbSubSer FOR SubSer.

/* We will send only the newest one */
FOR
   FIRST lbSubSer FIELDS (MsSeq ServCom SSDate) NO-LOCK USE-INDEX ServCom WHERE
      lbSubSer.MsSeq   = SubSer.MsSeq  AND
      lbSubSer.ServCom = SubSer.ServCom:

   IF lbSubSer.SSDate > SubSer.SSDate
   THEN RETURN.
END.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "SubSer"
   Mobile.RepLog.EventType = (IF NEW(SubSer)
                              THEN "CREATE"
                              ELSE IF llWasHPDService AND llMobSubWasAvailable AND (llIsHPDService = FALSE OR llMobSubIsAvailable = FALSE)
                              THEN "DELETE"
                              ELSE "MODIFY")
   Mobile.RepLog.EventTime = NOW
   .

IF Mobile.RepLog.EventType = "DELETE" 
THEN Mobile.RepLog.KeyValue = {HPD/keyvalue.i SubSer . {&HPDKeyDelimiter} MsSeq ServCom SSDate}.
ELSE Mobile.RepLog.RowID    = STRING(ROWID(SubSer)).

IF NOT NEW(SubSer)
THEN DO: 
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE SubSer USING
      MsSeq ServCom SSDate
   TO oldSubSer SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:  
     
      CREATE Mobile.RepLog.
      ASSIGN
         Mobile.RepLog.TableName = "SubSer"
         Mobile.RepLog.EventType = "DELETE"
         Mobile.RepLog.EventTime = NOW
         Mobile.RepLog.KeyValue  = {HPD/keyvalue.i oldSubSer . {&HPDKeyDelimiter} MsSeq ServCom SSDate}
         .
   END.
END.

&ENDIF