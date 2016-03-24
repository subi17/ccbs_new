TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SubSer OLD BUFFER oldSubSer.

{Syst/tmsconst.i}
{HPD/HPDConst.i}

&IF {&SUBSER_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE VARIABLE llMobSubAvailable AS LOGICAL INITIAL FALSE NO-UNDO.

FOR FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
   MobSub.MsSeq = SubSer.MsSeq:
   llMobSubAvailable = TRUE.
END.

/* If this is a new SubSer and SubSer servcom is not hpd service,
   we won't send the information */ 
IF NEW(SubSer) AND ( LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) = 0 OR llMobSubAvailable = FALSE )
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
                               ELSE IF LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) = 0 OR llMobSubAvailable = FALSE
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