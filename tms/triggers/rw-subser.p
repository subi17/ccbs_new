TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SubSer OLD BUFFER oldSubSer.

{tmsconst.i}
{HPD/HPDConst.i}

&IF {&SUBSER_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new SubSer and SubSer servcom is not hpd service,
   we won't send the information */ 
IF NEW(SubSer) AND LOOKUP(SubSer.ServCom,{&HPD_SERVICES}) = 0
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.RowID     = STRING(ROWID(SubSer))
   Mobile.RepLog.TableName = "SubSer"
   Mobile.RepLog.EventType = (IF NEW(SubSer)
                               THEN "CREATE"
                               ELSE "MODIFY")
   Mobile.RepLog.EventTime = NOW
   .

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
         Mobile.RepLog.KeyValue  = STRING(oldSubSer.MsSeq) + {&HPDKeyDelimiter} +
                                   oldSubSer.ServCom + {&HPDKeyDelimiter} +
                                   STRING(oldSubSer.SSDate)
         .
   END.
END.

&ENDIF