TRIGGER PROCEDURE FOR REPLICATION-WRITE OF CliType OLD BUFFER oldCliType.

{Syst/tmsconst.i}

{HPD/HPDConst.i}

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

&IF {&CLITYPE_WRITE_TRIGGER_ACTIVE} &THEN

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.RowID     = STRING(ROWID(CliType))
   Ordercanal.RepLog.TableName = "CliType"
   Ordercanal.RepLog.EventType = (IF NEW(CliType)
                           THEN "CREATE"
                           ELSE "MODIFY")
   Ordercanal.RepLog.EventTime = NOW
   .

IF NOT NEW(CliType)
THEN DO: 
   BUFFER-COMPARE CliType USING
      CliType
   TO oldCliType SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
   CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.TableName = "CliType"
         Ordercanal.RepLog.EventType = "DELETE"
         Ordercanal.RepLog.EventTime = NOW
         Ordercanal.RepLog.KeyValue  = SUBSTITUTE("&1",oldCliType.CliType)
         .
   END.

END.

&ENDIF

IF NOT NEW(CliType) 
THEN BUFFER-COMPARE CliType TO oldCliType SAVE RESULT IN llSameValues.

IF NOT llSameValues OR NEW(CliType) THEN
DO:
   FIND FIRST DumpFile NO-LOCK WHERE
              DumpFile.Brand     EQ Syst.Parameters:gcBrand AND
              DumpFile.DumpName  EQ {&DUMP_CLITYPE_TRACK}   NO-ERROR.

   IF AVAIL DumpFile THEN DO:
      FIND FIRST DFTimeTable EXCLUSIVE-LOCK WHERE
                 DFTimeTable.Brand       = Syst.Parameters:gcBrand AND 
                 DFTimeTable.DumpId      = DumpFile.DumpId         AND 
                 DFTimeTable.DumpTrigger = NO                      NO-ERROR.

      IF AVAIL DFTimeTable THEN
         ASSIGN DFTimeTable.DumpTime    = STRING(TIME + 1800, "HH:MM")
                DFTimeTable.DumpTrigger = YES.
   END.     

END.
