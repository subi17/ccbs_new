TRIGGER PROCEDURE FOR REPLICATION-WRITE OF CLIType OLD BUFFER oldCLIType.

{Syst/tmsconst.i}

{HPD/HPDConst.i}

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

&IF {&CLITYPE_WRITE_TRIGGER_ACTIVE} &THEN

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.RowID     = STRING(ROWID(CLIType))
   Ordercanal.RepLog.TableName = "CLIType"
   Ordercanal.RepLog.EventType = (IF NEW(CLIType)
                           THEN "CREATE"
                           ELSE "MODIFY")
   Ordercanal.RepLog.EventTime = NOW
   .

IF NOT NEW(CLIType)
THEN DO: 
   BUFFER-COMPARE CLIType USING
      CLIType
   TO oldCLIType SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
   CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.TableName = "CLIType"
         Ordercanal.RepLog.EventType = "DELETE"
         Ordercanal.RepLog.EventTime = NOW
         Ordercanal.RepLog.KeyValue  = SUBSTITUTE("&1",oldCLIType.CLIType)
         .
   END.

END.

&ENDIF

IF NOT NEW(CLIType) 
THEN BUFFER-COMPARE CLIType TO oldCLIType SAVE RESULT IN llSameValues.

IF NOT llSameValues OR NEW(CLIType) THEN
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
