{triggers/hpdwrite_generic.i CliType CLITYPE Ordercanal CliType}

{tmsconst.i}

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

IF NOT NEW(CliType) THEN
   BUFFER-COMPARE CliType TO oldCliType SAVE RESULT IN llSameValues.

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

