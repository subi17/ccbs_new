{triggers/hpdwrite_generic.i CliType CLITYPE Ordercanal CliType}

{commali.i}
{tmsconst.i}

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.
DEFINE VARIABLE iDumpTotMin  AS INTEGER NO-UNDO.

IF NOT NEW(CliType) THEN
   BUFFER-COMPARE CliType TO oldCliType SAVE RESULT IN llSameValues.

IF NOT llSameValues OR NEW(CliType) THEN
DO:
   FIND FIRST DumpFile NO-LOCK WHERE
              DumpFile.Brand     EQ gcBrand               AND
              DumpFile.DumpName  EQ {&DUMP_CLITYPE_TRACK} AND
              DumpFile.MainTable EQ "CliType"             NO-ERROR.

   IF AVAIL DumpFile THEN DO:
      FIND FIRST DFTimeTable EXCLUSIVE-LOCK WHERE
                 DFTimeTable.Brand       = gcBrand         AND 
                 DFTimeTable.DumpId      = DumpFile.DumpId AND 
                 DFTimeTable.DumpTrigger = NO              NO-WAIT NO-ERROR.

      IF AVAIL DFTimeTable THEN
      DO:
          iDumpTotMin = INT(ENTRY(1,DFTimeTable.DumpTime,":")) * 60
                      + INT(ENTRY(2,DFTimeTable.DumpTime,":")).

          IF INT(TIME / 60) > iDumpTotMin THEN 
             ASSIGN DFTimeTable.DumpTime    = STRING(TIME + 7200, "HH:MM")
                    DFTimeTable.DumpTrigger = YES.
            
      END.
   END.     

END.         



