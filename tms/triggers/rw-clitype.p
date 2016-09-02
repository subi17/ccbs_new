TRIGGER PROCEDURE FOR REPLICATION-WRITE OF CliType OLD BUFFER oldCliType.

{commali.i}
{tmsconst.i}

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

BUFFER-COMPARE CliType TO oldCliType SAVE RESULT IN llSameValues.

IF NOT llSameValues OR NEW(CliType) THEN
DO:

   FIND FIRST DumpFile NO-LOCK WHERE
              DumpFile.Brand    EQ gcBrand AND
              DumpFile.DumpName EQ {&DUMP_CLITYPE_TRACK} NO-ERROR.

   IF AVAIL DumpFile THEN
      FIND FIRST DFTimeTable EXCLUSIVE-LOCK WHERE
                 DFTimeTable.DumpId = DumpFile.DumpId NO-WAIT NO-ERROR.

      IF AVAIL DFTimeTable THEN
         ASSIGN DFTimeTable.DumpTime = STRING(TIME + 7200, "HH:MM").
END.         



