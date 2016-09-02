TRIGGER PROCEDURE FOR REPLICATION-DELETE OF CliType.

{commali.i}
{tmsconst.i}

IF NEW CliType
THEN RETURN.

FIND FIRST DumpFile NO-LOCK WHERE 
           DumpFile.Brand    EQ gcBrand AND
           DumpFile.DumpName EQ {&DUMP_CLITYPE_TRACK} NO-ERROR.

IF AVAIL DumpFile THEN
   FIND FIRST DFTimeTable EXCLUSIVE-LOCK WHERE 
              DFTimeTable.DumpId = DumpFile.DumpId NO-WAIT NO-ERROR.

   IF AVAIL DFTimeTable THEN   
      ASSIGN DFTimeTable.DumpTime = STRING(TIME + 7200, "HH:MM").

