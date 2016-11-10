{triggers/hpddelete_generic.i CliType CLITYPE Ordercanal CliType}

{tmsconst.i}

IF NEW CliType
THEN RETURN.

FIND FIRST DumpFile NO-LOCK WHERE 
           DumpFile.Brand    EQ Syst.Parameters:gcBrand AND
           DumpFile.DumpName EQ {&DUMP_CLITYPE_TRACK}   NO-ERROR.

IF AVAIL DumpFile THEN DO:
   FIND FIRST DFTimeTable EXCLUSIVE-LOCK WHERE 
              DFTimeTable.Brand       = Syst.Parameters:gcBrand AND  
              DFTimeTable.DumpId      = DumpFile.DumpId         AND 
              DFTimeTable.DumpTrigger = NO                      NO-ERROR.

   IF AVAIL DFTimeTable THEN
      ASSIGN DFTimeTable.DumpTime    = STRING(TIME + 1800, "HH:MM")
             DFTimeTable.DumpTrigger = YES.
END.

