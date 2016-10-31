{triggers/hpddelete_generic.i CliType CLITYPE Ordercanal CliType}

{commpaa.i}
gcBrand = "1".

{tmsconst.i}

IF NEW CliType
THEN RETURN.

DEFINE VAR iDumpTotMin  AS INT NO-UNDO.

FIND FIRST DumpFile NO-LOCK WHERE 
           DumpFile.Brand    EQ gcBrand AND
           DumpFile.DumpName EQ {&DUMP_CLITYPE_TRACK} NO-ERROR.

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

