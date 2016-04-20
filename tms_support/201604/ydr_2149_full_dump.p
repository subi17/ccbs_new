{commpaa.i}
katun = "Cron".
gcBrand = "1".

{host.i}

DEF VAR lcDumpFile AS CHAR NO-UNDO. 
DEF VAR lcDate     AS CHAR NO-UNDO. 
DEF VAR liDumped   AS INT NO-UNDO. 

def STREAM strout. 

ASSIGN 
   lcDumpFile = "DWH_Full_AREC_rejections_YYYYMMDD.txt"
   lcDate     = STRING(YEAR(TODAY),"9999") +
                STRING(MONTH(TODAY),"99")  +
                STRING(DAY(TODAY),"99")
   lcDumpFile = REPLACE(lcDumpFile,"YYYYMMDD",lcDate).              

OUTPUT STREAM strout TO VALUE(lcDumpFile).

FIND FIRST DumpFile NO-LOCK WHERE 
           DumpFile.Brand    = gcBrand AND 
           DumpFile.DumpName = "ARECDWHDump" NO-ERROR.

IF AVAIL DumpFile THEN DO:
   
   RUN dumpfile_run(DumpFile.DumpID,  /* Dump ID */
                    "Full",
                    "",
                    fIsThisReplica(),
                    OUTPUT liDumped). 
   
   IF liDumped >= 0 THEN
      PUT STREAM strout UNFORMATTED
         "AREC rejections full dump event count : " + STRING(liDumped) SKIP.
   ELSE
      PUT STREAM strout UNFORMATTED
         "Error in creating AREC rejections full dump event !" SKIP.  

END.

OUTPUT STREAM strout CLOSE.
