DEFINE VARIABLE lcDumpNames AS CHARACTER NO-UNDO.

lcDumpNames = "HPD_EDRHistory,HPD_MobCDR,HPD_PrepCDR,HPD_PrepEDR,HPD_Order," +
              "HPD_Invoice,HPD_MsRequest,HPD_Payment,HPD_PrepaidRequest," +
              "HPD_ServiceLCounter".
              
FUNCTION fModifyDumpFile RETURNS LOGICAL
   (icDumpName AS CHARACTER):

   FIND FIRST DumpFile NO-LOCK WHERE
      DumpFile.Brand = "1" AND
      DumpFile.DumpName = icDumpName
   NO-ERROR.
   
   IF NOT AVAILABLE DumpFile
   THEN DO:
      MESSAGE "Not found"
        VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.      

   FIND DumpHPD NO-LOCK WHERE DumpHPD.DumpID = DumpFile.DumpID NO-ERROR.
   
   IF NOT AVAILABLE DumpHPD
   THEN DO TRANSACTION:
      CREATE DumpHPD.
      ASSIGN
         DumpHPD.UnitsToDump = INTEGER(DumpFile.FullCollModule) 
         DumpHPD.StartTime   = DumpFile.ModCollModule.
   END.

   IF INDEX(DumpFile.FileName,"#TENANT") = 0
   THEN DO TRANSACTION:
      
      FIND CURRENT DumpFile EXCLUSIVE-LOCK.
      
      DumpFile.FileName = SUBSTRING(DumpFile.FileName,1,R-INDEX(DumpFile.FileName,"#TIME") + 4) +
                          "_#TENANT" +
                          SUBSTRING(DumpFile.FileName,R-INDEX(DumpFile.FileName,"#TIME") + 5). 
      
      RELEASE DumpFile.
      
   END. 

END FUNCTION.

DEFINE VARIABLE lii AS INTEGER NO-UNDO.

DO lii = 1 TO NUM-ENTRIES(lcDumpNames):
   fModifyDumpFile(ENTRY(lii,lcDumpNames)).
END.
