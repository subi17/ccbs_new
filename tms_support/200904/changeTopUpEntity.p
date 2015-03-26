DEFINE INPUT PARAMETER pcInputFile   AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER plSimulated   AS LOGICAL   NO-UNDO. 

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ctrimedLine AS CHAR NO-UNDO.

DEFINE STREAM sData.
DEFINE STREAM sLog.

DEFINE VARIABLE lcEntityValue AS CHAR NO-UNDO.
DEFINE VARIABLE lcEntityName AS CHAR NO-UNDO. 


OUTPUT STREAM sLog TO VALUE(pcLogFile).
INPUT STREAM sData FROM VALUE(pcInputFile).

/* First remove all the previous entities saved in TMSCodes table */
DEFINE VARIABLE llReady AS LOGICAL NO-UNDO INIT TRUE.
FOR EACH TMSCodes WHERE TMSCodes.TableName = "PrepaidRequest"
                  AND TMSCodes.FieldName = "Entidad" 
                  AND TMSCodes.CodeGroup = "Prepaid" EXCLUSIVE-LOCK:

         PUT STREAM sLog UNFORMATTED "Entity " TMSCodes.CodeName " to be deleted " SKIP.
       IF NOT plSimulated THEN DO:
          IF AVAIL TMSCodes THEN DO:
                   PUT STREAM sLog UNFORMATTED "Entity " TMSCodes.CodeName " deleted " SKIP.
                   DELETE TMSCodes.
          END.
          ELSE DO:
               PUT STREAM sLog UNFORMATTED "Entity " TMSCodes.CodeName 
                                               "could not be locked !! " SKIP.
               llReady = FALSE.
          END.
       END.
END.

IF NOT llReady THEN RETURN. 

/* If eveything is Ok then lets go to write new records */
REPEAT:
   IMPORT STREAM sData UNFORMATTED cLine.

   /* trim the line */
   ctrimedLine = TRIM(cLine).

   /* extract the Entity Value and Entity Name */
   lcEntityValue = SUBSTRING(ctrimedLine,1,6).
   lcEntityName = SUBSTRING(ctrimedLine,7).

   IF CAN-FIND(FIRST TMSCodes  WHERE TMSCodes.TableName = "PrepaidRequest"
                                 AND TMSCodes.FieldName = "Entidad"
                                 AND TMSCodes.CodeGroup = "Prepaid"
                                 AND TMSCodes.CodeName = lcEntityName
                                 AND TMSCodes.CodeValue = lcEntityValue  NO-LOCK ) THEN 
       PUT STREAM sLog UNFORMATTED "Entity " lcEntityName " with value " lcEntityValue
                                  " alredy exist".
   ELSE DO: 

         PUT STREAM sLog UNFORMATTED 
            "Entity " lcEntityName 
            " with value " lcEntityValue 
            " created " SKIP.
      
         IF NOT plSimulated THEN
         DO:
            CREATE TMSCodes.
            ASSIGN TMSCodes.TableName = "PrepaidRequest"
                   TMSCodes.FieldName = "Entidad"
                   TMSCodes.CodeGroup = "Prepaid"
                   TMSCodes.CodeValue = lcEntityValue
                   TMSCodes.CodeName = lcEntityName. 
         END.

             
   END.
  
END.

INPUT STREAM sData CLOSE.
OUTPUT STREAM sLog CLOSE.

