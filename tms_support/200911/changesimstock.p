DEFINE INPUT PARAMETER pcInputFile   AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcTargetStock AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER plSimulated   AS LOGICAL   NO-UNDO. 

DEFINE VARIABLE cICC AS CHARACTER NO-UNDO. 

DEFINE STREAM sSIM.
DEFINE STREAM sLog.

OUTPUT STREAM sLog TO VALUE(pcLogFile).
INPUT STREAM sSIM FROM VALUE(pcInputFile).

REPEAT:
   IMPORT STREAM sSIM UNFORMATTED cICC.
   FIND SIM WHERE SIM.ICC = cICC NO-LOCK NO-ERROR.
   IF AVAIL SIM THEN
   DO:
         PUT STREAM sLog UNFORMATTED 
            "SIM with ICC " cICC 
            " moved from stock " SIM.Stock 
            " to stock " pcTargetStock SKIP.
         IF NOT plSimulated THEN
         DO:
            FIND CURRENT SIM EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL SIM THEN
            DO:
               SIM.Stock = pcTargetStock.
            END.
            ELSE 
               PUT STREAM sLog UNFORMATTED 
                  "Error! Could not lock SIM with ICC " cICC 
                  " to update it." SKIP.
         END.
   END.
   ELSE
      PUT STREAM sLog UNFORMATTED "Error! SIM with ICC " 
         cICC " does not exist" SKIP.
END.

INPUT STREAM sSIM CLOSE.
OUTPUT STREAM sLog CLOSE.

