DEFINE INPUT PARAMETER pcInputFile    AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piTargetStatus AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER plSimulated    AS LOGICAL   NO-UNDO. 

DEFINE VARIABLE cCLI AS CHARACTER NO-UNDO. 

DEFINE STREAM sMSISDN.
DEFINE STREAM sLog.

OUTPUT STREAM sLog TO VALUE(pcLogFile).
INPUT STREAM sMSISDN FROM VALUE(pcInputFile).

{commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{msisdn.i}

REPEAT:
   IMPORT STREAM sMSISDN UNFORMATTED cCLI.
   FIND FIRST MSISDN WHERE
        MSISDN.CLI = cCLI NO-LOCK NO-ERROR.
   IF AVAIL MSISDN THEN
   DO:
         PUT STREAM sLog UNFORMATTED 
            "MSISDN with CLI " cCLI 
            " in stock " MSISDN.POS
            " moved from status " MSISDN.StatusCode 
            " to status " piTargetStatus SKIP.
         IF NOT plSimulated THEN
         DO:
           fMakeMsidnHistory(recid(msisdn)).
           assign msisdn.statuscode = piTargetStatus.
         END.
   END.
   ELSE
      PUT STREAM sLog UNFORMATTED "Error! MSISDN with CLI " 
         cCLI " does not exist" SKIP.
END.

INPUT STREAM sMSISDN CLOSE.
OUTPUT STREAM sLog CLOSE.

