
DEFINE VARIABLE plSimulated AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cICC AS CHARACTER NO-UNDO. 

DEFINE STREAM sSIM.
DEFINE STREAM sLog.

OUTPUT STREAM sLog TO "yot_454.log".
INPUT STREAM sSIM FROM "yot_454.input".

plSimulated = FALSE.

REPEAT:
   IMPORT STREAM sSIM UNFORMATTED cICC.
   FIND SIM WHERE SIM.ICC = cICC NO-LOCK NO-ERROR.
   IF AVAIL SIM THEN
   DO:
         IF NOT plSimulated THEN DO:
            FIND CURRENT SIM EXCLUSIVE-LOCK NO-ERROR.
            /* If the ICC is already in any Stock move it to the VIP stock */
            IF SIM.Stock NE "" THEN DO:
                ASSIGN  SIM.Stock = "VIP".
                PUT STREAM sLog UNFORMATTED cICC " : moved to VIP Stock" SKIP.
            END.
            ELSE DO:
            /* add it to the VIP stock with status available (1) */ 
             ASSIGN SIM.Stock = "VIP"
                    SIM.SimStat = 1.  
             PUT STREAM sLog UNFORMATTED cICC " : assigned to VIP Stock with Status 1" SKIP.
            END.
         END.
         ELSE DO:
             IF SIM.Stock NE "" THEN 
                PUT STREAM sLog UNFORMATTED cICC " : moved to VIP Stock" SKIP.
             ELSE 
                PUT STREAM sLog UNFORMATTED cICC " : assigned to VIP Stock with Status 1" SKIP.
         END.
   END.
   ELSE
      PUT STREAM sLog UNFORMATTED cICC ": Error does not exist SIM" SKIP.
END.

/* change status to available*/
DEFINE VARIABLE lcRedList AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
lcRedList = "8934040307000766397,8934070000000050366,8934070000000053972".
REPEAT iCount = 1 TO NUM-ENTRIES(lcRedList):
   cICC = ENTRY(iCount,lcRedList).
   FIND SIM WHERE SIM.ICC = cICC NO-LOCK NO-ERROR.
   IF AVAIL SIM THEN DO:
    IF NOT plSimulated THEN DO:
       FIND CURRENT SIM EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN SIM.SimStat = 1.
    END.
    PUT STREAM sLog UNFORMATTED cICC " : changed to status available" SKIP.
   END.
   ELSE 
   PUT STREAM sLog UNFORMATTED cICC ": Error does not exist SIM" SKIP.
END.


/* change one specific case */
FIND SIM WHERE SIM.ICC = "8934070000000049533" NO-LOCK NO-ERROR.
IF AVAIL SIM THEN DO:
   IF NOT plSimulated THEN DO:
       FIND CURRENT SIM EXCLUSIVE-LOCK NO-ERROR.
       ASSIGN SIM.SimStat = 1
              SIM.Stock = "RETAILER".
    END.
    PUT STREAM sLog UNFORMATTED "8934070000000049533: changed to  RETAILER Stock and status available" SKIP.
END.
ELSE 
PUT STREAM sLog UNFORMATTED "8934070000000049533: Error does not exist SIM" SKIP.

INPUT STREAM sSIM CLOSE.
OUTPUT STREAM sLog CLOSE.

