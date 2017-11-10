/*------------------------------------------------------------------------
  MODULE .......: tarifftrans.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......: 10.11.2017
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/  

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER icBaseFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER icFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttTrans NO-UNDO 
   FIELD tLangType  AS CHARACTER 
   FIELD tLangint   AS INTEGER 
   FIELD tLangtext  AS CHARACTER
   FIELD tLangTrans AS CHARACTER.
 
DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile   AS CHARACTER NO-UNDO.

DEFINE STREAM strin.
DEFINE STREAM TTLog.

/* ********************  Functions And Procedures ******************** */

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM TTLog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.


PROCEDURE pCreTranslations:

   FOR EACH ttTrans NO-LOCK:    
      IF CAN-FIND(FIRST ShaperConf WHERE 
                        ShaperConf.Brand        = Syst.Var:gcBrand  AND 
                        ShaperConf.ShaperConfID = ttTrans.tLangType)
      THEN DO:
         FIND FIRST RepText EXCLUSIVE-LOCK WHERE 
            RepText.Brand    = Syst.Var:gcBrand AND
            RepText.TextType = 11 AND
            RepText.LinkCode = ttTrans.tLangType AND
            RepText.Language = ttTrans.tLangint
         NO-ERROR.
         
         IF NOT AVAILABLE RepText THEN DO:
            CREATE RepText.
            ASSIGN
               RepText.Brand    = Syst.Var:gcBrand
               RepText.TextType = 11
               RepText.LinkCode = ttTrans.tLangType
               RepText.Language = ttTrans.tLangint.               
         END. 

         ASSIGN 
            RepText.FromDate = TODAY     
            RepText.ToDate   = 12/31/49       
            RepText.RepText  = ttTrans.tLangTrans.
            
         IF ERROR-STATUS:ERROR THEN DO:
            fError("Creating translations for Tariffs").
            RETURN "ERROR".
         END.
            
      END.
      ELSE DO:
         fError("ShaperConf doesn't exists").
         RETURN "ERROR".
      END.
   END.
   
   RETURN "OK".
        
END PROCEDURE.

/* ***************************  Main Block  *************************** */

lcLogFile = icSpoolDir + icBaseFile + ".log".
OUTPUT STREAM TTlog TO VALUE(lcLogFile) APPEND.
     
INPUT STREAM strin FROM VALUE(icFile).

IMPORT STREAM strin UNFORMATTED lcLine. /* Reads first line (Header). Ignore it */                          
    
REPEAT: 
   IMPORT STREAM strin UNFORMATTED lcLine.    
                                                                   
   IF TRIM(lcLine) EQ "" OR NUM-ENTRIES(lcLine, ";") <> 4 THEN
      NEXT.

   CREATE ttTrans.
   ASSIGN 
      ttTrans.tLangType  = TRIM(ENTRY(1,lcLine,";")) 
      ttTrans.tLangint   = INTEGER(TRIM(ENTRY(2,lcLine,";")))
      ttTrans.tLangtext  = TRIM(ENTRY(3,lcLine,";")) 
      ttTrans.tLangTrans = TRIM(TRIM(ENTRY(4,lcLine,";")),'"') NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Incorrect input translation data").
      RETURN "ERROR". 
   END.
   
END.

RUN pCreTranslations.

IF RETURN-VALUE <> "OK" THEN 
   RETURN RETURN-VALUE.   


RETURN "OK".

FINALLY: 
   INPUT STREAM strin  CLOSE.
   OUTPUT STREAM TTLog CLOSE.
END FINALLY.

/* ************************* Main Block ends  ************************* */
 