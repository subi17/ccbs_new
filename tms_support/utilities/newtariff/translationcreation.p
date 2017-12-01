/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER icBaseFile AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttTrans NO-UNDO 
    FIELD tLangType  AS CHARACTER 
    FIELD tLangint   AS INTEGER 
    FIELD tLangTrans AS CHARACTER.

DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE giTextType  AS INTEGER  NO-UNDO.

DEFINE STREAM strin.
DEFINE STREAM BTLog.

/* ********************  Functions  ******************** */

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM BTLog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fRecordFound RETURNS LOGICAL
   (icKeyValue AS CHARACTER):

   CASE giTextType:
      WHEN 1
      THEN IF NOT CAN-FIND(FIRST BillItem NO-LOCK WHERE 
                                 BillItem.Brand    = Syst.Var:gcBrand  AND 
                                 BillItem.BillCode = icKeyValue)
           THEN RETURN FALSE.
      WHEN 9
      THEN IF NOT CAN-FIND(FIRST CLIType NO-LOCK WHERE 
                                 CLIType.Brand    = Syst.Var:gcBrand  AND 
                                 CLIType.CLIType = icKeyValue)
           THEN RETURN FALSE.
      WHEN 11
      THEN IF NOT CAN-FIND(FIRST RatePlan NO-LOCK WHERE 
                                 RatePlan.Brand    = Syst.Var:gcBrand  AND 
                                 RatePlan.RatePlan = icKeyValue)
           THEN RETURN FALSE.
      OTHERWISE RETURN FALSE.      
   END CASE.

   RETURN TRUE.

END FUNCTION.

PROCEDURE pCreTranslations:

   FOR EACH ttTrans NO-LOCK:
      
      IF NOT fRecordFound(ttTrans.tLangType)
      THEN DO:
         fError(SUBSTITUTE("While reading file '&1' cannot find the source record where key value is '&2'", icBaseFile, ttTrans.tLangType)).
         RETURN "ERROR".
      END.
      FIND FIRST RepText EXCLUSIVE-LOCK WHERE 
         RepText.Brand    = Syst.Var:gcBrand  AND
         RepText.TextType = giTextType        AND
         RepText.LinkCode = ttTrans.tLangType AND
         RepText.Language = ttTrans.tLangint  AND
         RepText.FromDate <= TODAY AND
         RepText.ToDate   >= TODAY
      NO-ERROR.
      
      IF AVAILABLE RepText
      THEN DO:
         fError(SUBSTITUTE("RepText having texttype=&1, linkcode=&2 and language=&3 " + 
                           "is already available and active",
                           giTextType, ttTrans.tLangType, ttTrans.tLangint)).
         RETURN "ERROR".
      END.
      
      CREATE RepText.
      ASSIGN
         RepText.Brand    = Syst.Var:gcBrand
         RepText.TextType = giTextType
         RepText.LinkCode = ttTrans.tLangType
         RepText.Language = ttTrans.tLangint
         RepText.FromDate = TODAY     
         RepText.ToDate   = 12/31/49       
         RepText.RepText  = ttTrans.tLangTrans
         NO-ERROR.
         
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Creating translation").
         RETURN "ERROR".
      END.
         
   END.
   
   RETURN "OK".
        
END PROCEDURE.

/* ***************************  Main Block  *************************** */

lcLogFile   = icSpoolDir + icBaseFile + ".log".

OUTPUT STREAM BTLog TO VALUE(lcLogFile) APPEND.

CASE ENTRY(1,icBaseFile, "."):

   WHEN "billitem_translation"
   THEN giTextType = 1.
   WHEN "rateplan_translation"   
   THEN giTextType = 11.
   WHEN "tariff_translation"   
   THEN giTextType = 9.
   OTHERWISE DO:
      fError(SUBSTITUTE("No implementation to handle '&1' type translation", ENTRY(1,icBaseFile, "."))).
      RETURN "ERROR".      
   END.

END CASE.


INPUT STREAM strin FROM VALUE(icFile).

DEFINE VARIABLE llFirst AS LOGICAL INITIAL TRUE NO-UNDO.
                           
REPEAT:

   IMPORT STREAM strin UNFORMATTED lcLine.
   
   /* Ignore the first line - (Header) */
   IF llFirst
   THEN DO:
      llFirst = FALSE.
      NEXT.
   END.
   
   IF TRIM(lcLine) eq "" THEN NEXT.
   
   FIND FIRST Language NO-LOCK WHERE
      Language.Langname BEGINS TRIM(ENTRY(2,lcLine,";"))
   NO-ERROR.
   
   IF NOT AVAILABLE Language   
   THEN DO:
      fError(SUBSTITUTE("Invalid language name &1", TRIM(ENTRY(2,lcLine,";")))).
      RETURN "ERROR".
   END. 

   CREATE ttTrans.
   ASSIGN 
      ttTrans.tLangType  = TRIM(ENTRY(1,lcLine,";")) 
      ttTrans.tLangint   = Language.Language
      ttTrans.tLangTrans = TRIM(TRIM(ENTRY(3,lcLine,";")),'"') NO-ERROR.
   
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
   OUTPUT STREAM BTLog CLOSE.
END FINALLY.
/* ***************************  Main End  *************************** */