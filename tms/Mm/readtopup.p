{commali.i}
{timestamp.i}
{cparam2.i}
{ftopup.i}
{ftaxdata.i}
{ftransdir.i}

DEF INPUT  PARAMETER icFile   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead   AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiErrors AS INT  NO-UNDO.

DEF VAR ldAmount    AS DEC  NO-UNDO.
DEF VAR ldVatAmt    AS DEC  NO-UNDO. 
DEF VAR lcPrefix    AS CHAR NO-UNDO.
DEF VAR lcCLI       AS CHAR NO-UNDO.    
DEF VAR lcICC       AS CHAR NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO.
DEF VAR liRequest   AS INT  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR lcReadLine  AS CHAR NO-UNDO. 
DEF VAR lcTaxZone   AS CHAR NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO.

DEF STREAM sRead.
DEF STREAM sLog.

FORM 
   oiRead   AT 2 FORMAT ">>>>>>>>9" LABEL "Rows Read" SKIP
   oiErrors AT 2 FORMAT ">>>>>>>>9" LABEL "Errors .." SKIP
   WITH OVERLAY CENTERED ROW 10 SIDE-LABELS TITLE " IMPORT " FRAME fQty.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcReadLine  ";"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
   oiErrors = oiErrors + 1.
END FUNCTION.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

ASSIGN
   lcLogFile  = fCParamC("ReadTopupLog")
   lcTransDir = fCParamC("ReadTopupArc").

IF lcLogFile = ? OR lcLogFile = "" THEN 
   lcLogFile = "/tmp/camp_topup_#DATE.log".
   
lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(TODAY,"999999")).

/* file without the dir */
lcPlainFile = icFile.
IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
   lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
 

INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   icFile  " "
   STRING(TODAY,"99.99.99") " "
   STRING(TIME,"hh:mm:ss") SKIP.

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.


REPEAT:

   lcReadLine = "".
    
   IMPORT STREAM sRead UNFORMATTED lcReadLine.
   
   oiRead = oiRead + 1.
   
   IF NOT SESSION:BATCH AND 
      (oiRead < 100 OR oiRead MOD 100 = 0) THEN DO:
      PAUSE 0.
      DISP oiRead oiErrors WITH FRAME fQty.
   END.
   
   IF NUM-ENTRIES(lcReadLine,";") < 4 THEN DO:
      fError("Invalid format").
      NEXT.
   END.

   ASSIGN 
      lcCLI    = ENTRY(1,lcReadLine,";")
      lcICC    = ENTRY(2,lcReadLine,";")
      lcPrefix = ENTRY(3,lcReadLine,";")
      ldAmount = DECIMAL(ENTRY(4,lcReadLine,";"))
      ldVatAmt = 0
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Invalid format").
      NEXT.
   END.

   FIND FIRST MobSub WHERE MobSub.CLI = lcCLI NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN DO:
      fError("Subscription not available").
      NEXT.
   END.
   
   IF MobSub.ICC NE lcICC THEN DO:
      fError("ICC is not correct").
      NEXT.
   END.
   
   IF MobSub.PayType = FALSE THEN DO:
      fError("Subscription is not prepaid").
      NEXT.
   END.

   IF ldAmount = 0 THEN DO:
      fError("Amount is zero").
      NEXT.
   END.

   IF NOT CAN-FIND(FIRST TMSCodes WHERE
                         TMSCodes.TableName = "PrepaidRequest" AND
                         TMSCodes.FieldName = "PPReqPrefix" AND
                         TMSCodes.CodeValue = lcPrefix)
   THEN DO:
      fError("Invalid prefix").
      NEXT.
   END.
         
   /* taxcode */
   FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK.
   lcTaxZone = fRegionTaxZone(Customer.Region).
    
   liRequest = fCreateTopupRequest(MobSub.MsSeq,
                                   MobSub.CLI,
                                   "RefillTRequest",   
                                   "WEB Order",  /* source */
                                   "RefillTRequest",
                                   lcPrefix,
                                   "Import: " + lcPlainFile,  /* reference */
                                   lcTaxZone,
                                   0,
                                   ldAmount * 100,
                                   ldVatAmt * 100).

   IF liRequest = 0 THEN 
      fError("Request creation failed").
     
   ELSE
      fLogLine("OK " + STRING(liRequest)). 
         
END.

HIDE FRAME fQty NO-PAUSE.

/* move to archive */
IF oiRead > 0 AND lcTransDir > "" THEN DO:   
   fTransDir(icFile,
             "",
             lcTransDir).
END.
 
INPUT STREAM sRead CLOSE.
OUTPUT STREAM sLog CLOSE.


 
