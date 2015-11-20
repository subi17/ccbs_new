/* ----------------------------------------------------------------------
  MODULE .......: bob_pdfinvoice_xml.p
  TASK .........: BOB TOOL to generate PDF Invoice from a given list
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 08.07.15
  Version ......: Yoigo
----------------------------------------------------------------------- */

/* ***************************  Definitions  ************************** */
{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{cparam2.i}
{replog_reader.i}
{host.i}
{ftransdir.i}
{eventlog.i}

DEFINE VARIABLE lcLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO INITIAL ";".
DEFINE VARIABLE liNumOK  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNumErr AS INTEGER   NO-UNDO.

/* files and dirs */
DEFINE VARIABLE lcBOBLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInvDetails    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInvDate       AS CHARACTER NO-UNDO.

DEF VAR lcPDFFile AS CHAR NO-UNDO. 
DEF VAR lcToday   AS CHAR NO-UNDO. 
DEF VAR lcInvNum  AS CHAR NO-UNDO. 
DEF VAR liCount   AS INT  NO-UNDO. 
DEF VAR lcError   AS CHAR NO-UNDO. 
DEF VAR lcInvFile AS CHAR NO-UNDO.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
/* ***************************  Functions  *************************** */

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
   
   liNumErr = liNumErr + 1.
   
END FUNCTION.   

/* ***************************  Main Block  *************************** */

ASSIGN
   lcIncDir   = fCParam("PDFInvoice","IncDir")
   lcProcDir  = fCParam("PDFInvoice","IncProcDir")
   lcSpoolDir = fCParam("PDFInvoice","OutSpoolDir")
   lcOutDir   = fCParam("PDFInvoice","OutDir")
   lcPDFFile  = fCParamC("InvBOBPDFFile")
   lcToday    = STRING(YEAR(TODAY),"9999") + 
                STRING(MONTH(TODAY),"99")  +
                STRING(DAY(TODAY),"99").

IF TODAY EQ DATE(MONTH(TODAY),1,YEAR(TODAY)) OR 
   TODAY EQ DATE(MONTH(TODAY),2,YEAR(TODAY)) THEN DO:
   
   ASSIGN lcBOBLogFile = "PDF_INVOICE_" + lcToday +
                         "_" + STRING(TIME) +
                         "_" + ".log"
          lcOutDir     = lcOutDir + "/" + lcBOBLogFile.
    
   OUTPUT STREAM sLog TO VALUE(lcOutDir) APPEND.
   
   PUT STREAM sLog UNFORMATTED
      "Can't process incoming files on : " + STRING(TODAY).

   OUTPUT STREAM sLog CLOSE.

   LEAVE.   
END.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
  
  lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcInputFile) EQ FALSE THEN NEXT.
      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.
 
   ASSIGN 
      lcInvFile    = "PDF_INVOICE_" + lcToday              
      lcBOBLogFile = "PDF_INVOICE_" + lcToday + 
                     "_" + STRING(TIME) + 
                     "_" + ".log"
      lcBOBLogFile = lcSpoolDir + lcBOBLogFile.

   OUTPUT STREAM sLog TO VALUE(lcBOBLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
      
   ASSIGN
      liNumOk         = 0
      liNumErr        = 0
      lcLine          = ""
      lcReportFileOut = ""
      lcProcessedFile = ""
      lcInvDetails    = ""
      lcInvDate       = "".

   IF NOT lcFileName BEGINS lcInvFile THEN DO:
      fError("Incorrect input filename format"). 
      RUN pTransOnError.
      NEXT.
   END.
          
   fBatchLog("START", lcInputFile).    
   
   LINE_LOOP:
   REPEAT:
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.

      ASSIGN lcInvNum = TRIM(ENTRY(1,lcLine,lcSep)) NO-ERROR.
        
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.
      
      FIND FIRST Invoice NO-LOCK WHERE 
                 Invoice.Brand    = gcBrand  AND 
                 Invoice.ExtInvID = lcInvNum NO-ERROR.  
      IF NOT AVAIL Invoice THEN DO:
         fError("Invalid Invoice Number").
         NEXT.
      END.

      lcInvDate = STRING(YEAR(Invoice.InvDate),"9999") +
                  STRING(MONTH(Invoice.InvDate),"99")  +
                  STRING(DAY(Invoice.InvDate),"99").

      IF lcInvDetails EQ "" THEN 
         lcInvDetails = "~"" + lcInvDate + "|" + STRING(Invoice.InvNum) + "|" + "CUSTOMER_ID-" + Invoice.ExtInvID + "~"".
      ELSE 
         lcInvDetails = lcInvDetails + "," + "~"" + lcInvDate + "|" + STRING(Invoice.InvNum) + "|" + "CUSTOMER_ID-" + Invoice.ExtInvID + "~"".
   
      liNumOK = liNumOK + 1. 
 
   END.
  
   RUN pTransOnError.

   RUN pSendActiveMQMessage(lcInvDetails).
 
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
END.

PROCEDURE pTransOnError:
    
   PUT STREAM sLog UNFORMATTED 
      "input: " STRING(liNumOK + liNumErr) ", "
      "updated: " STRING(liNumOK)          ", "
      "errors: " STRING(liNumErr) SKIP.
   
   lcReportFileOut = fMove2TransDir(lcBOBLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).
   
END PROCEDURE. 

PROCEDURE pSendActiveMQMessage:
DEFINE INPUT PARAMETER lcInvoiceDetails AS CHAR NO-UNDO. 

DEF VAR lcPDFOutput  AS CHAR NO-UNDO INITIAL "". 
DEF VAR llgRecursive AS CHAR NO-UNDO INITIAL "".
DEF VAR llgMultiFile AS CHAR NO-UNDO INITIAL "".
DEF VAR lcFeedBackID AS CHAR NO-UNDO INITIAL "".
DEF VAR lcType       AS CHAR NO-UNDO INITIAL "".
DEF VAR lcProcess    AS CHAR NO-UNDO INITIAL "". 
DEF VAR llgHandled   AS LOG  NO-UNDO.

   ASSIGN 
     lcPDFOutput  = lcPDFFile 
     llgRecursive = "false"
     llgMultiFile = "false"
     lcFeedbackID = ""  /* tar will be created with input folder name */
     lcType       = "invoice"
     lcProcess    = "bobtool".

   RUN pInitialize(INPUT "revolver").

   IF RETURN-VALUE > "" THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
         RETURN RETURN-VALUE.
   END.

   /* Call ActiveMQ Publisher class */
   lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                       liTimeOut,"revolver",
                                       lcUserName,lcPassword).

   IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
   END.

   lcMessage = "~{" + "~"invoices~""         + "~:" + "~[" + lcInvoiceDetails + "~]" + "," +
                      "~"output_file_name~"" + "~:" + "~"" + lcPDFOutput      + "~"" + "," +
                      "~"recursive~""        + "~:" +        llgRecursive            + "," +
                      "~"multi_file~""       + "~:" +        llgMultiFile            + "," +
                      "~"feedback_id~""      + "~:" + "~"" + lcFeedbackID     + "~"" + "," +
                      "~"type~""             + "~:" + "~"" + lcType           + "~"" + "," +
                      "~"process~""          + "~:" + "~"" + lcProcess        + "~"" + "~}".

   IF lMsgPublisher:send_message(lcMessage) THEN
      llgHandled = TRUE.
   ELSE DO:
      llgHandled = FALSE.
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
   END.

   RUN pFinalize(INPUT "").

END PROCEDURE.
