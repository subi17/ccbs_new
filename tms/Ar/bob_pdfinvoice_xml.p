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
{ftransdir.i}
{cparam2.i}
{eventlog.i}

DEFINE VARIABLE lcLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO INITIAL ";".
DEFINE VARIABLE liNumOK  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNumErr AS INTEGER   NO-UNDO.

/* files and dirs */
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO.

DEF VAR lcXMLFile AS CHAR NO-UNDO. 
DEF VAR lcXMLFder AS CHAR NO-UNDO. 
DEF VAR lcFile    AS CHAR NO-UNDO. 
DEF VAR lcToday   AS CHAR NO-UNDO. 
DEF VAR lcBillRun AS CHAR NO-UNDO. 
DEF VAR liInvNum  AS INT  NO-UNDO. 
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
   lcFile     = fCParamC("InvXMLFile")
   lcXMLFile  = fCParamC("InvBOBXMLFile")
   lcToday    = STRING(YEAR(TODAY),"9999") + 
                STRING(MONTH(TODAY),"99")  +
                STRING(DAY(TODAY),"99")
   lcXMLFder  = lcToday + STRING(TIME,"99999"). 

IF TODAY EQ DATE(MONTH(TODAY),1,YEAR(TODAY)) OR 
   TODAY EQ DATE(MONTH(TODAY),2,YEAR(TODAY)) THEN DO:
   
   ASSIGN lcLogFile = "PDF_INVOICE_" + lcToday +
                      "_" + STRING(TIME) +
                      "_" + ".log"
          lcOutDir  = lcOutDir + "/" + lcLogFile.
    
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
      lcInvFile = "PDF_INVOICE_" + lcToday              
      lcBillRun = "PDF-INVOICE-" + lcToday + 
                   STRING(TIME,"99999")
      lcLogFile = "PDF_INVOICE_" + lcToday + 
                  "_" + STRING(TIME) + 
                  "_" + ".log"
      lcLogFile = lcSpoolDir + lcLogFile
      lcXMLFile = lcXMLFile + "/" + lcXMLFder + "*" + lcFile.

   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP
              "BILLING RUN ID: " lcBillRun SKIP.
      
   IF lcXMLFile = "" OR lcXMLFile = ? THEN DO:
      fError("ERROR:Print file not defined").
      LEAVE.
   END.

   ASSIGN
      liNumOk         = 0
      liNumErr        = 0
      lcLine          = ""
      lcReportFileOut = ""
      lcProcessedFile = "".

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

      ASSIGN liInvNum = INTEGER(ENTRY(1,lcLine,lcSep)) NO-ERROR.
        
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.
      
      FIND FIRST Invoice NO-LOCK WHERE 
                 Invoice.Brand   = gcBrand  AND 
                 Invoice.InvNum  = liInvNum AND 
                 Invoice.InvType = 99       NO-ERROR.  
      IF NOT AVAIL Invoice THEN DO:
         fError("Invalid Invoice Number").
         NEXT.
      END.

      RUN printdoc1co ("",
                       Invoice.CustNum,
                       Invoice.CustNum,
                       Invoice.ExtInvID,
                       Invoice.ExtInvID,
                       Invoice.InvDate,
                       FALSE,     /* only unprinted */
                       TRUE,      /* print credited */
                       Invoice.InvType,
                       Invoice.DelType,
                       "XMLSEP",  
                       lcXMLFile,
                       FALSE,
                       OUTPUT liCount,
                       OUTPUT lcError).
      
      IF lcError NE "" THEN DO:
         fError("Error creating Invoice XML").
         NEXT.
      END.
      ELSE  
         liNumOK = liNumOK + 1. 
 
   END.
  
   RUN pTransOnError.
 
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
END.

PROCEDURE pTransOnError:
    
   PUT STREAM sLog UNFORMATTED 
      "input: " STRING(liNumOK + liNumErr) ", "
      "updated: " STRING(liNumOK)          ", "
      "errors: " STRING(liNumErr) SKIP.
   
   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).
   
END PROCEDURE. 

