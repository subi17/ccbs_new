/* fpdfrun.i        30.04.03/aam 
   create pdf-file from xml using fopd
   send pdf-files via email
   send errorlog via email
*/

&IF "{&CParamFunctionDefined}" NE "YES"
&THEN
{cparam2.i}
&ENDIF

{lib/starfop.i}
{email.i}

DEF VAR lcFile       AS CHAR  NO-UNDO. 
DEF VAR lcXLTDir     AS CHAR  NO-UNDO. 
DEF VAR lcFopMethod  AS CHAR  NO-UNDO. 
DEF VAR lcLogDir     AS CHAR  NO-UNDO.
DEF VAR lcErrFile    AS CHAR  NO-UNDO. 
DEF VAR liErrQty     AS INT   NO-UNDO. 
DEF VAR lcConfDir    AS CHAR  NO-UNDO.

DEF STREAM sPDFLog.
DEF STREAM sMsg.


FUNCTION fSendPDFMail RETURNS LOGICAL 
   (icFile    AS CHAR,
    icAddress AS CHAR,
    iiLang    AS INT,
    icBodyTxt AS CHAR,
    icSubject AS CHAR,
    icPrintType AS CHAR).

   DEF VAR lcMsgFile  AS CHAR  NO-UNDO. 
   DEF VAR lcMsgTxt   AS CHAR  NO-UNDO. 
   DEF VAR llOk       AS LOG   NO-UNDO. 

   IF icAddress = "" THEN RETURN FALSE.

   /* file for body text (one per day and language)  */
   lcMsgFile = "/tmp/pdf_send" +
               icPrintType + 
               "_" + 
               STRING(TODAY,"999999") + 
               "_" +
               STRING(TIME) +
               ".txt".
   
   OUTPUT STREAM sMsg TO VALUE(lcMsgFile).
      
   IF icBodyTxt = "" THEN icBodyTxt = "PDF".
      
   PUT STREAM sMsg UNFORMATTED
      icBodyTxt
      SKIP.
   OUTPUT STREAM sMsg CLOSE.

           /* mail recipients */
   ASSIGN  xMailAddr = icAddress
           /* subject */
           xMailSubj = REPLACE(icSubject," ","_"). 
   
   llOk = SendMail(lcMsgFile,
                   icFile).

   OS-DELETE VALUE(lcMsgFile) NO-ERROR.
   
   RETURN llOk.
   
END FUNCTION.

FUNCTION fErrorLog RETURNS LOGICAL 
   (icMsg AS CHAR).

   PUT STREAM sPDFLog UNFORMATTED
      STRING(TODAY,"99.99.9999") 
      " "
      STRING(TIME,"hh:mm:ss").
      
   IF AVAILABLE Customer 
   THEN PUT STREAM sPDFLog UNFORMATTED
      " Customer " Customer.CustNum.
       
   IF AVAILABLE Invoice 
   THEN PUT STREAM sPDFLog UNFORMATTED
      ", Invoice " Invoice.InvNum.
      
   PUT STREAM sPDFLog UNFORMATTED   
      ": "
      icMsg
      SKIP.

   liErrQty = liErrQty + 1.
      
END FUNCTION.

FUNCTION fPDFInit RETURNS LOGICAL
   (icPrintType AS CHAR,
    ilFormPDF   AS LOG).
   
   /* wait for fop to finish printing or leave the job running as batch */
   lcFopMethod = CAPS(fCParamC("Fop" + icPrintType + "Print")).
   IF lcFopMethod = ? THEN lcFopMethod = CAPS(fCParamC("FopPrint")).
   
   ASSIGN lcLogDir    = fCParamC("PDFLogDir")
          xMailFrom   = fCParamC("PDFMailSender")
          lcConfDir   = fCParamC("RepConfDir")
          liErrQty    = 0. 

   /* valid values are "wait" and "done" */
   IF LOOKUP(lcFopMethod,"wait,done") = 0 OR
      lcFopMethod = ?
   THEN lcFopMethod = "WAIT".

   /* log file for errors */
   lcErrFile = lcLogDir + 
               (IF SUBSTRING(lcLogDir,LENGTH(lcLogDir),1) NE "/"
                THEN "/"
                ELSE "") +
               "pdf" + icPrintType + "_" +
               STRING(YEAR(TODAY),"9999") + 
               STRING(MONTH(TODAY),"99")  + 
               STRING(DAY(TODAY),"99")    +
               ".log".
            
   OUTPUT STREAM sPDFLog TO VALUE(lcErrFile) APPEND.

   /* layout directory */
   lcXLTDir = OS-GETENV("XLTDIR").
   IF lcXLTDir = ? AND ilFormPDF THEN DO:
      fErrorLog("XLT directory has not been set.").
   
      /* nothing can be done */
      RETURN FALSE.
   END. 
   ELSE IF SUBSTRING(lcXLTDir,LENGTH(lcXLTDir),1) NE "/"
        THEN lcXLTDir = lcXLTDir + "/". 

   RETURN TRUE. 
   
END FUNCTION.

PROCEDURE pRunFOPD:
 
   DEF INPUT  PARAMETER icXLTFile AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icXMLFile AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER olOK      AS LOG  NO-UNDO. 

   /* send xml-file to fopd along with layout file (xlt) */
   RUN QuickLauncher (icXLTFile,      /* layout */
                      "PDF",          /* format */
                      icXMLFile,      /* xml-file */
                      lcFopMethod).   /* wait that print is complete */

   /* was fopd succesful */
   IF NOT RETURN-VALUE BEGINS "DONE" THEN DO:
      fErrorLog("Fopd returned " + RETURN-VALUE).
      olOK = FALSE.
   END. 
   
   ELSE olOK = TRUE. 

END.


FUNCTION fPDFFinish RETURNS LOGICAL
   (icPrintType AS CHAR,
    icMessage   AS CHAR).

   OUTPUT STREAM sPDFLog CLOSE.

   /* send errorlog via email */
   IF liErrQty > 0 THEN DO:
      ASSIGN xMailAttach = lcErrFile   
             lcFile      = "/tmp/pdferr_" + icPrintType + "_msg.txt".
       
      /* header message */      
      OUTPUT STREAM sPDFLog TO VALUE(lcFile).
      PUT STREAM sPDFLog UNFORMATTED
         "Errors in creating / sending pdf from " + icMessage + " " + 
         STRING(TODAY,"99.99.9999") + "."
         SKIP.
      OUTPUT STREAM sPDFLog CLOSE.

      /* mail recipients */
      GetRecipients(lcConfDir + "pdf_" + icPrintType + "_error.email").

      IF xMailAddr > "" THEN DO:
         /* actual sending */  
         SendMail(lcFile,xMailAttach).
      END.

      RETURN TRUE. 
   END.
      
END FUNCTION. 

