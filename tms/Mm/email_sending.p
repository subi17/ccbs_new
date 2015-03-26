/* ----------------------------------------------------------------------
  MODULE .......: email_sending.p  
  TASK .........: Handle email sending Requests
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 23.05.12
---------------------------------------------------------------------- */
&GLOBAL-DEFINE MailTitleSpaces Allow

{commali.i}
{tmsconst.i}
{date.i}
{timestamp.i}
{email.i}
{femailinvoice.i}
{fusioninvoice.i}
{ftransdir.i}

DEF INPUT PARAMETER iiMsRequest AS INT  NO-UNDO.

DEF STREAM sEmail.

DEF VAR lcEmailText AS CHAR NO-UNDO. 

/******** Main start *********/

FIND FIRST MsRequest WHERE
           MsRequest.MSRequest = iiMsRequest  AND
           MsRequest.ReqType   = {&REQTYPE_EMAIL_SENDING}
     NO-LOCK NO-ERROR.
IF NOT AVAIL MsRequest THEN
   RETURN "ERROR:Unknown MSRequest " + STRING(iiMsRequest).

IF MsRequest.ReqCparam1 = "" OR MsRequest.ReqCparam1 = ? THEN DO:
   fReqError("ERROR:Email Template is blank").
   RETURN.
END.

IF MsRequest.ReqCparam2 = "" OR MsRequest.ReqCparam2 = ? THEN DO:
   fReqError("ERROR:Email address is blank").
   RETURN.
END.

lcEmailText = fGetEmailText(INPUT "EMAIL",
                            INPUT MsRequest.ReqCparam1,
                            INPUT 1,
                            OUTPUT xMailSubj).
IF lcEmailText = "" THEN DO:
   fReqError("ERROR:Email text not available").
   RETURN.
END.
   
IF xMailSubj = "" OR xMailSubj = ? THEN DO:
   fReqError("ERROR:Email Subject not available").
   RETURN.
END.

IF MSRequest.ReqCparam1 EQ "FusionEmail" THEN RUN pSendFusionEMail.
ELSE RUN pSendEmail.

IF RETURN-VALUE BEGINS "ERROR:" THEN
   fReqError(RETURN-VALUE).

RETURN RETURN-VALUE.

/******** Main end *********/


PROCEDURE pSendEmail:

   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR lcEmailFile        AS CHAR NO-UNDO.
   DEF VAR lcTransDir         AS CHAR NO-UNDO.
   DEF VAR lcEmailActLink     AS CHAR NO-UNDO.

   ASSIGN xMailFrom   = fCparam("SelfService","EmailFromAddress")
          lcEmailFile = fCparam("SelfService","EmailFile")
          lcTransDir  = fCParam("SelfService","MailArcDir")
          xMailAddr   = MsRequest.ReqCparam2.

   IF xMailFrom = "" OR xMailFrom = ? THEN
      RETURN "ERROR:Email From Address not available".

   IF lcEmailFile = "" OR lcEmailFile = ? THEN
      RETURN "ERROR:Email File not available".

   lcEmailFile = lcEmailFile + "_" + STRING(TODAY,"999999") + "_" +
                 STRING(TIME) + ".rtf".

   lcEmailFile = fUniqueFileName(lcEmailFile,"").

   lcEmailText = REPLACE(lcEmailText,"#INFO",MsRequest.ReqCparam3).
   lcEmailText = REPLACE(lcEmailText,CHR(10),"<br />").

   OUTPUT STREAM sEmail TO VALUE(lcEmailFile).
   PUT STREAM sEmail UNFORMATTED lcEmailText SKIP.
   OUTPUT STREAM sEmail CLOSE.

   /* Send the email */
   IF LOOKUP(lcMailHost,{&HOSTNAME_STAGING}) > 0 THEN
      SendMaileInvoice(lcEmailText,"").
   ELSE DO:
      ASSIGN xMailSubj     = "'" + xMailSubj + "'"
             xMailFileType = "text/html".
      SendMail(lcEmailFile,"").
   END. /* ELSE DO: */

   /* wait for confirmation */
   fReqStatus(2,"").

   /* move the file to archive directory */
   IF lcTransDir > "" THEN 
      fTransDir(lcEmailFile,
                ".rtf",
                lcTransDir).

   RETURN "".
   
END PROCEDURE. /* PROCEDURE pSendEmail: */

PROCEDURE pSendFusionEmail:

   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR lcEmailFile        AS CHAR NO-UNDO.
   DEF VAR lcTransDir         AS CHAR NO-UNDO.
   DEF VAR lcEmailReplacedText AS CHAR NO-UNDO. 

   ASSIGN
       xMailFrom     = fCparam("EIF","EmailFromAddress")
       lcEmailFile   = fCparam("EIF","EmailFile")
       lcTransDir    = fCParam("EIF","MailArcDir")
       xMailAddr     = MsRequest.ReqCparam2.

   IF xMailFrom = "" OR xMailFrom = ? THEN
      RETURN "ERROR:Email From Address not available".

   IF lcEmailFile = "" OR lcEmailFile = ? THEN
      RETURN "ERROR:Email File not available".

   FIND FIRST FusionInvoice NO-LOCK WHERE
              FusionInvoice.FuInvNum = MsRequest.ReqIParam1 NO-ERROR.
   IF NOT AVAIL FusionInvoice THEN
      RETURN "ERROR:Fusion invoice not available".
   
   lcEmailReplacedText = fFillFusionEmailText(
                           BUFFER FusionInvoice,
                           lcEmailText,
                           xMailFrom,
                           OUTPUT lcError).
   IF lcError > "" THEN RETURN lcError.
   
   lcEmailFile = lcEmailFile + "_" + STRING(TODAY,"999999") + "_" +
                 STRING(TIME) + ".html".

   lcEmailFile = fUniqueFileName(lcEmailFile,"").
   OUTPUT STREAM sEmail TO VALUE(lcEmailFile).
   PUT STREAM sEmail UNFORMATTED lcEmailReplacedText SKIP.
   OUTPUT STREAM sEmail CLOSE.

   /* Send the email */
   SendMaileInvoice(lcEmailReplacedText,"").

   /* wait for confirmation */
   fReqStatus(2,"").

   /* move the file to archive directory */
   IF lcTransDir > "" THEN 
      fTransDir(lcEmailFile,
                ".rtf",
                lcTransDir).

   RETURN "".
   
END PROCEDURE.
