/* ----------------------------------------------------------------------
  MODULE .......: activate_einvoice.p  
  TASK .........: Handle eInvoice Activation Requests
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 29.03.12
---------------------------------------------------------------------- */
&GLOBAL-DEFINE MailTitleSpaces Allow

{commali.i}
{tmsconst.i}
{date.i}
{timestamp.i}
{email.i}
{femailinvoice.i}
{ftransdir.i}
{edefine.i NEW}

DEF INPUT PARAMETER iiMsRequest AS INT  NO-UNDO.

DEF STREAM sEmail.

/******** Main start *********/

FIND FIRST MsRequest WHERE
           MsRequest.MSRequest = iiMsRequest  AND
           MsRequest.ReqType   = {&REQTYPE_ACTIVATE_EMAIL_INVOICE}
     NO-LOCK NO-ERROR.
IF NOT AVAIL MsRequest THEN
   RETURN "ERROR:Unknown MSRequest " + STRING(iiMsRequest).

CASE MsRequest.ReqStatus:
   WHEN {&REQUEST_STATUS_NEW} THEN
         RUN pSendEmail.
   OTHERWISE RETURN "ERROR:Current status is not handled".
END CASE. /* CASE MsRequest.ReqStatus: */

IF RETURN-VALUE BEGINS "ERROR:" THEN
   fReqError(RETURN-VALUE).

RETURN RETURN-VALUE.

/******** Main end *********/


PROCEDURE pSendEmail:

   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR liRequest          AS INT  NO-UNDO.

   DEF VAR lcEmailSubject     AS CHAR NO-UNDO.
   DEF VAR lcEmailAddress     AS CHAR NO-UNDO.
   DEF VAR lcEmailText        AS CHAR NO-UNDO.
   DEF VAR lcEmailFile        AS CHAR NO-UNDO.
   DEF VAR lcTransDir         AS CHAR NO-UNDO.
   DEF VAR lcEmailActLink     AS CHAR NO-UNDO.

   FIND FIRST Customer WHERE
              Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "ERROR:Customer not found".

   IF Customer.Email = "" OR Customer.Email = ? THEN
      RETURN "ERROR:Customer email address is blank".

   lcEmailText = fGetEmailText(INPUT "EMAIL",
                               INPUT "ActEmailInvoice",
                               INPUT Customer.Language,
                               OUTPUT xMailSubj).

   IF lcEmailText = "" THEN RETURN "ERROR:Email text not available".

   ASSIGN xMailFrom   = fCparam("EI","ValidationEmailFromAddress")
          lcEmailFile = fCparam("EI","EmailActFile")
          lcTransDir  = fCParam("EI","ActMailArcDir").

   IF MsRequest.ReqCparam1 > "" AND
      MsRequest.ReqCparam1 <> Customer.Email THEN
      RETURN "ERROR:Email Address is not same as Customer Email".

   IF MsRequest.ReqCparam1 > "" THEN
      xMailAddr = MsRequest.ReqCparam1.
   ELSE
      xMailAddr = Customer.Email.

   IF xMailAddr = "" OR xMailAddr = ? THEN
      RETURN "ERROR:Customer Email Address not available".

   IF xMailFrom = "" OR xMailFrom = ? THEN
      RETURN "ERROR:Email From Address not available".

   IF xMailSubj = "" OR xMailSubj = ? THEN
      RETURN "ERROR:Email Subject not available".

   IF lcEmailFile = "" OR lcEmailFile = ? THEN
      RETURN "ERROR:Email File not available".

   lcEmailFile = lcEmailFile + "_" + STRING(Customer.CustNum) + "_" +
                 STRING(TODAY,"999999") + "_" + STRING(TIME) + ".html".

   lcEmailFile = fEPLFileName(lcEmailFile).

   /* Replace the Tokens and generate the links */
   lcEmailActLink = fGenerateEmailActLink(INPUT MsRequest.MsRequest,
                                          INPUT Customer.CustNum,
                                          INPUT xMailAddr).
   IF lcEmailActLink = "" OR lcEmailActLink = ? THEN
      RETURN "ERROR:Email Activation Link is blank".

   ASSIGN lcEmailText = REPLACE(lcEmailText,"#CUSTNAME",Customer.FirstName)
          lcEmailText = REPLACE(lcEmailText,"#LINK",lcEmailActLink)
          lcEmailText = REPLACE(lcEmailText,"#EMAIL",xMailFrom).

   lcEmailText = REPLACE(lcEmailText,"'","").

   OUTPUT STREAM sEmail TO VALUE(lcEmailFile).
   PUT STREAM sEmail UNFORMATTED xMailSubj SKIP(1).
   PUT STREAM sEmail UNFORMATTED lcEmailText SKIP.
   OUTPUT STREAM sEmail CLOSE.

   /* Send the email */
   SendMaileInvoice(lcEmailText,"").

   /* wait for confirmation */
   fReqStatus(19,"").

   /* move the file to archive directory */
   IF lcTransDir > "" THEN 
      fTransDir(lcEmailFile,
                ".html",
                lcTransDir).

   RETURN "".
   
END PROCEDURE. /* PROCEDURE pSendEmail: */

