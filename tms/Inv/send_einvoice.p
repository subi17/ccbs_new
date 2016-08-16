/* ----------------------------------------------------------------------
  MODULE .......: send_einvoice.p 
  TASK .........: Handle sending email invoice request (type 85)
  APPLICATION ..: TMS
  AUTHOR .......: vikasagr
  CREATED ......: 12.04.2012
  Version ......: Yoigo
----------------------------------------------------------------------- */
&GLOBAL-DEFINE MailTitleSpaces Allow

{commali.i}
{tmsconst.i}
{fmakemsreq.i}
{cparam2.i}
{email.i}
{femailinvoice.i}
{ftransdir.i}
{edefine.i NEW}
{heartbeat.i}
{refcode.i}

DEF INPUT PARAMETER iiMSrequest AS INT  NO-UNDO.

DEF VAR ldaDateFrom             AS DATE NO-UNDO. 
DEF VAR liMonth                 AS INT  NO-UNDO. 
DEF VAR lcSep                   AS CHAR NO-UNDO. 
DEF VAR lcAddrConfDir           AS CHAR NO-UNDO.
DEF VAR lcMailContent           AS CHAR NO-UNDO.
DEF VAR liBillPeriod            AS INT  NO-UNDO.
DEF VAR lcEmailFile             AS CHAR NO-UNDO.
DEF VAR lcTransDir              AS CHAR NO-UNDO.
DEF VAR lcEmailText             AS CHAR NO-UNDO.
DEF VAR lcEmailPDFLink          AS CHAR NO-UNDO.
DEF VAR lcLatestEmailFile       AS CHAR NO-UNDO.
DEF VAR lcEmailReplacedText     AS CHAR NO-UNDO.
DEF VAR lcMonitor               AS CHAR NO-UNDO. 
DEF VAR liLoop                  AS INT  NO-UNDO. 
DEF VAR lcName                  AS CHAR NO-UNDO. 
DEF VAR lcMonthName             AS CHAR NO-UNDO. 
DEF VAR lcMiYoigoLink           AS CHAR NO-UNDO.
DEF VAR lcAddrConfDirNotify     AS CHAR NO-UNDO.
DEF VAR lcLatestEmailFileNotify AS CHAR NO-UNDO.
/* DEF VAR lcQ25Note               AS CHAR NO-UNDO. Removed by YOT-4050 */

DEF STREAM sEmail.
DEF STREAM sNotify.

FUNCTION fPickMonthName RETURN CHARACTER
   (iiMonth AS INT,
    iiLanguage AS INT):

   DEF VAR lcMonth AS CHAR NO-UNDO. 
   DEF VAR liMonBegin AS INT NO-UNDO INIT 542.
   
   IF iiMonth > 0 THEN
      lcMonth = fTeksti(liMonBegin + iiMonth,iiLanguage).
   RETURN lcMonth.
END FUNCTION.

FUNCTION fNotify RETURN CHARACTER
   (lcType AS CHAR):
   DEF VAR lcMailSubj AS CHAR NO-UNDO.
   ASSIGN lcMailSubj = xMailSubj
          lcAddrConfDirNotify = lcAddrConfDir + "emailinvoicenotify.email".
   GetRecipients(lcAddrConfDirNotify).
   
   ASSIGN xMailSubj  = lcType + " invoice " + lcMailSubj + " This is your electronic invoice".
   
   ASSIGN lcLatestEmailFileNotify = lcEmailFile + "_" + STRING(Customer.CustNum) +
                                    "_" + "Notify_" + STRING(TODAY,"999999") + "_" +
                                    STRING(TIME) + ".html"
          lcLatestEmailFileNotify = fEPLFileName(lcLatestEmailFileNotify).
   OUTPUT STREAM sNotify TO VALUE(lcLatestEmailFileNotify).
   PUT STREAM sNotify UNFORMATTED  xMailSubj SKIP(1).
   PUT STREAM sNotify UNFORMATTED lcEmailReplacedText SKIP.
   OUTPUT STREAM sNotify CLOSE.

   SendMaileInvoice(lcEmailReplacedText,"","").
   IF lcTransDir > "" THEN
      fTransDir(lcLatestEmailFileNotify,
                ".html",
                lcTransDir).
   xMailSubj = lcMailSubj.
END FUNCTION.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR 
   MsRequest.ReqType NE {&REQTYPE_SEND_EMAIL_INVOICE} THEN RETURN "ERROR".

lcMonitor = fGetRequestNagiosToken(MsRequest.Reqtype).

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

/* Email Address Conf File */
ASSIGN lcAddrConfDir = fCParamC("RepConfDir")
       ldaDateFrom   = MsRequest.ReqDtParam1
       liMonth       = MONTH(ldaDateFrom)
       xMailFrom     = fCparam("EI","EmailFromAddress")
       lcEmailFile   = fCparam("EI","EmailPDFFile")
       lcTransDir    = fCParam("EI","PDFMailArcDir")
       lcMiYoigoLink = fCparam("URL","MiYoigoInvoiceURL").

INVOICE_LOOP:
FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand AND
         Invoice.InvType  = 1 AND
         Invoice.InvDate >= ldaDateFrom AND
         Invoice.InvAmt  >= 0 AND
         Invoice.DelType  = {&INV_DEL_TYPE_EMAIL} NO-LOCK
   BREAK BY Invoice.InvType:

   IF Invoice.InvCfg[1] THEN NEXT INVOICE_LOOP.

   IF MONTH(Invoice.InvDate) NE liMonth THEN NEXT INVOICE_LOOP.

   liBillPeriod = YEAR(Invoice.ToDate) * 100 + MONTH(Invoice.ToDate).

   FIND FIRST Customer WHERE
              Customer.Custnum = Invoice.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN NEXT INVOICE_LOOP.

   IF Customer.Email = "" OR Customer.Email = ? THEN NEXT INVOICE_LOOP.

   lcSep = (IF Customer.Language = 5 THEN "." ELSE ",").

   lcEmailText = fGetEmailText("EMAIL",
                               "SendEmailInvoice",
                               Customer.Language,
                               OUTPUT xMailSubj).

   IF lcEmailText = "" THEN NEXT INVOICE_LOOP.

   ASSIGN lcLatestEmailFile = lcEmailFile + "_" + STRING(Customer.CustNum) +
                              "_" + STRING(TODAY,"999999") + "_" +
                              STRING(TIME) + ".html"
          lcLatestEmailFile = fEPLFileName(lcLatestEmailFile).

   /* Replace the Tokens and generate the links */
   lcEmailPDFLink = fGenerateEmailPDFLink(INPUT liBillPeriod,
                                          INPUT Customer.CustNum,
                                          INPUT Invoice.InvNum).
   IF lcEmailPDFLink = "" OR lcEmailPDFLink = ? THEN
      NEXT INVOICE_LOOP.

   IF Customer.Language = 1 OR 
      Customer.Language = 2 OR
      Customer.Language = 5 THEN 
   lcMonthName = fPickMonthName(MONTH(Invoice.ToDate),Customer.Language).
   

   ASSIGN 
          lcEmailReplacedText = REPLACE(lcEmailText,"#NAME",
                                        REPLACE(Customer.FirstName,"'","&#39"))
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#MONTH",lcMonthName)
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#YEAR",STRING(YEAR(Invoice.ToDate)))
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#LINK",lcEmailPDFLink)
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#MiYoigoLINK",lcMiYoigoLink)
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#EMAIL",xMailFrom)
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#AMOUNT", 
          REPLACE(TRIM(STRING(Invoice.InvAmt,"->>>>>>9.99")),".",lcSep)).
   /*Notification for the First AND Last Invoice will be sent to a specific group as part of YOT-4037*/
   IF FIRST-OF(Invoice.InvType) THEN fNotify("First").
   ELSE IF LAST-OF(Invoice.InvType) THEN fNotify("Last").
   
   xMailAddr = Customer.Email.
   
   OUTPUT STREAM sEmail TO VALUE(lcLatestEmailFile).
   PUT STREAM sEmail UNFORMATTED xMailSubj SKIP(1).
   PUT STREAM sEmail UNFORMATTED lcEmailReplacedText SKIP.
   OUTPUT STREAM sEmail CLOSE.
   
   /* Send the email */
   SendMaileInvoice(lcEmailReplacedText,"","").

   /* move the file to archive directory */
   IF lcTransDir > "" THEN 
      fTransDir(lcLatestEmailFile,
                ".html",
                lcTransDir).

   liLoop = liLoop + 1.
   IF liLoop MOD 1000 EQ 0 AND
      lcMonitor > "" THEN fKeepAlive(lcMonitor).
END. /* FOR EACH Invoice WHERE */

/* Send an email to configure list*/
IF lcAddrConfDir > "" THEN
   lcAddrConfDir = lcAddrConfDir + "emailinvoice.email".

lcTransDir = lcTransDir + "/einvoice.log".

IF lcTransDir > "" AND SEARCH(lcAddrConfDir) <> ? THEN DO:
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand            AND
             InvText.Target    = "General"          AND
             InvText.KeyValue  = "EmailConfeInvoice" AND
             InvText.Language  = 5                  AND 
             InvText.FromDate <= today              AND
             InvText.ToDate   >= today:
       lcMailContent = InvText.InvText.
   END. /* FOR FIRST InvText NO-LOCK WHERE */

   OUTPUT TO VALUE(lcTransDir).
   PUT UNFORMATTED lcMailContent skip.
   OUTPUT CLOSE.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   SendMail(lcTransDir,"").
END. /* IF SEARCH(lcAddrConfDir) <> ? THEN DO: */

fReqStatus(2,""). /* request handled succesfully */
