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
DEF VAR lcQ25Note               AS CHAR NO-UNDO. 


DEF STREAM sEmail.

FUNCTION fPickMonthName RETURN CHARACTER
   (iiMonth AS INT,
    iiLanguage AS INT):

   DEF VAR lcMonth AS CHAR NO-UNDO. 
   DEF VAR liMonBegin AS INT NO-UNDO INIT 542.
   
   IF iiMonth > 0 THEN
      lcMonth = fTeksti(liMonBegin + iiMonth,iiLanguage).
   RETURN lcMonth.
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
       lcTransDir    = fCParam("EI","PDFMailArcDir").

INVOICE_LOOP:
FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand AND
         Invoice.InvType  = 1 AND
         Invoice.InvDate >= ldaDateFrom AND
         Invoice.InvAmt  >= 0 AND
         Invoice.DelType  = {&INV_DEL_TYPE_EMAIL} NO-LOCK:

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
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#EMAIL",xMailFrom)
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#AMOUNT", 
          REPLACE(TRIM(STRING(Invoice.InvAmt,"->>>>>>9.99")),".",lcSep))
          xMailAddr = Customer.Email.

   /* additional text for q25 cases */
   RUN pGetQ25Text(Invoice.InvNum,
                   Invoice.CustNum,
                   Invoice.InvDate,
                   Customer.Language,
                   OUTPUT lcQ25Note).
   lcEmailReplacedText = REPLACE(lcEmailReplacedText,"#Q25NOTE",lcQ25Note).

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


PROCEDURE pGetQ25Text:

   DEF INPUT  PARAMETER iiInvNum    AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiInvCust   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiLanguage  AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER ocText      AS CHAR NO-UNDO.
   
   DEF VAR liQ25Qty    AS INT  NO-UNDO.
   DEF VAR liFees      AS INT  NO-UNDO.
   DEF VAR liMonth     AS INT  NO-UNDO.
   DEF VAR lcCLI       AS CHAR NO-UNDO.
   DEF VAR ldaTo       AS DATE NO-UNDO.
   DEF VAR lcTextID    AS CHAR NO-UNDO.
   DEF VAR liInvPeriod AS INT  NO-UNDO.
   DEF VAR liMaxPeriod AS INT  NO-UNDO.
   DEF VAR lcQ25Link   AS CHAR NO-UNDO.
   
   DEF BUFFER bExtension FOR DCCLI.

   ASSIGN 
      ocText = ""
      liInvPeriod = YEAR(idaInvDate) * 100 + MONTH(idaInvDate)
      liMaxPeriod = IF MONTH(idaInvDate) <= 8
                    THEN YEAR(idaInvDate) * 100 + MONTH(idaInvDate) + 3
                    ELSE (YEAR(idaInvDate) + 1) * 100 +
                         3 - (12 - MONTH(idaInvDate)).
   
   FOR EACH SubInvoice NO-LOCK WHERE
            SubInvoice.InvNum = iiInvNum AND
            SubInvoice.MsSeq > 0,
      FIRST MobSub NO-LOCK WHERE
            MobSub.MsSeq = SubInvoice.MsSeq,
      FIRST SingleFee NO-LOCK USE-INDEX HostTable WHERE
            SingleFee.Brand       = gcBrand AND
            SingleFee.HostTable   = "Mobsub" AND
            SingleFee.KeyValue    = STRING(SubInvoice.MsSeq) AND
            SingleFee.CustNum     = iiInvCust AND
            SingleFee.BillPeriod <= liMaxPeriod AND
            SingleFee.SourceTable = "DCCLI" AND
            SingleFee.CalcObj     = "RVTERM" AND
            SingleFee.Active AND
            SingleFee.Billed = FALSE,
      FIRST DCCLI NO-LOCK WHERE
            DCCLI.PerContractId = INT(SingleFee.SourceKey) AND
            DCCLI.MsSeq   = INT(SingleFee.KeyValue) AND
            DCCLI.DCEvent = SingleFee.CalcObj AND
            /* not yet terminated */
            DCCLI.TermDate = ? AND
            /* renewal not done */
            DCCLI.RenewalDate = ?:

      IF INTERVAL(DCCLI.ValidTo,DCCLI.ValidFrom,"Months") < 23 THEN NEXT. 
      
      /* extension done */
      IF CAN-FIND(FIRST FixedFee WHERE
            FixedFee.Brand = gcBrand AND
            FixedFee.HostTable = "MobSub" AND
            FixedFee.KeyValue = STRING(SubInvoice.MsSeq) AND
            FixedFee.BillCode BEGINS "RVTERM" AND
            FixedFee.InUse) THEN NEXT.
      IF CAN-FIND(FIRST bExtension NO-LOCK WHERE
               bExtension.Brand   = gcBrand AND
               bExtension.DCEvent = "RVTERM12" AND
               bExtension.MsSeq   = SubInvoice.MsSeq AND
               bExtension.ValidTo >= TODAY) THEN NEXT.
         
      /* terminal returned */
      FIND FIRST TermReturn WHERE
                 TermReturn.OrderId = SingleFee.OrderId AND
                 TermReturn.ReturnTS > fHMS2TS(DCCLI.ValidFrom,"0") 
         NO-LOCK NO-ERROR.
      IF AVAIL TermReturn AND TermReturn.DeviceScreen AND
               TermReturn.DeviceStart THEN NEXT.
      
      liFees = 0.
      /* which month is this */
      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand = gcBrand AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue = SingleFee.KeyValue AND
                FixedFee.BillCode BEGINS "PAYTERM" AND
                FixedFee.SourceKey = SingleFee.SourceKey AND
                FixedFee.InUse AND
                FixedFee.EndPer >= liInvPeriod,
           EACH FFItem OF FixedFee NO-LOCK WHERE
                FFItem.BillPeriod > liInvPeriod:
         liFees = liFees + 1.       
      END.        
      IF liFees = 0 OR liFees > 3 THEN NEXT.      
      
      /* msisdn is used only if there is one subscription in q25 */
      ASSIGN 
         lcCLI = MobSub.CLI
         liQ25Qty = liQ25Qty + 1.

      /* text according to the oldest */
      IF 25 - liFees > liMonth THEN ASSIGN
         ldaTo = DCCLI.ValidTo
         liMonth = 25 - liFees.
   END.          

   IF liMonth = 24 THEN lcTextID = "EMailInvoiceQ25Final".
   ELSE lcTextID = "EMailInvoiceQ25Prior".

   ocText = fGetEmailText("EMAIL",
                          lcTextID,
                          iiLanguage,
                          OUTPUT xMailSubj).

   IF liQ25Qty > 1 THEN lcCLI = "".
   lcQ25Link = fGenerateQ25Link(lcCLI).
   
   IF lcQ25Link = ? OR lcQ25Link = "" THEN ocText = "".
   
   ASSIGN 
      ocText = REPLACE(ocText,"#Q25DD",STRING(DATE(ldaTo),"99")) 
      ocText = REPLACE(ocText,"#Q25MM",STRING(MONTH(ldaTo),"99"))
      ocText = REPLACE(ocText,"#Q25LINK",lcQ25Link).

END PROCEDURE.

