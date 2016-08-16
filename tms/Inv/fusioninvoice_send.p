/* ----------------------------------------------------------------------
  MODULE .......: fusioninvoice_send.p 
  TASK .........: Handle fusion summary email request (type 90)
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 10.12.2013
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
{heartbeat.i}
{fusioninvoice.i}

DEF INPUT PARAMETER iiMSrequest AS INT  NO-UNDO.

DEF VAR ldaDateFrom             AS DATE NO-UNDO. 
DEF VAR lcEmailFile             AS CHAR NO-UNDO.
DEF VAR lcTransDir              AS CHAR NO-UNDO.
DEF VAR lcEmailAddress          AS CHAR NO-UNDO. 
DEF VAR lcEmailText             AS CHAR NO-UNDO.
DEF VAR lcLatestEmailFile       AS CHAR NO-UNDO.
DEF VAR lcEmailReplacedText     AS CHAR NO-UNDO.
DEF VAR lcMonitor               AS CHAR NO-UNDO. 
DEF VAR liLoop                  AS INT NO-UNDO. 
DEF VAR ldaInvDateTo            AS DATE NO-UNDO. 
DEF VAR liRequest               AS INT NO-UNDO. 
DEF VAR lcLogDir                AS CHAR NO-UNDO. 
DEF VAR lcReminderSMS           AS CHAR NO-UNDO. 
DEF VAR lcError                 AS CHAR NO-UNDO. 
DEF VAR ldeSendTime             AS DEC NO-UNDO.
DEF VAR lcAddrConfDirNotify     AS CHAR NO-UNDO.
DEF VAR lcLatestEmailFileNotify AS CHAR NO-UNDO.

DEF STREAM sEmail.
DEF STREAM sLog.
DEF STREAM sNotify.

FUNCTION fLog RETURNS LOG
   (iiFusionInvNum AS INT,
    icNote AS CHAR):

   PUT STREAM sLog UNFORMATTED
      iiFusionInvNum ";"
      icNote SKIP.
END.

FUNCTION fNotify RETURN CHARACTER
   (lcType AS CHAR):
   DEF VAR lcMailSubj AS CHAR NO-UNDO.
   ASSIGN lcMailSubj = xMailSubj
          lcAddrConfDirNotify = lcAddrConfDirNotify + "emailinvoicenotify.email".
   GetRecipients(lcAddrConfDirNotify).
   ASSIGN xMailSubj  = lcType + " invoice " + lcMailSubj.
   ASSIGN lcLatestEmailFileNotify = lcEmailFile + "_" + STRING(Customer.CustNum) +
                                    "_" + "Notify_" + STRING(TODAY,"999999") + "_" +
                                    STRING(TIME) + ".html".
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
   MsRequest.ReqType NE {&REQTYPE_FUSION_EMAIL} THEN RETURN "ERROR".

lcMonitor = fGetRequestNagiosToken(MsRequest.Reqtype).

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

/* Email Address Conf File */
ASSIGN lcAddrConfDirNotify = fCParamC("RepConfDir")
       ldaDateFrom   = MsRequest.ReqDtParam1
       ldaInvDateTo  = fLastDayOfMonth(ldaDateFrom)
       xMailFrom     = fCparam("EIF","EmailFromAddress")
       lcEmailFile   = fCparam("EIF","EmailFile")
       lcTransDir    = fCParam("EIF","MailArcDir")
       lcLogDir      = fCParam("EIF","MailLogDir").

IF lcTransDir EQ ? OR lcTransDir EQ "" OR
   lcLogDir EQ ? OR lcLogDir EQ "" THEN DO:
   fReqError("Directory definition missing").
   RETURN.
END.

OUTPUT STREAM slog to VALUE(lcLogDir + "fusioninvoice_" + 
   STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + 
   "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log") APPEND.

lcEmailText = fGetEmailText("EMAIL",
                            "FusionEmail",
                            1,
                            OUTPUT xMailSubj).

IF lcEmailText = "" THEN DO:
   fReqError("Email definition missing").
   RETURN.
END.

lcReminderSMS = fGetSMSText("SMS",
                            "ActEmailInvoiceReminder",
                            1).

IF NOT lcReminderSMS > "" THEN DO:
   fReqError("Reminder SMS definition missing").
   RETURN.
END.

INVOICE_LOOP:
FOR EACH FusionInvoice EXCLUSIVE-LOCK WHERE
         FusionInvoice.InvDate >= ldaDateFrom AND
         FusionInvoice.InvDate <= ldaInvDateTo AND
         FusionInvoice.DeliveryState = {&FI_DELIVERY_STATE_NEW}
    BREAK BY FusionInvoice.DeliveryState:

   /* Yoigo or Yoigo+Telefonica customer */
   IF FusionInvoice.InvNum > 0 THEN DO:

      FIND Customer NO-LOCK WHERE
           Customer.Custnum = FusionInvoice.Custnum NO-ERROR.
      IF NOT AVAIL Customer THEN DO:
         fLog(FusionInvoice.FuInvNum,"ERROR:Customer not found").
         NEXT.
      END.

      FIND FIRST Invoice NO-LOCK WHERE
                 Invoice.InvNum = FusionInvoice.InvNum NO-ERROR.
      IF NOT AVAIL Invoice THEN DO:
         fLog(FusionInvoice.FuInvNum,"ERROR:Invoice not found").
         NEXT.
      END.

      IF Invoice.InvCfg[1] THEN NEXT.

      FIND InvoiceTargetGroup NO-LOCK WHERE
           InvoiceTargetGroup.ITGroupID = Invoice.ITGroupID NO-ERROR.
      IF NOT AVAIL InvoiceTargetGroup THEN DO:
         fLog(FusionInvoice.FuInvNum,"ERROR:Invoice target group not found").
         NEXT.
      END.
         
      IF Customer.Email EQ ? OR Customer.Email EQ "" THEN DO:
         fLog(FusionInvoice.FuInvNum,"ERROR:Customer email is empty").
         NEXT.
      END.

      FIND Mobsub NO-LOCK WHERE
           Mobsub.MsSeq = FusionInvoice.MsSeq NO-ERROR.
         
      IF AVAIL Mobsub AND 
         InvoiceTargetGroup.Deltype NE {&INV_DEL_TYPE_FUSION_EMAIL} THEN DO:

         IF fPendingEmailActRequest(Customer.Custnum) THEN DO:
            fLog(FusionInvoice.FuInvNum,"INFO:Activation SMS sent").
            FusionInvoice.DeliveryState = {&FI_DELIVERY_STATE_SMS}.
         END.
         ELSE DO:
            liRequest = fEmailInvoiceRequest(
                                 INPUT fMakeTS(),
                                 INPUT TODAY,
                                 INPUT katun,
                                 INPUT 0,
                                 INPUT "",
                                 INPUT Customer.CustNum,
                                 INPUT {&REQUEST_SOURCE_FUSION_EMAIL},
                                 INPUT Customer.Email,
                                 INPUT 0, /*orderid*/ 
                                 OUTPUT lcError).
            IF liRequest > 0 THEN DO:
               fLog(FusionInvoice.FuInvNum,"INFO:Activation email sent").
               FusionInvoice.DeliveryState = {&FI_DELIVERY_STATE_SMS}.
            END.
            ELSE DO:
               fLog(FusionInvoice.FuInvNum,
                    SUBST("WARNING:Activation email request failed: &1",
                           lcError)).
               NEXT.
            END.
         END.

         lcReminderSMS = fGetSMSTxt("ActEmailInvoiceReminder",
                                    TODAY,
                                    Customer.Language,
                                    OUTPUT ldeSendTime).

         fMakeSchedSMS2(Mobsub.CustNum,
                        Mobsub.CLI,
                        44,
                        lcReminderSMS,
                        ldeSendTime,
                        "Fact. Yoigo",
                        "").

      END.
      lcEmailAddress = Customer.Email.
   END.
   /* Telefonica only customer */
   ELSE DO:
      /* YOT-4340 Stop sending Fusion Emails invoice for customer 
         with only fix part*/
      FusionInvoice.DeliveryState = {&FI_DELIVERY_STATE_CANCELLED}.
      NEXT.
   END.
   IF lcEmailAddress EQ ? OR lcEmailAddress EQ "" THEN DO:
      fLog(FusionInvoice.FuInvNum,"ERROR:Email address not defined").
      NEXT.
   END.
      
   lcEmailReplacedText = fFillFusionEmailText(
                              BUFFER FusionInvoice,
                              lcEmailText,
                              xMailFrom,
                              OUTPUT lcError).
   IF lcError > "" THEN DO:
      fLog(FusionInvoice.FuInvNum,lcError).
      NEXT.
   END.
   
   ASSIGN lcLatestEmailFile = lcEmailFile + "_" +
                              STRING(FusionInvoice.FuInvNum) +
                              "_" + STRING(TODAY,"999999") + "_" +
                              STRING(TIME) + ".html"
          lcLatestEmailFile = fUniqueFileName(lcLatestEmailFile,"")
          lcEmailReplacedText = REPLACE(lcEmailReplacedText,"'","").
   
   /*Notification for the First AND Last Invoice will be sent to a specific people as part of YOT-4037 along WITH the own customer*/
   IF FIRST-OF(FusionInvoice.DeliveryState) THEN fNotify("Primero").
   ELSE IF LAST-OF(FusionInvoice.DeliveryState) THEN fNotify("Último").

   xMailAddr = lcEmailAddress.

   OUTPUT STREAM sEmail TO VALUE(lcLatestEmailFile).
   PUT STREAM sEmail UNFORMATTED xMailSubj SKIP(1).
   PUT STREAM sEmail UNFORMATTED lcEmailReplacedText SKIP.
   OUTPUT STREAM sEmail CLOSE.

   /* Send the email */
   SendMaileInvoice(lcEmailReplacedText,"",""). 
   /* move the file to archive directory */
   IF lcTransDir > "" THEN DO:
      fTransDir(lcLatestEmailFile,
                ".html",
                lcTransDir).
   END.
   
   FusionInvoice.DeliveryState = {&FI_DELIVERY_STATE_EMAIL}.
   fLog(FusionInvoice.FuInvNum,SUBST("OK:&1",xMailAddr)).

   liLoop = liLoop + 1.
   IF liLoop MOD 1000 EQ 0 AND
      lcMonitor > "" THEN fKeepAlive(lcMonitor).
END. /* FOR EACH Invoice WHERE */

OUTPUT STREAM slog CLOSE.

fReqStatus(2,""). /* request handled succesfully */
