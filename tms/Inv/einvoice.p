/* ----------------------------------------------------------------------
  MODULE .......: smsinvoice.p 
  TASK .........: Handles sms invoice request (type 79), YDR-104 
  APPLICATION ..: TMS
  AUTHOR .......: ilsavola
  CREATED ......: 11/2017 
  Version ......: Yoigo
----------------------------------------------------------------------- */
/* Description: einvoice is called from newton side where invoice button
   pressed for any customers after billing run. This will generate request 
   that will call this program to create SMS to all invoiced customers.   */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/cparam2.i}
{Func/femailinvoice.i}
{Func/email.i}
{Func/smsnotify.i}
{Func/heartbeat.i}
{Func/aes_encrypt.i}

&SCOPED-DEFINE MIDNIGHT-SECONDS 86400

DEF INPUT PARAMETER iiMSrequest AS INT  NO-UNDO.
DEF VAR ldeActStamp             AS DECIMAL NO-UNDO.
DEF VAR ldaDateFrom             AS DATE NO-UNDO. 
DEF VAR liMonth                 AS INT  NO-UNDO. 
DEF VAR lcAddrConfDir           AS CHAR NO-UNDO.
DEF VAR lcContConFile           AS CHAR NO-UNDO.
DEF VAR lcMailContent           AS CHAR NO-UNDO.
DEF VAR liBillPeriod            AS INT  NO-UNDO.
DEF VAR lcMonitor AS CHAR NO-UNDO. 
DEF VAR lcPass AS CHAR NO-UNDO.
DEF VAR liStartTime   AS INT  NO-UNDO. 
DEF VAR liStopTime    AS INT  NO-UNDO. 
DEF VAR lEndSeconds   AS INTEGER   NO-UNDO.
DEF VAR lIniSeconds   AS INTEGER   NO-UNDO.
DEF VAR llFirstInv    AS LOGICAL   NO-UNDO.
DEF VAR lcTemplate    AS CHAR      NO-UNDO.
DEF VAR lcTestCustomers AS CHAR NO-UNDO.
DEF VAR lcLink        AS CHAR NO-UNDO.
DEF VAR liTestFilter AS INT NO-UNDO.
DEF STREAM sEmail.

/*Temp table for sending NC response info to WEB.*/
DEF TEMP-TABLE eInvoiceContent NO-UNDO
   FIELD MsSeq           AS CHAR
   FIELD MSISDN          AS CHAR
   FIELD Amount          AS CHAR
   FIELD InvDate         AS CHAR
   FIELD InvNum          AS INT
   FIELD InvNumCrypted   AS CHAR.


FUNCTION fGenerateEmailTemplate RETURNS CHAR
   (icTemplate AS CHAR,
    iiMsSeq AS INT,
    icMSISDN AS CHAR,
    ideAmount AS DECIMAL,
    icDate AS CHAR, 
    icLink AS CHAR,
    iiInvNum AS INT):
   DEF VAR lcTargetType AS CHAR NO-UNDO.
   DEF VAR llcMessage  AS LONGCHAR NO-UNDO.
   DEF VAR lcMessagePayload AS CHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.
   DEF VAR lcLocalLink AS CHAR NO-UNDO.
   DEF VAR lcInvNumCrypted AS CHAR NO-UNDO.

   ASSIGN
      lcInvNumCrypted =  encrypt_data(icMSISDN,
                                      {&ENCRYPTION_METHOD}, 
                                  lcPassPhrase) 
      /* convert some special characters to url encoding (at least '+' char
         could cause problems at later phases. */
      lcInvNumCrypted = fUrlEncode(lcInvNumCrypted, "query").
mgetmessage pÃ¤Ã¤ttÃ¤Ã¤ templaten.     
 linkbase + crypted
/*InvText.paramtext.keyvalue Jsonparam "MsSeq=#MSSEQ| ..."*/
/*TODO: obviously we need to build link in this function*/
   ASSIGN
      lcMessagePayload = icTemplate
      lcMessagePayload = REPLACE(lcMessagePayload,"#LINK",icLink + "/" + 
                                 lcInvNumCrypted)
      lcMessagePayload = REPLACE(lcMessagePayload,"#MSISDN",icMSISDN) 
      lcMessagePayload = REPLACE(lcMessagePayload,"#AMOUNT",STRING(ideAmount))
      lcMessagePayload = REPLACE(lcMessagePayload,"#INVDATE",icDate)
      lcMessagePayload = REPLACE(lcMessagePayload,"#INVNUM",STRING(iiInvNum)).

   IF lcMessagePayload NE "" AND lcMessagePayload ne ? THEN 
   RETURN lcMessagePayload.
RETURN "".
END.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
                 MsRequest.ReqType NE ({&REQTYPE_E_INVOICE}) THEN 
   RETURN "ERROR".

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

lcMonitor = fGetRequestNagiosToken(MsRequest.Reqtype).

/* Email Address Conf File */
ASSIGN lcAddrConfDir = fCParamC("RepConfDir")
       lcPass        = fCParamC("EinvoicePass")
       lcContConFile = fCParamC("SMSInvContFile")
       /* ie. "32400-79200" Send between 9:00-22:00 */
       ldaDateFrom   = MsRequest.ReqDtParam1
       liMonth       = MONTH(ldaDateFrom)
       liStartTime   = TIME
       llFirstInv    = FALSE
       lcLink        = fCParamC("ESI_LinkBase")
       liTestFilter  = fCparamI("ESI_TestFilter")
       lcTestCustomers = fCparamC("ESI_TestCustomers").

INVOICE_LOOP:
FOR EACH Invoice WHERE
         Invoice.Brand    = "1" AND
         Invoice.InvDate >= ldaDateFrom AND
         Invoice.DelType = {&INV_DEL_TYPE_ESI} AND
         Invoice.InvType  = 1 AND
         Invoice.InvAmt  >= 0 NO-LOCK:

   IF Invoice.InvCfg[1] THEN NEXT INVOICE_LOOP.

   IF MONTH(Invoice.InvDate) NE liMonth THEN NEXT INVOICE_LOOP.

   liBillPeriod = YEAR(Invoice.ToDate) * 100 + MONTH(Invoice.ToDate).

   FIND FIRST Customer WHERE
              Customer.Custnum = Invoice.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN NEXT INVOICE_LOOP.

   /*For testing: if test switch is on, allow only customers from test list*/
   IF liTestFilter NE 0 AND 
      LOOKUP(STRING(Customer.CustNum), lcTestCustomers) EQ 0 
      THEN NEXT INVOICE_LOOP.

   /* Sending SMS Invoice to customers */
   SUBINVOICE_LOOP:
   FOR EACH SubInvoice OF Invoice NO-LOCK:
   
      FIND FIRST MobSub WHERE
                 MobSub.MsSeq = SubInvoice.MsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub OR 
         MobSub.CustNum NE Invoice.CustNum THEN NEXT SUBINVOICE_LOOP.
      
      /*Notification for the First Invoice will be sent to a specific people as part of YOT-4037 along with the own customer*/
      IF llFirstInv = FALSE THEN DO:
         /*TODO: this also to MQ*/
/*         fSMSNotify("First",
                    "Einvoicing starts",
                    lcAddrConfDir,
                    lIniSeconds,
                    lEndSeconds).
         llFirstInv = TRUE.*/
      END.
         /**/
      IF Mm.MManMessage:mGetMessage("SMS", "EInvMessage", 1) EQ TRUE THEN DO: 
         lcTemplate = fGetSMSTxt("EInvMessage",
                                 TODAY,
                                 5,
                                 OUTPUT ldeActStamp).
         
         lcTemplate = fGenerateEmailTemplate(lcTemplate,
                                             MobSub.MsSeq,
                                             MobSub.CLI,
                                             Invoice.InvAmt,
                                             STRING(Invoice.InvDate),
                                             lcLink,
                                             Invoice.InvNum).
         Mm.MManMessage:ParamKeyValue = lcTemplate.
         Mm.MManMessage:mCreateMMLogSMS(MobSub.CLI).
      END.
   END. /* FOR EACH SubInvoice OF Invoice NO-LOCK: */
END. /* FOR EACH Invoice WHERE */


/*Einvoice project: inherit following from SMSinvoicing.*/
/*Notification for the Last Invoice will be sent to a specific people as part of YOT-4037 along with the own customer*/
/* TODO this also to MQ*/
/*
fSMSNotify("Last",
           "Last Eincoice Sent",
           lcAddrConfDir,
           lIniSeconds,
           lEndSeconds).
*/
/* Send an email to configure list*/
/*TODO: this must be removed because TMS is not aware when last sms is sent.*/
IF lcAddrConfDir > "" THEN
   lcAddrConfDir = lcAddrConfDir + "einvoice.email".

IF lcContConFile > "" AND SEARCH(lcAddrConfDir) <> ? THEN DO:
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = "1"            AND
             InvText.Target    = "General"          AND
             InvText.KeyValue  = "EmailConfEInv"  AND
             InvText.Language  = 5                  AND 
             InvText.FromDate <= today              AND
             InvText.ToDate   >= today:
       lcMailContent = InvText.InvText.
   END. /* FOR FIRST InvText NO-LOCK WHERE */

   OUTPUT TO VALUE(lcContConFile).
   PUT UNFORMATTED lcMailContent skip.
   OUTPUT CLOSE.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   SendMail(lcContConFile,"").
END. /* IF SEARCH(lcAddrConfDir) <> ? THEN DO: */

fReqStatus(2,""). /* request handled succesfully */
