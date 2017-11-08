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

DEF VAR ldaDateFrom             AS DATE NO-UNDO. 
DEF VAR liMonth                 AS INT  NO-UNDO. 
DEF VAR lcAddrConfDir           AS CHAR NO-UNDO.
DEF VAR lcContConFile           AS CHAR NO-UNDO.
DEF VAR lcMailContent           AS CHAR NO-UNDO.
DEF VAR liBillPeriod            AS INT  NO-UNDO.
DEF VAR lcMonitor AS CHAR NO-UNDO. 

DEF VAR liStartTime   AS INT  NO-UNDO. 
DEF VAR liStopTime    AS INT  NO-UNDO. 
DEF VAR lEndSeconds   AS INTEGER   NO-UNDO.
DEF VAR lIniSeconds   AS INTEGER   NO-UNDO.
DEF VAR llFirstInv    AS LOGICAL   NO-UNDO.
DEF VAR lcTemplate    AS CHAR      NO-UNDO.

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
   (iiMsSeq AS INT,
    icMSISDN AS CHAR,
    ideAmount AS DECIMAL,
    icDate AS CHAR, 
    iiInvNum AS INT):
   DEF VAR lcTargetType AS CHAR NO-UNDO.
   DEF VAR llcMessage  AS LONGCHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.
   DEF VAR lcPass AS CHAR NO-UNDO.

InvTextist.paramtext.keyvalue Jsonparam"MsSeq=#MSSEQ| ..."
   CREATE eInvoiceContent.
   ASSIGN
      eInvoiceContent.MsSeq = STRING(iiMsSeq)
      eInvoiceContent.MSISDN = icMSISDN
      eInvoiceContent.Amount = STRING(ideAmount)
      eInvoiceContent.InvDate = icDate
      eInvoiceContent.InvNum = iiInvNum
      eInvoiceContent.InvNumCrypted = encrypt_data(STRING(iiInvNum),
                                                   {&ENCRYPTION_METHOD},
                                                   lcPass).
   llgOK = TEMP-TABLE eInvoiceContent:WRITE-JSON("LONGCHAR", /*writing type*/
                                             llcMessage, /*target*/
                                             TRUE). /*formatted to readabale*/

   EMPTY TEMP-TABLE eInvoiceContent.
   IF llgOK EQ TRUE THEN RETURN STRING( llcMessage  ).


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
       lcPassPhrase  = fCParamC("EinvoicePass")
       lcContConFile = fCParamC("SMSInvContFile")
       /* ie. "32400-79200" Send between 9:00-22:00 */
       ldaDateFrom   = MsRequest.ReqDtParam1
       liMonth       = MONTH(ldaDateFrom)
       liStartTime   = TIME
       llFirstInv    = FALSE.

INVOICE_LOOP:
FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand AND
         Invoice.InvDate >= ldaDateFrom AND
         Invoice.DelType = {&INV_DEL_TYPE_NO_DELIVERY} AND
         Invoice.InvType  = 1 AND
         Invoice.InvAmt  >= 0 NO-LOCK:

   IF Invoice.InvCfg[1] THEN NEXT INVOICE_LOOP.

   IF MONTH(Invoice.InvDate) NE liMonth THEN NEXT INVOICE_LOOP.

   liBillPeriod = YEAR(Invoice.ToDate) * 100 + MONTH(Invoice.ToDate).

   FIND FIRST Customer WHERE
              Customer.Custnum = Invoice.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN NEXT INVOICE_LOOP.

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
         fSMSNotify("First",
                    "Einvoicing starts",
                    lcAddrConfDir,
                    lIniSeconds,
                    lEndSeconds).
         llFirstInv = TRUE.
      END.
      DO TRANS:
         lcTemplate = fGenerateEmailTemplate(MobSub.MsSeq,
                                             MobSub.CLI,
                                             Invoice.InvAmt,
                                             STRING(Invoice.InvDate),
                                             Invoice.InvNum
                                            ).

         /* fMakeSchedSMS2(MobSub.CustNum,
                        MobSub.CLI,
                        44,
                        lcSMSReplacedText,
                        fMakeTS(),
                        "Fact. Yoigo",
                        STRING(lIniSeconds) + "-" + STRING(lEndSeconds)). */

      END. /* DO TRANS: */
   END. /* FOR EACH SubInvoice OF Invoice NO-LOCK: */
END. /* FOR EACH Invoice WHERE */


/*Einvoice project: inherit following from SMSinvoicing.*/
/*Notification for the Last Invoice will be sent to a specific people as part of YOT-4037 along with the own customer*/
/*Ilkka TODO this also to MQ*/
fSMSNotify("Last",
           "Last Eincoice Sent",
           lcAddrConfDir,
           lIniSeconds,
           lEndSeconds).

/* Send an email to configure list*/
IF lcAddrConfDir > "" THEN
   lcAddrConfDir = lcAddrConfDir + "einvoice.email".

IF lcContConFile > "" AND SEARCH(lcAddrConfDir) <> ? THEN DO:
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand            AND
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
