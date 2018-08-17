/* ----------------------------------------------------------------------
  MODULE .......: einvoice.p 
  TASK .........: Handles electronic invoice request
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
{Func/femailinvoice.i}
{Func/heartbeat.i}
{Func/msisdn_prefix.i}
{Func/msreqfunc.i}
{Func/email.i}


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
DEF VAR lcTemplate    AS CHAR      NO-UNDO.
DEF VAR lcTestCustomers AS CHAR NO-UNDO.
DEF VAR lcLink        AS CHAR NO-UNDO.
DEF VAR liTestFilter AS INT NO-UNDO.
DEF VAR lcRecipient AS CHAR NO-UNDO.
DEF VAR lcContactNbr AS CHAR NO-UNDO.
DEF VAR llFirstInv AS LOGICAL NO-UNDO INIT FALSE.
DEF VAR liCounter AS INT NO-UNDO.

DEF STREAM sIn. /*1st/Last notification recipients*/

DEF STREAM sPilot.
OUTPUT STREAM sPilot TO VALUE("/scratch/log/esi/ESI_logging.txt") APPEND.
PUT STREAM sPilot UNFORMATTED "Satrt " + 
                               STRING(DATE(TODAY)) + " " +
                               STRING(TIME,"hh:mm:ss") SKIP.

FUNCTION fFindSMSTarget RETURNS CHAR
   (icCLI AS CHAR):
   DEF BUFFER bMobsub FOR mobsub.
   DEF BUFFER bCustomer FOR customer.

   if fIsMobileNumber(icCLI) EQ TRUE THEN RETURN icCLI.
   ELSE DO:
      FIND FIRST bMobsub NO-LOCK WHERE
                 bMobsub.Brand EQ "1" AND
                 bMobsub.CLI EQ icCLI NO-ERROR.
      IF NOT AVAIL bMobsub THEN RETURN "".
      FIND FIRST bCustomer  NO-LOCK  WHERE
                 bCustomer.brand eq bMobsub.Brand AND
                 bCustomer.Custnum EQ bMobsub.Custnum.
     IF NOT AVAIL bCustomer THEN RETURN "".
     RETURN bCustomer.smsnumber.
      /*Find contactnumber.*/
   END.
   RETURN "".
END.

/**/
FUNCTION fShortCrypt RETURNS CHAR
   (icMSISDN AS CHAR,
    iiPeriod AS INT):
   DEF VAR lcChSet AS CHAR NO-UNDO.  /* CharSet of out string */
   DEF VAR lcNum AS CHAR NO-UNDO.    /* Temp variable */
   DEF VAR liNum AS INT64 NO-UNDO.   /* long int to be converted to new set */
   DEF VAR liBase AS INT64 NO-UNDO.  /* base: how many chars are in char set */
   DEF VAR liRem AS INT64 NO-UNDO.   /* remainder in modulo */
   DEF VAR lcCharInPosition AS CHAR NO-UNDO. /* corresponding char in chset */
   DEF VAR lcOut AS CHAR NO-UNDO.    /* Result */
   /*Support variables for creating security part */
   DEF VAR liLen AS INT NO-UNDO.
   DEF VAR lcDigest AS CHAR NO-UNDO. /* Key to support crypting */
   DEF VAR lcAlpha AS CHAR NO-UNDO.
   DEF VAR lcDigestPart AS CHAR NO-UNDO.
   DEF VAR liPosition AS INT NO-UNDO.

   /*1234567890*/
   /*123 = 3*10^0 + 2*10^1 + 1*10^2*  */
   lcNum = icMSISDN + SUBSTRING(STRING(iiPeriod),3). /* 201806 -> 1806 */
   liNum = INT64(lcNum).
   lcChSet = "kD0EFGHI1Zz5fghijlmnoAYqrstNOPQRS987cdepuvwxTUVW2346abyBCJKLMX"
   /*+ "<>|-=_.:;,!#¤%&()@§".*/ .
   /*lcDigest = STRING(MESSAGE-DIGEST("SHA-256", lcNum, "S0mmarEn")).*/
    lcDigest = HMAC-BASE64("SHA-256", "S0mmarEn", lcNum).

   /* HEX test  lcChSet = "0123456789ABCDEF".*/
   liBase = LENGTH(lcChSet).
   IF liNum EQ 0 THEN lcOut = SUBSTRING(lcChSet, 1, 1).
   DO WHILE liNum > 0:
      liRem = liNum MOD liBase.
      /* in progress array starts from 1 */
      lcCharInPosition = SUBSTRING(lcChSet, (liRem + 1), 1).
      liNum = TRUNCATE(liNum / liBase, 0).
      lcOut = lcCharInPosition + lcOut.

   END.
   /* Calculete security part */
   /* Take alphabethical characters from digest */
   /* If there are no´t enough alphabets then assign A */
   liPosition = 1.
   lcAlpha = "abcdefghijklmnopqrstuvwxyz" +
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
   DO WHILE LENGTH(lcDigestPart) < 5:

      IF liPosition > LENGTH(lcDigest) THEN DO:
         lcDigestPart = lcDigestPart + "A".
      END.
      ELSE IF INDEX(lcAlpha, SUBSTRING(lcDigest, liPosition,1)) > 0 THEN
         lcDigestPart = lcDigestPart + SUBSTRING(lcDigest, liPosition,1).

      liPosition = liPosition + 1.

   END.
   lcOut = lcDigestPart + lcOut.

   RETURN lcOut.
END.

FUNCTION fGenerateEinvoiceTemplate RETURNS CHAR
   (icTemplate AS CHAR,
    iiMsSeq AS INT,
    icMSISDN AS CHAR,
    ideAmount AS DECIMAL,
    icDate AS CHAR, 
    icLink AS CHAR,
    iiInvNum AS INT,
    iiPeriod AS INT, 
    iiLang AS INT):
   DEF VAR lcTargetType AS CHAR NO-UNDO.
   DEF VAR llcMessage  AS LONGCHAR NO-UNDO.
   DEF VAR lcMessagePayload AS CHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.
   DEF VAR lcLocalLink AS CHAR NO-UNDO.
   DEF VAR lcCrypted AS CHAR NO-UNDO.
   DEF VAR lcLang AS CHAR NO-UNDO.

   IF iiLang EQ 1 THEN lcLAng = "es". 
   ELSE IF iiLang EQ 2 THEN lcLang = "ca".
   ELSE IF iiLang EQ 3 THEN lcLang = "eu".
   ELSE lcLang = "en".
 
   ASSIGN
      
      /* lcCrypted =  encrypt_data(icMSISDN + "|" + 
                                STRING(iiPeriod),
                                {&ENCRYPTION_METHOD}, 
                                {&ESI_PASSPHRASE}) */
      lcCrypted = fShortCrypt(icMSISDN, iiPeriod) 
      /* convert some special characters to url encoding (at least '+' char
         could cause problems at later phases. */
      lcCrypted = fUrlEncode(lcCrypted, "query").
   ASSIGN
      lcMessagePayload = icTemplate
      lcMessagePayload = REPLACE(lcMessagePayload,"#LANG",lcLang)
      lcMessagePayload = REPLACE(lcMessagePayload,"#LINK",icLink + lcCrypted )
      lcMessagePayload = REPLACE(lcMessagePayload,"#MSISDN",icMSISDN) 
      lcMessagePayload = REPLACE(lcMessagePayload,"#AMOUNT",STRING(ideAmount))
      lcMessagePayload = REPLACE(lcMessagePayload,"#INVDATE",icDate)
      lcMessagePayload = REPLACE(lcMessagePayload,"#INVNUM",STRING(iiInvNum)).

   IF lcMessagePayload NE "" AND lcMessagePayload ne ? THEN 
   RETURN lcMessagePayload.
RETURN "".
END.

FUNCTION fSpecialNotify RETURNS CHAR
   (icContent AS CHAR):
   INPUT STREAM sIn FROM VALUE(lcAddrConfDir + "/smsinvoice.sms").
   REPEAT:
      IMPORT STREAM sIn UNFORMATTED lcRecipient.
      Mm.MManMessage:mCreateMMLogSMS(lcRecipient, FALSE).
   END.
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

/* Address Conf File */
ASSIGN lcAddrConfDir = fCParamC("RepConfDir")
       lcContConFile = fCParamC("SMSInvContFile")
       /* ie. "32400-79200" Send between 9:00-22:00 */
       ldaDateFrom   = MsRequest.ReqDtParam1
       liMonth       = MONTH(ldaDateFrom)
       lcLink        = fCParamC("ESI_LinkBase")
       liTestFilter  = fCparamI("ESI_TestFilter")
       lcTestCustomers = fCparamC("ESI_TestCustomers").

INVOICE_LOOP:
FOR EACH Invoice WHERE
         Invoice.Brand    = "1" AND
         Invoice.InvDate >= ldaDateFrom AND
         Invoice.DelType =  {&INV_DEL_TYPE_ESI} AND /* 4 in testing phase */
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

      /*Ilkka test 28.3.2018*/
/*      
         IF NOT (Mobsub.CLI EQ "661473828" OR
                 Mobsub.CLI EQ "661485255" OR
                 Mobsub.CLI EQ "639152671") THEN NEXT SUBINVOICE_LOOP.
*/
      IF Mm.MManMessage:mGetMessage("SMS", "EInvMessage", 1) EQ TRUE THEN DO:
         lcTemplate = fGetSMSTxt("EInvMessage",
                                 TODAY,
                                 5,
                                 OUTPUT ldeActStamp).
 /*Check to Customer's SMS if the mobsub is not mobile.*/
         lcContactNbr = fFindSMSTarget(Mobsub.CLI).

         PUT STREAM sPilot UNFORMATTED
            STRING(Customer.Custnum) + ";" +
            STRING(Mobsub.CLI) + ";" +
            STRING(lcContactNbr) + ";" +
            STRING(Invoice.InvAmt) SKIP.

         lcTemplate = fGenerateEinvoiceTemplate(lcTemplate,
                                             MobSub.MsSeq,
                                             MobSub.CLI,
                                             Invoice.InvAmt,
                                      STRING(Invoice.DueDate),
                                             lcLink,
                                             Invoice.InvNum,
                                             liBillPeriod,
                                             Customer.Language).

         Mm.MManMessage:ParamKeyValue = lcTemplate.
         IF llFirstInv EQ FALSE THEN DO:
            fSpecialNotify(lcTemplate).
            llFirstInv = TRUE.
         END.
         Mm.MManMessage:mCreateMMLogSMS(lcContactNbr).
      END.
      ELSE DO:
      END.
      /*counter for following subinvoice handling speed with log*/
      liCounter = liCounter + 1.
      IF liCounter MOD 10000 EQ 0 THEN DO:
      PUT STREAM sPilot UNFORMATTED 
         "Count " +
         STRING(liCounter) + ";" +
         STRING(DATE(TODAY)) + " " +
         STRING(TIME,"hh:mm:ss") SKIP.
      END.
      /*Workaround to slowness: making pause connection is repoened 
        and this releases buffers*/
      IF liCounter MOD 50000 EQ 0 THEN PAUSE(120).
       
   END. /* FOR EACH SubInvoice OF Invoice NO-LOCK: */

END. /* FOR EACH Invoice WHERE */

/*notify the last message.*/
IF Mm.MManMessage:mGetMessage("SMS", "EInvMessage", 1) EQ TRUE THEN DO:
   Mm.MManMessage:ParamKeyValue = lcTemplate.
   fSpecialNotify(lcTemplate).
END.
/* Send an email to configure list*/
IF lcAddrConfDir > "" THEN
   lcAddrConfDir = lcAddrConfDir + "smsinvoice.email".

IF lcContConFile > "" AND SEARCH(lcAddrConfDir) <> ? THEN DO:
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = Syst.Var:gcBrand            AND
             InvText.Target    = "General"          AND
             InvText.KeyValue  = "EmailConfSMSInv"  AND
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
PUT STREAM sPilot UNFORMATTED "Done " + 
                               STRING(DATE(TODAY)) + " " +
                               STRING(TIME,"hh:mm:ss") SKIP.


