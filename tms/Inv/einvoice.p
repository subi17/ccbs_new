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

/* From Progress documentation
   https://knowledgebase.progress.com/articles/Article/000043232
*/
FUNCTION BinaryXOR RETURNS INTEGER
    (INPUT intOperand1 AS INTEGER,
    INPUT intOperand2 AS INTEGER):

    DEFINE VARIABLE iByteLoop  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iXOResult  AS INTEGER NO-UNDO.
    DEFINE VARIABLE lFirstBit  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecondBit AS LOGICAL NO-UNDO.

    iXOResult = 0.

    /*spin through each byte of each char*/
    DO iByteLoop = 0 TO 7: /* as processing a single byte character */

        /*find state (true / false) of each integers byte*/
        ASSIGN
            lFirstBit  = GET-BITS(intOperand1,iByteLoop + 1,1) = 1
            lSecondBit = GET-BITS(intOperand2,iByteLoop + 1,1) = 1.

        /* XOR each bit*/
        IF (lFirstBit AND NOT lSecondBit) OR
            (lSecondBit AND NOT lFirstBit) THEN
            iXOResult = iXOResult + EXP(2, iByteLoop).
    END.
 
    RETURN iXOResult.
END FUNCTION. /*End function of BinaryXOR */

/* From Progress documentation
   https://knowledgebase.progress.com/articles/Article/000043232
 */
FUNCTION HMAC-BASE64 RETURN CHARACTER 
    (INPUT pcSHA AS CHARACTER,
    INPUT pcKey AS CHARACTER, 
    INPUT pcData AS CHARACTER):

    DEFINE VARIABLE mKeyOpad       AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE mKeyIpad       AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE mData          AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE mKey           AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE mInnerCombined AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE mOuterCombined AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE iBytePos       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOpad          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIpad          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iKey           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTimeTaken     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rRawDataSHA    AS RAW       NO-UNDO.
    DEFINE VARIABLE cHMACSHA       AS CHARACTER NO-UNDO.
    
    &SCOPED-DEFINE xiBlockSize  64
    
    SET-SIZE(mKey)     = 0.
    SET-SIZE(mKeyOpad) = 0.
    SET-SIZE(mKeyIpad) = 0.
    SET-SIZE(mKey)     = {&xiBlockSize}.
    SET-SIZE(mKeyOpad) = {&xiBlockSize}.
    SET-SIZE(mKeyIpad) = {&xiBlockSize}.
    
    DO iBytePos = 1 TO {&xiBlockSize}:
        PUT-BYTES(mKey,     iBytePos) = HEX-DECODE("00":U).  /* 64 bytes of zeros 0x00*/
        PUT-BYTES(mKeyOpad, iBytePos) = HEX-DECODE("5C":U).  /* 64 bytes of 0x5C (92 dec,  "/" ascii) */
        PUT-BYTES(mKeyIpad, iBytePos) = HEX-DECODE("36":U).  /* 64 bytes of 0x36 (54 dec, "6" ascii)*/
    END.
    
    /* correction by Valery A.Eliseev */
    IF LENGTH(pcKey) > {&xiBlockSize} THEN 
    DO:
        set-size(mData) = LENGTH(pcKey).
        put-string(mData, 1, LENGTH(pcKey)) = pcKey.
        rRawDataSHA = SHA1-DIGEST(mData).
        PUT-BYTES(mKey, 1) = rRawDataSHA.
    END.
    ELSE
        /* end of correction */
    
        PUT-STRING(mKey, 1, LENGTH(pckey))  = pcKey. 
    
    DO iBytePos = 1 TO {&xiBlockSize}:
      
        ASSIGN
            iKey  = GET-BYTE(mKey,     iBytePos)
            iOpad = GET-BYTE(mKeyOpad, iBytePos)
            iIpad = GET-BYTE(mKeyIpad, iBytePos).
      
        /* The inner key, mKeyIpad is formed from mKey by XORing each byte with 0x36.. */
        PUT-BYTE(mKeyIpad, iBytePos) = BinaryXOR(INPUT iKey, 
            INPUT iIpad).
    
        /* The inner key, mKeyOpad is formed from mKey by XORing each byte with 0x5C. */
        PUT-BYTE(mKeyOpad, iBytePos) = BinaryXOR(INPUT iKey, 
            INPUT iOpad).
    
    END.
    
    SET-SIZE(mKey)  = 0.
    SET-SIZE(mData) = 0.
    SET-SIZE(mData) = LENGTH(pcData).
    PUT-STRING(mData,1,LENGTH(pcData)) = pcData.
    
    
    /* Inner Loop*/
    SET-SIZE(mInnerCombined)      = GET-SIZE(mKeyIpad) + GET-SIZE(mData).
    
    PUT-BYTES(mInnerCombined, 1)  = mKeyIpad.
    SET-SIZE(mKeyIpad) = 0.
    
    /*Append the data the end of the block size.*/
    PUT-BYTES(mInnerCombined, {&xiBlockSize} + 1) = mData.
    
    /* Deallocates any memory. */
    SET-SIZE(mData) = 0.
    
    /* Get the results of the SHA Digest.*/
    CASE pcSHA:
        WHEN 'SHA1' THEN
            ASSIGN
                rRawDataSHA = SHA1-DIGEST(mInnerCombined).
        WHEN 'SHA-256' THEN
            ASSIGN
                rRawDataSHA = MESSAGE-DIGEST('SHA-256', mInnerCombined).
        OTHERWISE 
        ASSIGN
            rRawDataSHA = SHA1-DIGEST(mInnerCombined).
    END CASE.
                                     
    /* Deallocates any memory. */
    SET-SIZE(mInnerCombined) = 0.
    
    /* Outer Loop calculation for SHA*/
    SET-SIZE(mOuterCombined)                      = 0.
    SET-SIZE(mOuterCombined)                      = GET-SIZE(mKeyOpad) + LENGTH(rRawDataSHA,'RAW':U).
    PUT-BYTES(mOuterCombined, 1)                  = mKeyOpad.
    PUT-BYTES(mOuterCombined, {&xiBlockSize} + 1) = rRawDataSHA.
    
    /* SHA*/
    CASE pcSHA:
        WHEN 'SHA1' THEN
            ASSIGN
                rRawDataSHA = SHA1-DIGEST(mOuterCombined).
        WHEN 'SHA-256' THEN
            ASSIGN
                rRawDataSHA = MESSAGE-DIGEST('SHA-256', mOuterCombined).
        OTHERWISE 
        ASSIGN
            rRawDataSHA = SHA1-DIGEST(mOuterCombined).
    END CASE.
    
    /* Deallocates any memory. */
    SET-SIZE(mKeyOpad)       = 0.
    SET-SIZE(mOuterCombined) = 0.
    
    /* Convert the raw binary results into a human readable BASE-64 value.*/
    cHMACSHA = BASE64-ENCODE(rRawDataSHA).
    
    &UNDEFINE xiBlockSize
    RETURN cHMACSHA.
END FUNCTION. /** End Of Function HMACSHA1-BASE64 */

/* Function fShortCrypt 
   Function calculates a crypted part of key that stores information for 
   opening correct ESI page.
   Crypted part is combined of MSISDN + part of billing period
   Idea:
      MSISDN and part of billing period is put to inter variable (10 base digit)
      Secure key is taken from this number (5 first digits).
      The "10 base digit" is converted/compressed to shorter format by using "bigger base digit"
      Secure key and compressed parts are combined. 
      
      Decoding operation is done in ESI Landing Page
*/
FUNCTION fShortCrypt RETURNS CHAR
   (icMSISDN AS CHAR,
    iiPeriod AS INT):
   DEF VAR lcChSet AS CHAR NO-UNDO.  /* CharSet of out string */
   DEF VAR lcNum AS CHAR NO-UNDO.    /* Temp variable */
   DEF VAR liNum AS INT64 NO-UNDO.   /* long int to be converted to new set */
   DEF VAR liBase AS INT64 NO-UNDO.  /* base: how many chars are in the new char set */
   DEF VAR liRem AS INT64 NO-UNDO.   /* remainder in modulo */
   DEF VAR lcCharInPosition AS CHAR NO-UNDO. /* corresponding char in chset */
   DEF VAR lcOut AS CHAR NO-UNDO.    /* Result */
   /*Support variables for creating security part */
   DEF VAR liLen AS INT NO-UNDO.
   DEF VAR lcDigest AS CHAR NO-UNDO. /* Key to support crypting */
   DEF VAR lcAlpha AS CHAR NO-UNDO.
   DEF VAR lcDigestPart AS CHAR NO-UNDO.
   DEF VAR liPosition AS INT NO-UNDO.

   /* Combine input to integer variable */
   lcNum = icMSISDN + SUBSTRING(STRING(iiPeriod),3). /* 201806 -> 1806 */
   liNum = INT64(lcNum).
   lcChSet = "kD0EFGHI1Zz5fghijlmnoAYqrstNOPQRS987cdepuvwxTUVW2346abyBCJKLMX".
   
   /* Calculate secure key */
   /* Do not chhange this without agreeing with Landing Page side */
   lcDigest = HMAC-BASE64("SHA-256", "sz009_23#opleK_NaRv0q", lcNum).

   /* Transfer MSISDN+PERIOD to shorter format */
   /* Number system works: 123 = 3*10^0 + 2*10^1 + 1*10^2*  */
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

   /* Calculete security part - 5 first suitable digits from digest*/
   /* If there are not enough alphabets then assign A */
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

   /* combine and return final result */  
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

      
      IF Mobsub.CLI EQ "" OR
         Mobsub.CLI EQ ? THEN NEXT SUBINVOICE_LOOP.

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


