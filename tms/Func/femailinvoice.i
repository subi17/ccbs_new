/* ----------------------------------------------------------------------
  module .......: Func/femailinvoice.i
  task .........: Email Invoice Delivery Type related functions
  application ..: tms
  author .......: vikas
  created ......: 30.03.12
  version ......: yoigo
---------------------------------------------------------------------- */

&IF "{&fEMAILINVOICE}" NE "YES" 
&THEN

&GLOBAL-DEFINE fEMAILINVOICE YES

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/aes_encrypt.i}
{Func/fgettxt.i}
{Func/fcreatereq.i}

/* Make URL encoding, because it was not supported yeat in 10.2b version */
FUNCTION fUrlEncode RETURNS CHARACTER
  (INPUT icValue AS CHARACTER,
   INPUT icEnctype AS CHARACTER) :
/****************************************************************************
Description: Encodes unsafe characters in a URL as per RFC 1738 section 2.2.
  <URL:http://ds.internic.net/rfc/rfc1738.txt>, 2.2
Input Parameters: Character string to encode, Encoding option where "query",
  "cookie", "default" or any specified string of characters are valid.
  In addition, all characters specified in the global variable lcUnSafe
  plus ASCII values 0 <= x <= 31 and 127 <= x <= 255 are considered unsafe.
Returns: Encoded string  (unkown value is returned as blank)
Variables: lcUnSafe, lcReserved
****************************************************************************/
  DEFINE VARIABLE hx          AS CHARACTER NO-UNDO INITIAL "0123456789ABCDEF":U.
  DEFINE VARIABLE encode-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE c           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lcUnSafe AS CHAR NO-UNDO INIT "/,=".
  DEFINE VARIABLE lcReserved AS CHAR NO-UNDO.


  /* Dont bother with blank or unknown  */
  IF LENGTH(icValue) = 0 OR icValue = ? THEN
    RETURN "".

  /* What kind of encoding should be used? */
  CASE icEnctype:
    WHEN "query":U THEN              /* QUERY_STRING name=value parts */
      encode-list = lcUnSafe + lcReserved + "+":U.
    WHEN "cookie":U THEN             /* Persistent Cookies */
      encode-list = lcUnSafe + " ,~;":U.
    WHEN "default":U OR WHEN "" THEN /* Standard URL encoding */
      encode-list = lcUnSafe.
    OTHERWISE
      encode-list = lcUnSafe + icEnctype.   /* user specified ... */
  END CASE.

  /* Loop through entire input string */
  ASSIGN i = 0.
  DO WHILE TRUE:
    ASSIGN
      i = i + 1
      /* ASCII value of character using single byte codepage */
      c = ASC(SUBSTRING(icValue, i, 1, "RAW":U), "1252":U, "1252":U).
    IF c <= 31 OR c >= 127 OR INDEX(encode-list, CHR(c)) > 0 THEN DO:
      /* Replace character with %hh hexidecimal triplet */
      SUBSTRING(icValue, i, 1, "RAW":U) =
        "%":U +
        SUBSTRING(hx, INTEGER(TRUNCATE(c / 16, 0)) + 1, 1, "RAW":U) + /* high */
        SUBSTRING(hx, c MODULO 16 + 1, 1, "RAW":U).             /* low digit */
      ASSIGN i = i + 2.   /* skip over hex triplet just inserted */
    END.
    IF i = LENGTH(icValue,"RAW":U) THEN LEAVE.
  END.

  RETURN icValue.
END FUNCTION.  /* furl-encode */

FUNCTION fPendingEmailActRequest RETURNS LOG (INPUT iiCustnum AS INT):
   
   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                       MsRequest.Brand = Syst.Var:gcBrand                    AND
                       MsRequest.ReqType = {&REQTYPE_ACTIVATE_EMAIL_INVOICE} AND
                       MsRequest.Custnum = iiCustnum                AND
   LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0).

   RETURN llExist.
   
END FUNCTION. /* FUNCTION fPendingEmailActRequest */


FUNCTION fCancelPendingEmailActRequest RETURNS LOG (INPUT iiCustnum AS INT,
                                                    INPUT icMemo    AS CHAR):

   DEF BUFFER bMsRequest FOR MsRequest.

   FIND FIRST bMsRequest WHERE
              bMsRequest.Brand = Syst.Var:gcBrand                    AND
              bMsRequest.ReqType = {&REQTYPE_ACTIVATE_EMAIL_INVOICE} AND
              bMsRequest.Custnum = iiCustnum                AND
        LOOKUP(STRING(bMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
        EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL bMsRequest THEN
      ASSIGN bMsRequest.ReqStatus   = {&REQUEST_STATUS_CANCELLED}
             bMsRequest.UpdateStamp = Func.Common:mMakeTS()
             bMsRequest.DoneStamp   = Func.Common:mMakeTS()
             bMsRequest.Memo        = bMsRequest.Memo + 
                                      (IF bMsRequest.Memo > ""
                                       THEN ", " ELSE "") + icMemo.
   RETURN TRUE.
   
END FUNCTION. /* FUNCTION fPendingEmailActRequest */

FUNCTION fGenerateEmailActLink RETURNS CHAR (INPUT iiRequestId AS INT,
                                             INPUT iiCustnum   AS INT,
                                             INPUT icCustEmail AS CHAR):
   
   DEF VAR lcEncodedLink    AS CHAR NO-UNDO.
   DEF VAR lcWebServerPath  AS CHAR NO-UNDO.
   DEF VAR lcSaltKey        AS CHAR NO-UNDO.

   ASSIGN lcWebServerPath = fCParam("EI","WebServerEmailLink")
          lcSaltKey       = fCParam("EI","SaltKey").

   IF lcWebServerPath = "" OR lcWebServerPath = ? THEN RETURN "".

   IF lcSaltKey = "" OR lcSaltKey = ? THEN RETURN "".

   icCustEmail = icCustEmail + STRING(iiCustnum).

   lcEncodedLink = HEX-ENCODE(SHA1-DIGEST(icCustEmail,lcSaltKey)).

   lcEncodedLink = lcWebServerPath + "?id=" + STRING(iiRequestId) +
                   "&h=" + lcEncodedLink.

   RETURN lcEncodedLink.
   
END FUNCTION. /* FUNCTION fGenerateEmailActLink */

FUNCTION fGenerateEmailPDFLink RETURNS CHAR (INPUT iiPeriod  AS INT,
                                             INPUT iiCustnum AS INT,
                                             INPUT iiInvNum  AS INT):
   
   DEF VAR lcEncodedLink    AS CHAR NO-UNDO.
   DEF VAR lcWebServerPath  AS CHAR NO-UNDO.
   DEF VAR lcSaltKey        AS CHAR NO-UNDO.

   ASSIGN lcWebServerPath = fCParam("EI","WebServerPDFLink")
          lcSaltKey       = fCParam("EI","SaltKey").

   IF lcWebServerPath = "" OR lcWebServerPath = ? THEN RETURN "".

   IF lcSaltKey = "" OR lcSaltKey = ? THEN RETURN "".

   lcEncodedLink = STRING(iiInvNum) + STRING(iiCustnum).

   lcEncodedLink = HEX-ENCODE(SHA1-DIGEST(lcEncodedLink,lcSaltKey)).

   lcEncodedLink = lcWebServerPath + "?p=" + STRING(iiPeriod) +
                   "&h=" + lcEncodedLink.

   RETURN lcEncodedLink.
   
END FUNCTION. /* FUNCTION fGenerateEmailPDFLink */

FUNCTION fGenerateQ25Link RETURNS CHAR (INPUT icCLI AS CHAR):
   
   DEF VAR lcEncryptedCLI AS CHAR NO-UNDO.
   DEF VAR lcQ25Link      AS CHAR NO-UNDO.
   DEF VAR lcSaltKey      AS CHAR NO-UNDO.

   ASSIGN 
      lcQ25Link = fCParam("EI","WebServerQ25LPLink")
      lcSaltKey = fCParam("Q25","Q25PassPhrase").
   IF lcSaltKey = "" OR lcSaltKey = ? THEN 
      lcSaltKey = {&Q25_PASSPHRASE}.

   IF lcQ25Link = "" OR lcQ25Link = ? THEN RETURN "".
   IF lcSaltKey = "" OR lcSaltKey = ? THEN RETURN "".

   IF icCLI > "" THEN DO:
      lcEncryptedCLI = encrypt_data(icCLI,
                                   {&ENCRYPTION_METHOD}, 
                                   lcSaltKey).
      /* convert some special characters to url encoding (at least '+' char
         could cause problems at later phases. */
      lcEncryptedCLI = fUrlEncode(lcEncryptedCLI, "query").
   END.
   
   lcQ25Link = REPLACE(lcQ25Link,"#CLI",lcEncryptedCLI).

   RETURN lcQ25Link.
   
END FUNCTION. /* FUNCTION fGenerateEmailPDFLink */

FUNCTION fGetSMSText RETURNS CHAR (INPUT icTarget    AS CHAR,
                                   INPUT icKeyValue  AS CHAR,
                                   INPUT iiLanguage  AS INT):
   
   DEF VAR lcSMSText AS CHAR NO-UNDO.

   DO WHILE TRUE:
      lcSMSText = fGetTxt(icTarget,
                          icKeyValue,
                          TODAY,
                          iiLanguage).

      /* use spanish if text not available for another language */
      IF lcSMSText = "" AND iiLanguage NE 1 THEN DO:
         iiLanguage = 1.
         NEXT.
      END.
      LEAVE.
   END. /* DO WHILE TRUE: */

   RETURN lcSMSText.
   
END FUNCTION. /* FUNCTION fGetSMSText */

FUNCTION fGetEmailText RETURNS CHAR (INPUT icTarget    AS CHAR,
                                     INPUT icKeyValue  AS CHAR,
                                     INPUT iiLanguage  AS INT,
                                     OUTPUT ocEmailSub AS CHAR):
   
   DEF VAR lcEmailText AS CHAR NO-UNDO.

   DO WHILE TRUE:
      FOR FIRST InvText NO-LOCK WHERE 
                InvText.Brand     = Syst.Var:gcBrand      AND
                InvText.Target    = icTarget     AND
                InvText.KeyValue  = icKeyValue   AND
                InvText.FromDate <= TODAY AND
                InvText.ToDate   >= TODAY AND
                InvText.Language  = iiLanguage:
         ASSIGN lcEmailText = InvText.InvText
                ocEmailSub  = InvText.TxtTitle.
      END. /* FOR FIRST InvText NO-LOCK WHERE */

      /* use spanish if text not available for another language */ 
      IF lcEmailText = "" AND iiLanguage NE 1 THEN DO:
         iiLanguage = 1.
         NEXT.
      END.
      LEAVE.
   END. /* DO WHILE TRUE: */

   RETURN lcEmailText.
   
END FUNCTION. /* FUNCTION fGetSMSText */

FUNCTION fEmailInvoiceRequest RETURNS INTEGER

   (INPUT idActStamp     AS DEC,    /* when request should be handled */
    INPUT idActDate      AS DATE,   /* when request should be handled */
    INPUT icUserCode     AS CHAR,   /* user code */
    INPUT iiMsSeq        AS INT,    /* MobSub  sequence */
    INPUT icCLI          AS CHAR,   /* mobsub CLI */
    INPUT iiCustNum      AS INT,    /* customer number */
    INPUT icSource       AS CHAR,
    INPUT icEmailAddress AS CHAR,   /* Email Address */
    INPUT iiOrder        AS INT,   /* Order ID */
    OUTPUT ocResult      AS CHAR):

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF BUFFER Order FOR Order.

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN
      idActStamp = Func.Common:mMakeTS().

   idActStamp = Func.Common:mSecOffSet(idActStamp,120). /* 2 mins delay */

   ocResult = fChkRequest(iiCustNum,
                          {&REQTYPE_ACTIVATE_EMAIL_INVOICE},
                          "EmailInvoice",
                          icUserCode).
   IF ocResult > "" THEN RETURN 0.

   IF iiorder > 0 THEN
      FIND FIRST Order NO-LOCK WHERE Order.Brand = Syst.Var:gcBrand AND
                                     Order.OrderId = iiOrder NO-ERROR.

   /* If Email address is already validated then no need to send again */
   IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                      MsRequest.Brand = Syst.Var:gcBrand AND
                      MsRequest.ReqType = {&REQTYPE_ACTIVATE_EMAIL_INVOICE} AND
                      MsRequest.Custnum = iiCustnum                AND
                      MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
                      MsRequest.ReqCparam1 = icEmailAddress) THEN RETURN 1.
   /* If customer does not have any postpaid subscriptions, no email needed */
   /* iiOrder can come as 0 from other request than new customer creation */
   IF (iiOrder = 0 OR Order.Paytype) AND 
      NOT (CAN-FIND (FIRST Mobsub NO-LOCK WHERE
                           Mobsub.Brand = Syst.Var:gcBrand AND
                           Mobsub.Custnum = iiCustNum AND
                           Mobsub.paytype = FALSE)) THEN RETURN 1.
   fCreateRequest({&REQTYPE_ACTIVATE_EMAIL_INVOICE},
                  idActStamp,
                  icUserCode,
                  FALSE,    /* create fees */
                  FALSE).   /* sms         */

   ASSIGN
      bCreaReq.CLI          = icCLI
      bCreaReq.MsSeq        = iiMsSeq
      bCreaReq.CustNum      = iiCustNum
      bCreaReq.ReqDtParam1  = idActDate
      bCreaReq.ReqCParam1   = icEmailAddress
      bCreaReq.ReqSource    = icSource
      liReqCreated          = bCreaReq.MsRequest.

   RELEASE bCreaReq.

   RETURN liReqCreated.

END FUNCTION. /* FUNCTION fEmailInvoiceRequest */


FUNCTION feInvoiceValidate RETURNS LOGICAL(INPUT idaPeriod AS DATE,
                                           OUTPUT ocresult AS CHAR):

   DEF VAR liMonth         AS INT NO-UNDO. 
   DEF VAR liYear          AS INT NO-UNDO. 
   DEF VAR ldeCurrentMonth AS DEC NO-UNDO. 
   DEF VAR ldeNextMonth    AS DEC NO-UNDO. 
   
   IF NOT CAN-FIND(FIRST ActionLog WHERE
                         ActionLog.Brand    = Syst.Var:gcBrand AND
                         ActionLog.ActionID = "PRINTINVXML" AND
                         ActionLog.ActionPeriod = YEAR(idaPeriod) * 100 +
                                                  MONTH(idaPeriod)
                         NO-LOCK) THEN
      ocResult = "Invoice XML files for current month are not created".
   ELSE DO:
      ASSIGN liMonth = MONTH(idaPeriod) + 1
             liYear = YEAR(idaPeriod).

      IF liMonth = 13 THEN
         ASSIGN liMonth = 1
                liYear = liYear + 1.

      ASSIGN ldeCurrentMonth = YEAR(idaPeriod) * 10000 +
                               MONTH(idaPeriod) * 100 + 1
             ldeNextMonth = liYear * 10000 + liMonth * 100 + 1.

      FIND FIRST MsRequest WHERE
                 MsRequest.Brand    = Syst.Var:gcBrand AND
                 MsRequest.ReqType  = {&REQTYPE_SEND_EMAIL_INVOICE} AND
                 MsRequest.ActStamp > ldeCurrentMonth AND
                 MsRequest.ActStamp < ldeNextMonth AND
                 LOOKUP(STRING(MsRequest.ReqStatus),"0,1,2,3") > 0
           NO-LOCK NO-ERROR.
      IF AVAIL MsRequest THEN
         ocResult = "Email invoice request for current month is ongoing or done".
   END. /* ELSE DO: */

   RETURN ocresult EQ "". 

END FUNCTION. /* FUNCTION feInvoiceValidate */

FUNCTION fSendeInvoiceRequest RETURNS INTEGER
   (INPUT  idactstamp  AS DEC,
    INPUT  idaPeriod   AS DATE,
    INPUT  iccreator   AS CHAR,
    INPUT  icsource    AS CHAR,
    OUTPUT ocresult    AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.

   IF NOT feInvoiceValidate(INPUT idaPeriod,
                            OUTPUT ocresult)
   THEN RETURN 0.

   fCreateRequest(({&REQTYPE_SEND_EMAIL_INVOICE}),
                  idactstamp,
                  iccreator,
                  FALSE,      /* fees */
                  FALSE).    /* send sms */

   ASSIGN bCreaReq.ReqSource   = icsource
          bCreaReq.ReqdtParam1 = idaPeriod
          liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION. /* FUNCTION fSendeInvoiceRequest */


&ENDIF
