&IF "{&FUSIONINVOICE_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE FUSIONINVOICE_I YES
/* ----------------------------------------------------------------------
  module .......: fusioninvoice.i
  task .........: Fusion invoice/email related functions
  application ..: tms
  author .......: anttis
  created ......: 02.12.13
  version ......: yoigo
---------------------------------------------------------------------- */
{commali.i}
{tmsconst.i}
{date.i}
{cparam2.i}
{timestamp.i}
{fcreatereq.i}
{msreqfunc.i}
{fgettxt.i}

&GLOBAL-DEFINE FI_MAPPING_YOIGO_AND_TF 1
&GLOBAL-DEFINE FI_MAPPING_YOIGO 2
&GLOBAL-DEFINE FI_MAPPING_TF 3

&GLOBAL-DEFINE FI_DELIVERY_STATE_NEW 0
&GLOBAL-DEFINE FI_DELIVERY_STATE_SMS 1
&GLOBAL-DEFINE FI_DELIVERY_STATE_EMAIL 2
&GLOBAL-DEFINE FI_DELIVERY_STATE_CANCELLED 3

FUNCTION fPendingFusionEmailRequest RETURNS LOGICAL
   (INPUT idaPeriod AS DATE):

   DEF VAR ldaNextMonth AS DATE NO-UNDO. 
   DEF VAR ldeNextMonth AS DEC NO-UNDO. 
   DEF VAR ldeCurrentMonth AS DEC NO-UNDO. 
   
   ldaNextMonth = fLastDayOfMonth(idaPeriod) + 1.
   ASSIGN ldeCurrentMonth = fMake2Dt(idaPeriod,0)
          ldeNextMonth = fMake2Dt(ldaNextMonth,0).

   RETURN CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                         MsRequest.Brand    = gcBrand AND
                         MsRequest.ReqType  = {&REQTYPE_FUSION_EMAIL} AND
                         MsRequest.ActStamp > ldeCurrentMonth AND
                         MsRequest.ActStamp < ldeNextMonth AND
                         MsRequest.ReqStatus NE 4).

END FUNCTION.

FUNCTION fFusionEmailValidate RETURNS LOGICAL(INPUT idaPeriod AS DATE,
                                           OUTPUT ocresult AS CHAR):

   DEF VAR liPeriod AS INT NO-UNDO. 
   DEF VAR ldeCurrentMonth AS DEC NO-UNDO. 
   DEF VAR ldaNextMonth AS DATE NO-UNDO. 
   DEF VAR ldeNextMonth AS DEC NO-UNDO. 

   IF NOT CAN-FIND(FIRST ActionLog NO-LOCK WHERE
                         ActionLog.Brand    = gcBrand AND
                         ActionLog.ActionID = "TELEFONICA" AND
                         ActionLog.ActionPeriod = YEAR(idaPeriod) * 100 +
                                                  MONTH(idaPeriod) AND
                         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}) THEN
      ocResult = "Telefonica file not processed".
   ELSE IF fPendingFusionEmailRequest(idaPeriod) THEN
      ocResult = "Fusion email sending request already exists".

   RETURN ocresult EQ "". 

END FUNCTION. /* FUNCTION feInvoiceValidate */

FUNCTION fFusionEmailRequest RETURNS INTEGER
   (INPUT  idactstamp  AS DEC,
    INPUT  iccreator   AS CHAR,
    INPUT  icsource    AS CHAR,
    OUTPUT ocresult    AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF VAR ldaPeriod AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO. 

   fSplitTS(idactstamp, OUTPUT ldaPeriod, OUTPUT liTime).
   ldaPeriod = DATE(MONTH(ldaPeriod),1,YEAR(ldaPeriod)).

   IF NOT fFusionEmailValidate(ldaPeriod,
                               OUTPUT ocresult)
   THEN RETURN 0.

   fCreateRequest(({&REQTYPE_FUSION_EMAIL}),
                  idactstamp,
                  iccreator,
                  FALSE,      /* fees */
                  FALSE).    /* send sms */

   ASSIGN bCreaReq.ReqSource   = icsource
          bCreaReq.ReqdtParam1 = ldaPeriod
          liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION. /* FUNCTION fSendeInvoiceRequest */

FUNCTION fGenerateFusionInvoiceHash RETURNS CHAR
   (INPUT iiInvNum AS INT,
    INPUT icInvoiceNum AS CHAR,
    INPUT icCustomerID AS CHAR):
   
   DEF VAR lcHash           AS CHAR NO-UNDO.   
   DEF VAR lcSaltKey        AS CHAR NO-UNDO.

   lcSaltKey = fCParam("EI","SaltKey").
   IF lcSaltKey = "" OR lcSaltKey = ? THEN RETURN "".

   lcHash = STRING(iiInvNum) +
            STRING(icInvoiceNum) +
            STRING(icCustomerID).

   lcHash = HEX-ENCODE(SHA1-DIGEST(lcHash,lcSaltKey)).

   RETURN lcHash.
END.

FUNCTION fGenerateFusionInvoiceLink RETURNS CHAR 
   (INPUT iiFuInvnum AS INT,
    INPUT icHash AS CHAR):
      
   DEF VAR lcEncodedLink    AS CHAR NO-UNDO.
   DEF VAR lcWebServerPath  AS CHAR NO-UNDO.
   
   lcWebServerPath = fCParam("EIF","WebServerLink").
   IF lcWebServerPath = "" OR lcWebServerPath = ? THEN RETURN "".

   lcEncodedLink = lcWebServerPath + STRING(iiFuInvNum) + "?hash=" + icHash.

   RETURN lcEncodedLink.
   
END FUNCTION.
   
FUNCTION fFillFusionEmailText RETURNS CHAR
   (BUFFER FusionInvoice FOR FusionInvoice,
    INPUT icEmailText AS CHAR,
    INPUT icSenderAddress AS CHAR,
    OUTPUT ocError AS CHAR):

   DEF VAR lcHash AS CHAR NO-UNDO. 
   DEF VAR lcEmailLink AS CHAR NO-UNDO. 
   DEF VAR lcMonth AS CHAR NO-UNDO. 
   DEF VAR ldaDatePrev AS DATE NO-UNDO.
      
   lcHash = fGenerateFusionInvoiceHash(
               FusionInvoice.InvNum,
               FusionInvoice.InvoiceNum,
               FusionInvoice.CustomerID).

   IF lcHash EQ "" OR lcHash EQ ? THEN DO:
      ocError = "ERROR:Fusion invoice hash creation failed".
      RETURN "".
   END.

   /* Replace the Tokens and generate the links */
   lcEmailLink = fGenerateFusionInvoiceLink(
      FusionInvoice.FuInvNum,
      lcHash).

   IF lcEmailLink = "" OR lcEmailLink = ? THEN DO:
      ocError = "ERROR:Email link creation failed".
      RETURN "".
   END.


   ASSIGN ldaDatePrev = ADD-INTERVAL(FusionInvoice.InvDate,
                                   -1,
                                   "months").
                                   
   ASSIGN icEmailText = REPLACE(icEmailText,"#LINK",lcEmailLink)
          icEmailText = REPLACE(icEmailText,"#YEAR",
                        STRING(YEAR(ldaDatePrev)))
          lcMonth     = ENTRY(MONTH(ldaDatePrev),{&MONTHS_ES})
          icEmailText = REPLACE(icEmailText,"#MONTH",lcMonth)
          icEmailText = REPLACE(icEmailText,"#SENDER",icSenderAddress).

   RETURN icEmailText.
END.

&ENDIF
