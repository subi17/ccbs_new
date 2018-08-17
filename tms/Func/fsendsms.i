/* ----------------------------------------------------------------------
  module .......: Func/fsendsms.i
  task .........: Trigger the SMS
  application ..: tms
  author .......: vikas
  created ......: 05.14.11
  version ......: yoigo
---------------------------------------------------------------------- */

&IF "{&fsendsms}" NE "YES" 
&THEN

&GLOBAL-DEFINE fsendsms YES

{Func/fmakesms.i}
{Func/transname.i}
{Mm/bundle_type.i}

def stream slog.
output stream slog to "/tmp/yot_5872_test.log".

FUNCTION fReplaceTags RETURNS CHARACTER(INPUT iiMsRequest   AS INTEGER,
                                        INPUT icSMSText     AS CHARACTER,
                                        INPUT icExtraParams AS CHARACTER,
                                        OUTPUT oiSMSType    AS INTEGER):

   DEFINE VARIABLE lcBundleName     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcReplacedTxt    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSMSSender      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcIPLContracts   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFLATContracts  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCONTSContracts AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCONTSFContracts AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBundleCLITypes AS CHARACTER NO-UNDO.

   DEF VAR ldaActDate   AS DATE NO-UNDO.
   DEF VAR liActTime    AS INT  NO-UNDO.

   FOR FIRST MsRequest WHERE
             MsRequest.MsRequest = iiMsRequest NO-LOCK:

       CASE MsRequest.ReqType:
          WHEN {&REQTYPE_BUNDLE_CHANGE} THEN DO:

             ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
                    lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
                    lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
                    lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS").

             /* Special conversion for BTC with upgrade upsell */
             IF INDEX(icSMSText,"#NEW_BUNDLE_CAPACITY") > 0 THEN DO:
                FIND FIRST ServiceLimit WHERE
                           ServiceLimit.GroupCode = MsRequest.ReqCParam2 AND
                           ServiceLimit.DialType  = {&DIAL_TYPE_GPRS}
                     NO-LOCK NO-ERROR.
                IF AVAILABLE ServiceLimit THEN DO:
                   IF ServiceLimit.InclAmt < 1024 THEN
                      lcReplacedTxt = STRING(ServiceLimit.InclAmt) + " MB".
                   ELSE
                      lcReplacedTxt = STRING(ServiceLimit.InclAmt / 1024) + " GB".
                END. /* IF AVAILABLE ServiceLimit THEN DO: */
                icSMSText = REPLACE(icSMSText,"#NEW_BUNDLE_CAPACITY",lcReplacedTxt).
             END. /* IF INDEX(icSMSText,"#NEW_BUNDLE_CAPACITY") > 0 */

             /* BTC to IPL/FLAT/INFINITA */
             IF LOOKUP(MsRequest.ReqCParam2,lcIPLContracts) > 0 OR
                LOOKUP(MsRequest.ReqCParam2,lcCONTSContracts) > 0 OR
                LOOKUP(MsRequest.ReqCParam2,lcFLATContracts) > 0 OR
                LOOKUP(MsRequest.ReqCParam2,lcCONTSFContracts) > 0 THEN DO:

                oiSMSType = 6.
                Func.Common:mSplitTS(MsRequest.ActStamp,OUTPUT ldaActDate,OUTPUT liActTime).

                IF INDEX(icSMSText,"#CLITYPE") > 0 OR
                   INDEX(icSMSText,"#NEW_BUNDLE") > 0 THEN DO:
                   lcReplacedTxt = fConvBundleToBillItem(INPUT MsRequest.ReqCParam2).
                   lcReplacedTxt = fGetItemName(Syst.Var:gcBrand,
                                                "BillItem",
                                                lcReplacedTxt,
                                                Customer.Language,
                                                TODAY).
                   icSMSText = REPLACE(icSMSText,"#CLITYPE",lcReplacedTxt).
                   icSMSText = REPLACE(icSMSText,"#NEW_BUNDLE",lcReplacedTxt).
                END. /* IF INDEX(icSMSText,"#CLITYPE") > 0 THEN DO: */

                IF INDEX(icSMSText,"#DATE") > 0 THEN
                   icSMSText = REPLACE(icSMSText,"#DATE",STRING(ldaActDate,"99/99/9999")).

                LEAVE.
             END. /* IF LOOKUP(MsRequest.ReqCParam2,lcIPLContracts) > 0 OR */

             oiSMSType = 10.
             lcReplacedTxt = fConvBundleToBillItem(INPUT MsRequest.ReqCParam1).
             lcReplacedTxt = fGetItemName(Syst.Var:gcBrand,
                                          "BillItem",
                                          lcReplacedTxt,
                                          Customer.Language,
                                          TODAY).
             icSMSText = REPLACE(icSMSText,"#OLD_BUNDLE",lcReplacedTxt).
      
             lcReplacedTxt = fConvBundleToBillItem(INPUT MsRequest.ReqCParam2).
             lcReplacedTxt = fGetItemName(Syst.Var:gcBrand,
                                          "BillItem",
                                          lcReplacedTxt,
                                          Customer.Language,
                                          TODAY).
             icSMSText = REPLACE(icSMSText,"#NEW_BUNDLE",lcReplacedTxt).
          END. /* WHEN {&REQTYPE_BUNDLE_CHANGE} THEN DO: */
          WHEN {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN DO:

             ASSIGN oiSMSType        = 6
                    lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

             Func.Common:mSplitTS(MsRequest.ReqDParam1,OUTPUT ldaActDate,OUTPUT liActTime).

             /* Get actual CLIType Translation name from Billing Item */
             IF LOOKUP(MsRequest.ReqCParam2,lcBundleCLITypes) > 0 AND
                MsRequest.ReqCParam5 > "" THEN DO:
                lcReplacedTxt = fConvBundleToBillItem(INPUT MsRequest.ReqCParam5).
                lcReplacedTxt = fGetItemName(Syst.Var:gcBrand,
                                             "BillItem",
                                             lcReplacedTxt,
                                             Customer.Language,
                                             TODAY).
             END. /* IF LOOKUP(MsRequest.ReqCParam2,lcBundleCLITypes) > 0 */
             ELSE
                lcReplacedTxt = fGetItemName(Syst.Var:gcBrand,
                                             "CLIType",
                                             MsRequest.ReqCParam2,
                                             Customer.Language,
                                             TODAY).
             icSMSText = REPLACE(icSMSText,"#CLITYPE", lcReplacedTxt).

             IF INDEX(icSMSText,"#DATE") > 0 THEN
                icSMSText = REPLACE(icSMSText,"#DATE",STRING(ldaActDate,"99/99/9999")).

             IF INDEX(icSMSText,"#MSISDN") > 0 THEN
                icSMSText = REPLACE(icSMSText,"#MSISDN",MsRequest.CLI).

          END. /* WHEN {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN DO: */
          WHEN {&REQTYPE_CONTRACT_ACTIVATION} OR
          WHEN {&REQTYPE_CONTRACT_TERMINATION} OR 
          WHEN {&REQTYPE_SUBSCRIPTION_CREATE} THEN
             oiSMSType = 10.

          WHEN {&REQTYPE_DSS} THEN DO:
             CASE MsRequest.ReqCParam1:
                WHEN "CREATE" OR WHEN "DELETE" THEN
                   oiSMSType = 10.
                OTHERWISE oiSMSType = 9.
             END CASE. /* CASE MsRequest.ReqCParam1: */
          END. /* WHEN {&REQTYPE_DSS} THEN DO: */

       END CASE. /* CASE MsRequest.ReqType: */
   END. /* FOR FIRST MsRequest WHERE */

   RETURN icSMSText.

END FUNCTION. /* FUNCTION fReplaceTags */

PROCEDURE pSendSMS:
   DEFINE INPUT PARAMETER iiMsSeq         AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER iiMsRequest     AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER icSMSText       AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iiSMSType       AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER icSender        AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icExtraParams   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE ldReqStamp             AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE liSMSType              AS INTEGER   NO-UNDO.
   DEF VAR lcMessage AS CHAR NO-UNDO.

   DEFINE BUFFER bMobSub    FOR MobSub.
   DEFINE BUFFER MsRequest  FOR MsRequest.
   
   IF NOT icSMSText > "" THEN RETURN.

   FIND FIRST bMobSub WHERE
              bMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bMobSub THEN RETURN.

   FIND FIRST Customer OF bMobSub NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN RETURN.

   IF Mm.MManMessage:mGetMessage("SMS",
                                 icSMSText,
                                 Customer.Language)
   THEN DO:
      IF iiMsRequest > 0
      THEN Mm.MManMessage:ParamKeyValue = fReplaceTags(INPUT iiMsRequest,
                                                       INPUT Mm.MManMessage:ParamKeyValue,
                                                       INPUT icExtraParams,
                                                       OUTPUT liSMSType).
      Mm.MManMessage:mCreateMMLogSMS(bMobSub.CLI).
      RETURN.
   END.
    /* YOT-5872 */
   put stream slog unformatted
            iiMsSeq "|"
            iiMsRequest "|"
            icSMSText "|"
            icSender "|"
            icExtraParams "|"
            Customer.Language
            skip.

   /* send SMS */
   lcMessage = fGetSMSTxt(icSMSText,
                          TODAY,
                          Customer.Language,
                          OUTPUT ldReqStamp).
   put stream slog unformatted
      lcMessage skip.
   output stream slog close.

   IF NOT lcMessage > "" THEN RETURN.

   IF iiMsRequest > 0 THEN DO:

      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsRequest = iiMsRequest NO-ERROR.
      IF NOT AVAIL MsRequest THEN RETURN.

      /* YTS-8342 */
      IF icSMSText EQ "STC_Requested" AND
        (MsRequest.ReqSource EQ {&REQUEST_SOURCE_MAIN_LINE_DEACTIVATION} OR
         MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION})
      THEN ldReqStamp = fGetSmsTS().

      lcMessage = fReplaceTags(INPUT iiMsRequest,
                               INPUT lcMessage,
                               INPUT icExtraParams,
                               OUTPUT liSMSType).
      IF NOT lcMessage > "" THEN RETURN.
   END.

   IF iiSMSType = ? THEN iiSMSType = liSMSType.

   fMakeSchedSMS2(bMobSub.CustNum,
                  bMobSub.CLI,
                  iiSMSType,
                  lcMessage,
                  ldReqStamp,
                  icSender,
                  "").

END PROCEDURE. /* PROCEDURE pSendSMS: */

&ENDIF

