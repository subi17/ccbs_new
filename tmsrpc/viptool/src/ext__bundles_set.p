/**
 * Set data bundle 
 *
 * @input  int;mandatory;subscription id
           string;mandatory;bundle id (Bono Contracts and DSS) 
           string;mandatory;status value (on,off) 
 * @output int;bundle request id
 * @Exceptions  1;MobSub not found
                2;Incorrect Bundle Id
                3;Bundle is not supported
                4;Bundle termination is not allowed
                5;Bundle already cancelled
                6;Bundle termination is not allowed since subscription has ongoing BTC with upgrade upsell
                7;Bundle already active
                8;Bundle activation is not allowed
                9;Invalid Bundle value
               10;Not enough balance
               11;DSS not allowed
               12;ERROR:Bundle request not created
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/matrix.i}

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
Syst.Var:katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId. 
Syst.Var:gcBrand = "1".
{Func/mdub.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/fsendsms.i}
{Func/dss_request.i}
{Func/fprepaidfee.i}
{Mm/fbundle.i}

DEF VAR pcParamArray  AS CHARACTER NO-UNDO.
DEF VAR pcParamStruct AS CHARACTER NO-UNDO.
DEF VAR pcParamValue  AS CHARACTER NO-UNDO.
DEF VAR pcBundleId AS CHARACTER NO-UNDO.
DEF VAR piMsSeq AS INTEGER NO-UNDO. 
DEF VAR pcActionValue AS CHARACTER NO-UNDO. 
DEF VAR liActionValue AS INTEGER NO-UNDO.  
DEF VAR liRequest AS INTEGER NO-UNDO. 
DEF VAR lcResult AS CHARACTER NO-UNDO. 
DEF VAR ldeActStamp AS DEC NO-UNDO. 
DEF VAR ldaActDate AS DATE NO-UNDO. 
DEF VAR liTime AS INT NO-UNDO. 
DEF VAR ldCurrBal AS DEC NO-UNDO.
DEF VAR llResult  AS LOG NO-UNDO.
DEF VAR ldeBundleFee        AS DEC  NO-UNDO.

DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.
DEF VAR lcPrepaidVoiceTariffs  AS CHAR NO-UNDO.
DEF VAR lcAllowedBONOContracts AS CHAR NO-UNDO.
DEF VAR lcOnlyVoiceContracts   AS CHAR NO-UNDO.
DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.
DEF VAR lcBONOContracts        AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id,"0").
pcBundleId = get_string(param_toplevel_id,"1").
pcActionValue = get_string(param_toplevel_id,"2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

ASSIGN lcPostpaidVoiceTariffs = fCParamC("POSTPAID_VOICE_TARIFFS")
       lcPrepaidVoiceTariffs  = fCParamC("PREPAID_VOICE_TARIFFS")
       lcAllowedBONOContracts = fCParamC("ALLOWED_BONO_CONTRACTS")
       lcOnlyVoiceContracts   = fCParamC("ONLY_VOICE_CONTRACTS")
       lcDataBundleCLITypes   = fCParamC("DATA_BUNDLE_BASED_CLITYPES")
       lcBONOContracts        = fCParamC("BONO_CONTRACTS").

/* currently we support only manual activation/termination for MDUB and MDUB2 */
IF LOOKUP(pcBundleId,lcBONOContracts) = 0 AND
   pcBundleId <> {&DSS}
THEN RETURN appl_err("Incorrect Bundle Id").

/* Check if subscription type is not compatible with bundle */
IF fMatrixAnalyse(Syst.Var:gcBrand,
                  "PERCONTR",
                  "PerContract;SubsTypeTo",
                  pcBundleId + ";" + MobSub.CLIType,
                  OUTPUT lcReqChar) NE 1 THEN DO:
   RETURN appl_err("Bundle is not supported").
END.
   
ldeActStamp = Func.Common:mMakeTS().

CASE pcActionValue :
   /* termination */
   WHEN "off" THEN DO: 
      /* Subscription level */
      IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO:
         /* should exist MDUB valid to the future */   
         IF (fGetActiveMDUB(INPUT "",INPUT ldNextMonthActStamp) NE pcBundleId) THEN
            RETURN appl_err("Bundle termination is not allowed").

         /* should not exist any pending request for MDUB */
         IF fPendingMDUBTermReq("") THEN
            RETURN appl_err("Bundle already cancelled").

         /* Ongoing BTC with upgrade upsell */
         IF fOngoingBTC(INPUT MobSub.MsSeq, INPUT pcBundleId, INPUT TRUE) THEN
            RETURN appl_err("Bundle termination is not allowed since " +
                            "subscription has ongoing BTC with upgrade upsell").

      END. /* IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO: */
      /* Customer level - As of now DSS only */
      ELSE DO:
         IF NOT fIsDSSActive(Mobsub.Custnum,ldNextMonthActStamp) THEN
            RETURN appl_err("Bundle termination is not allowed").

         IF fOngoingDSSTerm(Mobsub.Custnum,ldNextMonthActStamp) THEN
            RETURN appl_err("Bundle already cancelled").
      END. /* ELSE DO: */

      liActionValue = 0.
   END.

   /* activation */
   WHEN "on" THEN DO:
      /* Subscription level */
      IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO:
         /* should not exist any MDUB valid to the future */
         IF fGetActiveMDUB(INPUT "", INPUT ldeActStamp) > "" THEN
            RETURN appl_err("Bundle already active").

         /* should not exist any pending request for MDUB */
         IF LOOKUP(pcBundleId,lcAllowedBONOContracts) = 0 OR
            fPendingMDUBActReq("") THEN
            RETURN appl_err("Bundle activation is not allowed").

         /* check service package definition exist for SHAPER and HSDPA */
         IF NOT fServPackagesActive() THEN
            RETURN appl_err("Bundle activation is not allowed").
      END. /* IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO: */
      /* Customer level - As of now DSS only */
      ELSE DO:
         IF pcBundleId = {&DSS} THEN
            RETURN appl_err("DSS activation is not allowed").

         IF fIsDSSActive(Mobsub.Custnum,ldeActStamp) THEN
            RETURN appl_err("Bundle already active").

         IF fOngoingDSSAct(Mobsub.Custnum) THEN
            RETURN appl_err("Bundle activation is not allowed").
      END. /* ELSE DO: */

      liActionValue = 1. 
   END.
   OTHERWISE DO: 
     RETURN appl_err("Invalid Bundle value").
   END.
END CASE.

/* Validate Prepaid Balance before making PMDUB activation request */
IF pcBundleId = {&PMDUB} AND liActionValue = 1 THEN DO:
   ldeBundleFee = fgetPrepaidFeeAmount(pcBundleId, TODAY).
   RUN pEnoughBalance(INPUT MobSub.CLI,
                      INPUT ldeBundleFee,
                      OUTPUT llResult).
   IF llResult = FALSE THEN DO:
      RUN pSendSMS(INPUT MobSub.MsSeq, INPUT 0, INPUT pcBundleId + "BalChk",
                   INPUT 10, INPUT {&BONO8_SMS_SENDER}, INPUT "").
      RETURN appl_err("Not enough balance").
   END. /* IF llResult = FALSE THEN DO: */
END. /* IF pcBundleId = {&PMDUB} AND */

IF liActionValue = 0 THEN DO:
   Func.Common:mSplitTS(ldeActStamp, output ldaActDate, output liTime).
   ldeActStamp = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(ldaActDate),86399).
END.

IF pcBundleId = {&DSS} THEN DO:
   IF liActionValue = 0 THEN
      liRequest = fDSSRequest(MobSub.MsSeq,
                           Mobsub.Custnum,
                           "DELETE",
                           "DSS-ACCOUNT=" + STRING(Mobsub.Custnum),
                           pcBundleId,
                           ldeActStamp,
                           {&REQUEST_SOURCE_EXTERNAL_API},
                           "",
                           TRUE, /* Fees */
                           0,
                           FALSE,
                           OUTPUT lcResult).
END.
ELSE
   liRequest = fPCActionRequest(MobSub.MsSeq,
                                pcBundleId,
                                IF liActionValue = 1 THEN "act" ELSE "term",
                                ldeActStamp,
                                TRUE, /* fees */
                                {&REQUEST_SOURCE_EXTERNAL_API},
                                "",   /* creator */
                                0,    /* no father request */
                                FALSE,
                                "",
                                0,
                                0,
                                "",
                                OUTPUT lcResult).
   
IF liRequest = 0 THEN
   RETURN appl_err("ERROR:Bundle request not created; " + lcResult).

IF LOOKUP(pcBundleId,lcBONOContracts) > 0 AND liActionValue <> 1 THEN DO:
   /* Black Berry Project */
   FIND FIRST SubSer WHERE SubSer.ServCom = "BB"         AND
                           SubSer.MsSeq   = MobSub.MsSeq AND
                           SubSer.SsDate <= TODAY NO-LOCK NO-ERROR.
   IF AVAILABLE SubSer AND SubSer.SSStat = 1 THEN DO:
      IF LOOKUP(MobSub.CliType,lcPostpaidVoiceTariffs) > 0 AND
         LOOKUP(MobSub.CliType,lcDataBundleCLITypes) = 0 THEN DO:
         IF MobSub.TariffBundle = "" OR
            LOOKUP(MobSub.TariffBundle,lcOnlyVoiceContracts) > 0 THEN
            RUN pSendSMS(INPUT MobSub.MsSeq,
                         INPUT 0,
	                  INPUT "BBDeActBundPost_1",
                         INPUT 11,
                         INPUT {&BB_SMS_SENDER},
                         INPUT "").
      END. /* IF LOOKUP(MobSub.CliType,lcPostpaidVoiceTariffs) > 0 */
      ELSE IF LOOKUP(MobSub.CliType,lcPrepaidVoiceTariffs) > 0 THEN
         RUN pSendSMS(INPUT MobSub.MsSeq,
                      INPUT 0,
                      INPUT "BBDeActBundPre_1",
                      INPUT 11,
                      INPUT {&BB_SMS_SENDER},
                      INPUT "").
   END. /* IF AVAILABLE SubSer AND SubSer.SSStat = 1 THEN DO: */
   
   /* Cancel Ongoing BTC request, if deactivation request is created for bundle */
   FOR FIRST MsRequest NO-LOCK WHERE
             MsRequest.MsSeq   = MobSub.MsSeq  AND
             MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
             LOOKUP(STRING(MsRequest.ReqStatus),
                    {&REQ_INACTIVE_STATUSES}) = 0 AND
             LOOKUP(MsRequest.ReqCparam1,lcBONOContracts) > 0
       USE-INDEX MsSeq:
       fReqStatus(4,"Bundle Type Change can not be performed since " +
                    "subscriber has requested to cancel the bundle.").
       /* Send a SMS */
       RUN pSendSMS(INPUT MobSub.MsSeq, INPUT MsRequest.MsRequest,
                    INPUT "BTCBundelDeAct", INPUT 10,
                    INPUT {&UPSELL_SMS_SENDER}, INPUT "").
   END. /* FOR FIRST MsRequest NO-LOCK WHERE */
END. /* IF LOOKUP(pcBundleId,lcBONOContracts) > 0 */

add_int(response_toplevel_id, "", liRequest).

CREATE Memo.
ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = Syst.Var:gcBrand 
      Memo.HostTable = "MobSub" 
      Memo.KeyValue  = STRING(MobSub.MsSeq) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = Syst.Var:katun 
      Memo.MemoTitle = "Mobile Data Usage Bundle"
      Memo.MemoText  = "External API bundle activation/deactivation"
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoType  = "service".
 
FINALLY:
   END.

