/**
 * Set data bundle 
 *
 * @input  transaction_id;string;mandatory;transaction id
           msisdn;string;mandatory;subscription msisdn number
           bundle_id;string;mandatory;bundle id (eg: BONO Contracts/DSS) 
           bundle_status;string;mandatory;status value (on,off) 
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions  1;Subscription not found
                2;Incorrect Bundle Id
                3;Bundle is not allowed for this subscription type
                4;Bundle termination is not allowed
                5;Bundle already cancelled
                6;Bundle already active
                7;Bundle activation is not allowed
                8;Invalid Bundle value
                9;Not enough balance
               10;DSS not allowed
               11;Bundle request not created
               12;Application Id does not match
               13;Bundle termination is not allowed since subscription has ongoing BTC with upgrade upsell

 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/matrix.i}

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
Syst.Var:katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId.
Syst.Var:gcBrand = "1".
{Func/mdub.i}
{Syst/tmsconst.i}
{Func/msreqfunc.i}
{Func/fsendsms.i}
{Func/dss_request.i}
{Func/fexternalapi.i}
{Func/fprepaidfee.i}
{Mm/fbundle.i}

DEF VAR pcBundleId          AS CHAR NO-UNDO.
DEF VAR pcCLI               AS CHAR NO-UNDO.
DEF VAR pcTransId           AS CHAR NO-UNDO.
DEF VAR top_struct          AS CHAR NO-UNDO.
DEF VAR pcActionValue       AS CHAR NO-UNDO. 
DEF VAR liActionValue       AS INT  NO-UNDO.  
DEF VAR liRequest           AS INT  NO-UNDO. 
DEF VAR lcResult            AS CHAR NO-UNDO. 
DEF VAR ldeActStamp         AS DEC  NO-UNDO. 
DEF VAR ldaActDate          AS DATE NO-UNDO. 
DEF VAR liTime              AS INT  NO-UNDO. 
DEF VAR ldCurrBal           AS DEC  NO-UNDO.
DEF VAR llResult            AS LOG  NO-UNDO.
DEF VAR lcBONOContracts     AS CHAR NO-UNDO.

DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.
DEF VAR lcPrepaidVoiceTariffs  AS CHAR NO-UNDO.
DEF VAR lcAllowedBONOContracts AS CHAR NO-UNDO.
DEF VAR lcOnlyVoiceContracts   AS CHAR NO-UNDO.
DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.
DEF VAR ldeBundleFee           AS DEC  NO-UNDO.
DEF VAR lcApplicationId        AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId         AS CHAR NO-UNDO.
DEF VAR lcOnOff                AS CHAR NO-UNDO.
DEF VAR secondsFromPrevious    AS INT  NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string,string") EQ ? THEN RETURN.

ASSIGN pcTransId = get_string(param_toplevel_id, "0")
       pcCLI     = get_string(param_toplevel_id,"1")
       pcBundleId = get_string(param_toplevel_id,"2")
       pcActionValue = get_string(param_toplevel_id,"3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{selfservice/src/findtenant.i NO ordercanal MobSub Cli pcCLI}

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = ghAuthLog::EndUserId.

IF NOT fchkTMSCodeValues(ghAuthLog::UserName,lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

Syst.Var:katun = lcApplicationId + "_" + ghAuthLog::EndUserId.

/*YPR-4775*/
/*(De)Activation is not allowed if fixed line provisioning is pending*/
IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG}    /*16*/ OR 
    MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) /*17*/ THEN
   RETURN appl_err("Mobile line provisioning is not complete").


/* YDR-1783 Check that previous request is not done during
   previous five minutes from external api */
IF CAN-FIND( FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq = Mobsub.MsSeq AND
                   MsRequest.ActStamp > Func.Common:mSecOffSet(Func.Common:mMakeTS(),-300) AND
                   MsRequest.ReqType = 8 AND
                   MsRequest.ReqCParam3 = pcBundleId AND
                   MsRequest.ReqSource = {&REQUEST_SOURCE_EXTERNAL_API} 
                   USE-INDEX MsActStamp) THEN
   RETURN appl_err("The requested activation was not handled because " +
                   "there is a too recent activation.").

lcBONOContracts = fCParamC("BONO_CONTRACTS").

FIND FIRST DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = Syst.Var:gcBrand AND
           DayCampaign.DCEvent = pcBundleId NO-ERROR.
IF NOT AVAIL DayCampaign THEN RETURN appl_err("DayCampaign not defined").

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
   RETURN appl_err("Bundle is not allowed for this subscription type").
END.

ASSIGN lcPostpaidVoiceTariffs = fCParamC("POSTPAID_VOICE_TARIFFS")
       lcPrepaidVoiceTariffs  = fCParamC("PREPAID_VOICE_TARIFFS")
       lcAllowedBONOContracts = fCParamC("ALLOWED_BONO_CONTRACTS")
       lcOnlyVoiceContracts   = fCParamC("ONLY_VOICE_CONTRACTS")
       lcDataBundleCLITypes   = fCParamC("DATA_BUNDLE_BASED_CLITYPES")
       ldeActStamp            = Func.Common:mMakeTS().

CASE pcActionValue :
   /* termination */
   WHEN "off" THEN DO: 
      /* Subscription level */
      IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO:
         /* should exist MDUB valid to the future */   
         IF (fGetActiveMDUB(INPUT "", INPUT ldNextMonthActStamp) NE pcBundleId) THEN
            RETURN appl_err("Bundle termination is not allowed").

         /* should not exist any pending request for MDUB */
         IF fPendingMDUBTermReq("") THEN
            RETURN appl_err("Bundle already cancelled").

         /* Ongoing BTC with upgrade upsell */
         IF fOngoingBTC(INPUT MobSub.MsSeq, INPUT pcBundleId, INPUT TRUE) THEN
            RETURN appl_err("Bundle termination is not allowed since " +
                            "subscription has ongoing BTC with upgrade upsell").
      END. /* IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO: */
      ELSE DO:
         IF NOT fIsDSSActive(Mobsub.Custnum,ldeActStamp) THEN
            RETURN appl_err("Bundle termination is not allowed").

         IF fOngoingDSSTerm(Mobsub.Custnum,ldNextMonthActStamp) THEN
            RETURN appl_err("Bundle already cancelled").
      END. /* ELSE DO: */

      liActionValue = 0.
      lcOnOff = "Desactivar".
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
      ELSE DO:
         IF pcBundleId = {&DSS} THEN
            RETURN appl_err("DSS activation is not allowed").

         IF fIsDSSActive(Mobsub.Custnum,ldeActStamp) THEN
            RETURN appl_err("Bundle already active").

         IF fOngoingDSSAct(Mobsub.Custnum) THEN
            RETURN appl_err("Bundle activation is not allowed").
      END. /* ELSE DO: */

      liActionValue = 1. 
      lcOnOff = "Activar".
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

IF liRequest = 0 THEN RETURN appl_err("Bundle request not created"). 

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

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

Func.Common:mWriteMemoWithType("MobSub",                             /* HostTable */
                 STRING(Mobsub.MsSeq),                 /* KeyValue  */
                 MobSub.CustNum,                       /* CustNum */
                 DayCampaign.DCName,                   /* MemoTitle */
                 DayCampaign.DCName + " - " + lcOnOff, /* MemoText */
                 "Service",                            /* MemoType */
                 fgetAppDetailedUserId(INPUT lcApplicationId,
                                      INPUT Mobsub.CLI)).

FINALLY:
   /* Store the transaction id */
   ghAuthLog::TransactionId = pcTransId.

   END.

