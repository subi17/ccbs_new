/**
 * Set bundle 
 *
 * @input  id;string;mandatory; bundle id 
           bundle;struct; bundle data 
 * @bundle name;string;optional; bundle name
           value;int;mandatory;0 = ON, 1 = OFF
           activations;int;optional;rpc does not need this, but web does
           username;string;mandatory; user name
           reason;string;optional;
 * @output status;struct;
 * @status value;int;optional; only in case of MDUB
           activations;int;optional; only for Upsell
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/mdub.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/upsellbundle.i}
{Func/msreqfunc.i}
{Func/fsendsms.i}
{Func/dss_request.i}
{Func/fprepaidfee.i}
{Mm/fbundle.i}

DEF VAR liRequest                   AS INT  NO-UNDO.
DEF VAR lcBONOContracts             AS CHAR NO-UNDO.
DEF VAR lcVoiceBONOContracts        AS CHAR NO-UNDO.
DEF VAR lcAllowedBONOContracts      AS CHAR NO-UNDO.
DEF VAR lcMemoText                  AS CHAR NO-UNDO.
DEF VAR lcMemoTitle                 AS CHAR NO-UNDO.
DEF VAR pcStruct                    AS CHAR NO-UNDO. 
DEF VAR lcStruct                    AS CHAR NO-UNDO.  
DEF VAR pcReason                    AS CHAR NO-UNDO. 
DEF VAR piMsSeq                     AS INTE NO-UNDO. 
DEF VAR pcId                        AS CHAR NO-UNDO. 
DEF VAR pcBundleId                  AS CHAR NO-UNDO. 
DEF VAR piBundleAction              AS INTE NO-UNDO.  
DEF VAR lcResultStruct              AS CHAR NO-UNDO. 
DEF VAR liReturnValue               AS INTE NO-UNDO. 
DEF VAR lcReturnValue               AS CHAR NO-UNDO. 
DEF VAR lcError                     AS CHAR NO-UNDO. 
DEF VAR lcCounterError              AS CHAR NO-UNDO. 
DEF VAR lcOnOff                     AS CHAR NO-UNDO.
DEF VAR lcBundleList                AS CHAR NO-UNDO.
DEF VAR lcSupplementaryDataBundles  AS CHAR NO-UNDO.
DEF VAR lcSupplementaryVoiceBundles AS CHAR NO-UNDO.

DEFINE BUFFER bf_DayCampaign FOR DayCampaign.

FUNCTION fSetMDUB RETURNS INT
    (INPUT piMsSeq    AS INT,
     INPUT pcBundleId AS CHAR,
     INPUT piAction   AS INT,
     OUTPUT ocError   AS CHAR):

   DEF VAR liReturnValue AS INT NO-UNDO. 
   DEF VAR lcResult AS CHAR NO-UNDO. 
   DEF VAR ldeActStamp AS DEC NO-UNDO. 
   DEF VAR ldaActDate AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO. 
   DEF VAR llResult AS LOGICAL NO-UNDO. 

   DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.
   DEF VAR lcPrepaidVoiceTariffs  AS CHAR NO-UNDO.
   DEF VAR lcOnlyVoiceContracts   AS CHAR NO-UNDO.
   DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.
   DEF VAR ldeBundleFee           AS DEC  NO-UNDO.
   DEF VAR lcBundleType           AS CHAR NO-UNDO.

   ASSIGN lcPostpaidVoiceTariffs = fCParamC("POSTPAID_VOICE_TARIFFS")
          lcPrepaidVoiceTariffs  = fCParamC("PREPAID_VOICE_TARIFFS")
          lcOnlyVoiceContracts   = fCParamC("ONLY_VOICE_CONTRACTS")
          lcDataBundleCLITypes   = fCParamC("DATA_BUNDLE_BASED_CLITYPES")
          ldeActStamp            = Func.Common:mMakeTS().

   Func.Common:mSplitTS(ldeActStamp,output ldaActDate, output liTime).

   CASE piAction :
      /* termination */
      WHEN 0 THEN DO: 
         /* Subscription level */
         IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN 
         DO:
            IF NOT fAllowMDUBTermination("") THEN
               ocError = pcBundleId + " termination is not allowed".
            /* Ongoing BTC with upgrade upsell */
            ELSE IF fOngoingBTC(INPUT MobSub.MsSeq,INPUT pcBundleId,INPUT TRUE) THEN
               ocError = "Bundle termination is not allowed since subscription has ongoing BTC with upgrade upsell".
            ELSE 
               liReturnValue = 2. /* Ongoing Termination */
         END. /* IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO: */
         ELSE IF LOOKUP(pcBundleId,lcVoiceBONOContracts) > 0 THEN 
         DO:
            IF NOT fAllowMDUBTermination("VOICE_") THEN
               ocError = pcBundleId + " termination is not allowed".
            ELSE IF fOngoingBTC(INPUT MobSub.MsSeq,INPUT pcBundleId,INPUT TRUE) THEN
               ocError = "Bundle termination is not allowed since subscription has ongoing BTC with upgrade upsell".
            ELSE 
               liReturnValue = 2. /* Ongoing Termination */
         END. 
         ELSE IF LOOKUP(pcBundleId,lcSupplementaryVoiceBundles) > 0 THEN 
         DO:
            IF NOT fAllowMDUBTermination("SUPPLEMENT_VOICE_") THEN
               ocError = pcBundleId + " termination is not allowed".
            ELSE IF fOngoingBTC(INPUT MobSub.MsSeq,INPUT pcBundleId,INPUT TRUE) THEN
               ocError = "Bundle termination is not allowed since subscription has ongoing BTC with upgrade upsell".
            ELSE 
               liReturnValue = 2. /* Ongoing Termination */
         END. 
         ELSE IF LOOKUP(pcBundleId,lcSupplementaryDataBundles) > 0 THEN 
         DO:
            IF NOT fAllowMDUBTermination("SUPPLEMENT_DATA_") THEN
               ocError = pcBundleId + " termination is not allowed".
            ELSE IF fOngoingBTC(INPUT MobSub.MsSeq,INPUT pcBundleId,INPUT TRUE) THEN
               ocError = "Bundle termination is not allowed since subscription has ongoing BTC with upgrade upsell".
            ELSE 
               liReturnValue = 2. /* Ongoing Termination */
         END. 
         ELSE IF pcBundleId = {&TARJ_UPSELL} THEN DO:
            ocError = pcBundleId + " termination is not allowed".
         END.
         /* Customer level - As of now DSS only */
         ELSE DO:
            IF NOT fIsDSSActive(Mobsub.Custnum,ldNextMonthActStamp) OR
               fOngoingDSSTerm(Mobsub.Custnum,ldNextMonthActStamp) THEN
               ocError = pcBundleId + " termination is not allowed".
            ELSE liReturnValue = 2. /* Ongoing Termination */
         END. /* ELSE DO: */
      END.
      /* activation */
      WHEN 1 THEN DO:
         /*ILP, YPR-2174*/
         IF LOOKUP(pcBundleId, "DATA200_UPSELL,DSS200_UPSELL") > 0 THEN 
            ocError = pcBundleId + " activation is not allowed".         
         /**/
         IF pcBundleID EQ "VOICE200B" THEN DO:
            IF NOT fIsBundleAllowed(Mobsub.CLIType,
                                "VOICE200B",
                                OUTPUT lcError) OR
               NOT fAllowMDUBActivation("VOICE_") THEN
               ocError = pcBundleID + " Actiavtion not allowed".
         END.

         /* Data Bono's */
         ELSE IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN 
         DO:
            IF LOOKUP(pcBundleId,lcAllowedBONOContracts) = 0 OR NOT fAllowMDUBActivation("") THEN
               ocError = pcBundleId + " activation is not allowed".
            ELSE 
               liReturnValue = 3. /* Ongoing Activation */
         END. 
         /* Voice Bono's */
         ELSE IF LOOKUP(pcBundleId,lcVoiceBONOContracts) > 0 THEN 
         DO:
            IF LOOKUP(pcBundleId,lcAllowedBONOContracts) = 0 OR NOT fAllowMDUBActivation("VOICE_") THEN
               ocError = pcBundleId + " activation is not allowed".
            ELSE 
               liReturnValue = 3. /* Ongoing Activation */
         END. 
         ELSE IF LOOKUP(pcBundleId,lcSupplementaryDataBundles) > 0 THEN 
         DO:
            IF LOOKUP(pcBundleId,lcAllowedBONOContracts) = 0 OR NOT fAllowMDUBActivation("SUPPLEMENT_DATA_") THEN
               ocError = pcBundleId + " activation is not allowed".
            ELSE 
               liReturnValue = 3. /* Ongoing Activation */
         END. 
         ELSE IF LOOKUP(pcBundleId,lcSupplementaryVoiceBundles) > 0 THEN 
         DO:
            IF LOOKUP(pcBundleId,lcAllowedBONOContracts) = 0 OR NOT fAllowMDUBActivation("SUPPLEMENT_VOICE_") THEN
               ocError = pcBundleId + " activation is not allowed".
            ELSE 
               liReturnValue = 3. /* Ongoing Activation */
         END. 
         ELSE IF pcBundleId = {&TARJ_UPSELL} THEN .
         /* Customer level - As of now DSS only */
         ELSE DO:
            IF pcBundleId = {&DSS} OR fIsDSSActive(Mobsub.Custnum,ldeActStamp) OR fOngoingDSSAct(Mobsub.Custnum) THEN
               ocError = pcBundleId + " activation is not allowed".
            ELSE liReturnValue = 3. /* Ongoing Activation */
         END. /* ELSE DO: */
      END.
      OTHERWISE DO: 
        ocError = "Invalid " + pcBundleId + " value".
      END.
   END CASE.
   
   IF ocError NE "" THEN RETURN liReturnValue.

   /* Validate Prepaid Balance before making PMDUB/TARJ_UPSELL activation request */
   IF (pcBundleId = {&PMDUB} AND piAction = 1) OR
      pcBundleId = {&TARJ_UPSELL} THEN DO:
      ldeBundleFee = fgetPrepaidFeeAmount(pcBundleId, TODAY).

      RUN pEnoughBalance(INPUT MobSub.CLI,
                         INPUT ldeBundleFee,
                         OUTPUT llResult).
      IF llResult = FALSE THEN
         ocError = "Not enough balance".
   END.

   IF ocError NE "" THEN RETURN liReturnValue.

   IF piAction = 0 THEN
      ldeActStamp = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(ldaActDate),86399).

   IF pcBundleId = {&DSS} THEN DO:
      IF piAction = 0 THEN
         liRequest = fDSSRequest(MobSub.MsSeq,
                              Mobsub.Custnum,
                              "DELETE",
                              "DSS-ACCOUNT=" + STRING(Mobsub.Custnum),
                              pcBundleId,
                              ldeActStamp,
                              {&REQUEST_SOURCE_NEWTON},
                              "",
                              TRUE, /* Fees */
                              0,
                              FALSE,
                              OUTPUT lcResult).
   END.
   ELSE DO:

      liRequest = fPCActionRequest(MobSub.MsSeq,
                                pcBundleId,
                                IF piAction = 1 THEN "act" ELSE "term",
                                ldeActStamp,
                                TRUE,    /* fees */
                                {&REQUEST_SOURCE_NEWTON},
                                "",   /* creator */
                                0,    /* no father request */
                                FALSE,
                                "",
                                0,
                                0,
                                "",
                                OUTPUT lcResult).
   END.
   
   IF liRequest = 0 THEN 
      ocError = "ERROR:Bundle request not created; " + lcResult.
   ELSE IF piAction <> 1 AND 
          (LOOKUP(pcBundleId,lcBONOContracts)             > 0 OR 
           LOOKUP(pcBundleId,lcVoiceBONOContracts)        > 0 OR 
           LOOKUP(pcBundleId,lcSupplementaryDataBundles)  > 0 OR 
           LOOKUP(pcBundleId,lcSupplementaryVoiceBundles) > 0) THEN 
   DO:
      /* Cancel Ongoing BTC request */
      FOR FIRST MsRequest NO-LOCK WHERE 
                MsRequest.MsSeq   = MobSub.MsSeq                                 AND
                MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE}                     AND
                LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0 AND
                (LOOKUP(MsRequest.ReqCparam1,lcBONOContracts)            > 0 OR 
                 LOOKUP(MsRequest.ReqCparam1,lcVoiceBONOContracts)       > 0 OR 
                 LOOKUP(MsRequest.ReqCparam1,lcSupplementaryDataBundles) > 0 OR 
                 LOOKUP(MsRequest.ReqCparam1,lcSupplementaryVoiceBundles) > 0)        
          USE-INDEX MsSeq:
          fReqStatus(4,"Bundle Type Change can not be performed since " +
                       "subscriber has requested to cancel the bundle.").
          /* Send a SMS */
          RUN pSendSMS(INPUT MobSub.MsSeq, 
                       INPUT MsRequest.MsRequest,
                       INPUT "BTCBundelDeAct", 
                       INPUT 10,
                       INPUT {&UPSELL_SMS_SENDER}, 
                       INPUT "").
      END. /* FOR FIRST MsRequest NO-LOCK WHERE */
   END. /* IF LOOKUP(pcBundleId,lcBONOContracts) > 0 THEN DO: */
      
   /* Black Berry Project */
   FIND FIRST SubSer WHERE SubSer.ServCom = "BB"         AND
                           SubSer.MsSeq   = MobSub.MsSeq AND
                           SubSer.SsDate <= TODAY        NO-LOCK NO-ERROR.
   IF AVAILABLE SubSer AND SubSer.SSStat = 1 THEN 
   DO:
      IF LOOKUP(MobSub.CliType,lcPostpaidVoiceTariffs) > 0 AND LOOKUP(MobSub.CliType,lcDataBundleCLITypes) = 0 THEN 
      DO:
         IF MobSub.TariffBundle = "" OR LOOKUP(MobSub.TariffBundle,lcOnlyVoiceContracts) > 0 THEN
            RUN pSendSMS(INPUT MobSub.MsSeq,
                         INPUT 0,
                         INPUT "BBDeActBundPost_1",
                         INPUT 11,
                         INPUT {&BB_SMS_SENDER},
                         INPUT "").
      END. /* IF LOOKUP(MobSub.CliType,lcPostpaidVoiceTariffs}) > 0 */
      ELSE IF LOOKUP(MobSub.CliType,lcPrepaidVoiceTariffs) > 0 THEN
         RUN pSendSMS(INPUT MobSub.MsSeq,
                      INPUT 0,
                      INPUT "BBDeActBundPre_1",
                      INPUT 11,
                      INPUT {&BB_SMS_SENDER},
                      INPUT "").
   END. /* IF AVAILABLE SubSer AND SubSer.SSStat = 1 THEN DO: */

   RETURN liReturnValue.
   
END FUNCTION.

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN 
    RETURN.

ASSIGN 
    pcId     = get_string(param_toplevel_id,"0")
    pcStruct = get_struct(param_toplevel_id,"1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct,"name,value!,activations,username!,reason").
IF gi_xmlrpc_error NE 0 THEN RETURN.

Syst.Var:katun = "VISTA_" + get_string(pcStruct,"username").

piBundleAction = get_int(pcStruct,"value").

IF LOOKUP("reason",lcStruct) > 0 THEN 
    pcReason = get_string(pcStruct,"reason").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcBundleId = ENTRY(1,pcId,"|").
piMsSeq    = INT(ENTRY(2,pcId,"|")).

{newton/src/findtenant.i NO OrderCanal MobSub MsSeq piMsSeq}

/*YPR-4775*/
/*(De)Activation is not allowed if fixed line provisioning is pending*/
IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG}    /*16*/ OR
    MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) /*17*/ THEN
   RETURN appl_err("Mobile line provisioning is not complete").

IF TRIM(Syst.Var:katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF piBundleAction NE 0 AND piBundleAction NE 1 THEN
   RETURN appl_err(SUBST("incorrect action value: &1", piBundleAction)).

IF piBundleAction = 0 AND INDEX(pcBundleId,"UPSELL") > 0 THEN
    RETURN appl_err(pcBundleId + " termination is not allowed").

IF piBundleAction EQ 1 THEN 
    lcOnOff = "Activar".
ELSE 
    lcOnOff = "Desactivar".

FIND DayCampaign NO-LOCK WHERE DayCampaign.Brand = Syst.Var:gcBrand AND DayCampaign.DCEvent = pcBundleId NO-ERROR.
IF NOT AVAIL DayCampaign THEN
   RETURN appl_err(SUBST("Invalid Bundle Id: &1", pcBundleId)).

ASSIGN lcMemoTitle                 = DayCampaign.DcName
       lcBONOContracts             = fCParamC("BONO_CONTRACTS")
       lcVoiceBONOContracts        = fCParamC("VOICE_BONO_CONTRACTS") 
       lcAllowedBONOContracts      = fCParamC("ALLOWED_BONO_CONTRACTS")
       lcSupplementaryDataBundles  = fCParamC("SUPPLEMENT_DATA_BONO_CONTRACTS")
       lcSupplementaryVoiceBundles = fCParamC("SUPPLEMENT_VOICE_BONO_CONTRACTS")
       lcBundleList                = TRIM(lcBONOContracts             + "," + 
                                          lcSupplementaryDataBundles  + "," + 
                                          lcVoiceBONOContracts        + "," + 
                                          lcSupplementaryVoiceBundles,",").

IF LOOKUP(pcBundleId, lcBundleList) > 0 OR 
   pcBundleId = {&DSS}                  OR 
   pcBundleID = "voice200b"             OR
   pcBundleId = {&TARJ_UPSELL} THEN 
DO:
   ASSIGN 
       lcMemoText    = pcReason + " " + DayCampaign.DCName + " - " + lcOnOff
       lcMemoTitle   = DayCampaign.DCName
       lcReturnValue = "value"
       liReturnValue = fSetMDUB(piMsSeq,pcBundleId,piBundleAction,OUTPUT lcError).
END.
ELSE IF pcBundleId MATCHES ("*_UPSELL") THEN 
DO:
   fCreateUpsellBundle(piMsSeq,
                       pcBundleId,
                       {&REQUEST_SOURCE_NEWTON},
                       Func.Common:mMakeTS(),
                       OUTPUT liRequest,
                       OUTPUT lcError).

   IF pcBundleId EQ "TARJ7_UPSELL" OR
      pcBundleId EQ "DATA6_UPSELL" OR
      pcBundleId EQ "DSS_UPSELL" OR
      pcBundleId EQ "DSS2_UPSELL" THEN
       ASSIGN 
           lcMemoText = IF INDEX(Daycampaign.DCName,"Ampliaci�n") > 0 THEN 
                            DayCampaign.DCName  + " - " + lcOnOff
                        ELSE
                            "Ampliaci�n " + DayCampaign.DCName + " - " + lcOnOff
           lcMemoTitle = DayCampaign.DCName.
   ELSE 
       ASSIGN 
           lcMemoText = pcReason + " " + 
                        IF INDEX(Daycampaign.DCName,"Ampliaci�n") > 0 THEN
                           DayCampaign.DCName + " - " + lcOnOff
                        ELSE
                           "Ampliaci�n " + DayCampaign.DCName + " - " + lcOnOff
           lcMemoTitle = DayCampaign.DCName.
END.
ELSE 
   lcError = "Invalid Bundle Id: " + pcBundleId .

IF lcError NE "" THEN 
    RETURN appl_err(lcError).  

IF pcBundleId MATCHES ("*_UPSELL") THEN 
DO:   
   liReturnValue = fGetUpsellCount(pcBundleId,piMsSeq,MobSub.Custnum,OUTPUT lcCounterError).
   liReturnValue = liReturnValue + 1. 
   lcReturnValue = "activations".
END.

IF NOT (LOOKUP(MobSub.CliType,"CONTFH129_1000,CONTDSL99,CONTFH99_50,CONTFH109_300") > 0 AND
        pcBundleId = "FLEX_UPSELL") THEN DO:

   Func.Common:mWriteMemoWithType("MobSub",             /* HostTable */
                  STRING(Mobsub.MsSeq),                 /* KeyValue  */
                  MobSub.CustNum,                       /* CustNum   */
                  lcMemoTitle,                          /* MemoTitle */
                  lcMemoText,                           /* MemoText  */
                  "Service",                            /* MemoType  */
                  Syst.Var:katun).

END.

lcResultStruct = add_struct(response_toplevel_id, "").
add_int(lcResultStruct,lcReturnValue,liReturnValue).
