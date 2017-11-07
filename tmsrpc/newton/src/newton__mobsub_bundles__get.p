/**
 * Get bundles statues.
 *
 * @input id;array of string;mandatory; bundles
 * @output bundle;array of struct; bundle data
 * @bundle id;string; Bundle Id
           pending_bundle;string;BTC New Bundle Id
           upgrade_upsell;boolean;BTC with Upgrade upsell
           value;int; bundle status ( 0=desactivated,1=activated,
                                      2=cancelled-ongoing,
                                      3=activation-ongoing,
                                      4=ongoing BTC)
           activation_stamp;datetime;Bundle activation date (for active contracts)
           options;struct;extra parameters
           upcoming_data_bundle;boolean;true/false
           
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Mm/active_bundle.i}
{Mm/fbundle.i}
{Func/upsellcount.i}

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR liCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.

DEFINE VARIABLE liStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE piMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcBundleId AS CHARACTER NO-UNDO. 
DEF VAR liActivations AS INT NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR pcBTCBundleId AS CHAR NO-UNDO.
DEF VAR ldEndStamp    AS DEC  NO-UNDO.
DEF VAR ldEndDate     AS DATE NO-UNDO.
DEF VAR lcOptionsStruct AS CHAR NO-UNDO. 
DEF VAR llUpComingDataBundle AS LOG NO-UNDO.
DEF VAR llUpgradeUpsell AS LOG NO-UNDO.
DEF VAR lcBONOContracts AS CHAR NO-UNDO.
DEF VAR lcVoiceBundles  AS CHAR NO-UNDO.
DEF VAR lcSupplementaryDataBundles  AS CHAR NO-UNDO.
DEF VAR lcSupplementaryVoiceBundles AS CHAR NO-UNDO.
DEF VAR ldeActivationTS AS DEC  NO-UNDO. 
DEF VAR liActAllowed    AS INT  NO-UNDO INIT 1.

DEFINE BUFFER bMsRequest  FOR MsRequest.

ASSIGN ldEndDate  = Func.Common:mLastDayOfMonth(TODAY)
       ldEndStamp = Func.Common:mMake2DT(ldEndDate,86399).

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.
pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").


FUNCTION fGetMDUBStatus RETURNS INT (
   INPUT pcBundle AS CHAR,
   OUTPUT odeActivationTS AS DEC):
   
   DEF VAR liStat AS INT NO-UNDO. 
   DEF BUFFER MServiceLimit FOR MServiceLimit.
   liStat= 0. /* desactivated */ 

   llUpComingDataBundle = fBundleWithSTC(Mobsub.MsSeq,Func.Common:mSecOffSet(ldEndStamp,1)).
    
   /* Activated */
   FOR EACH ServiceLimitGroup NO-LOCK WHERE 
            ServiceLimitGroup.Brand     = Syst.Var:gcBrand AND
            ServiceLimitGroup.GroupCode = pcBundle,
       EACH ServiceLimit NO-LOCK WHERE 
            ServiceLimit.GroupCode  = pcBundle AND 
            ServiceLimit.ValidFrom <= TODAY AND 
            ServiceLimit.ValidTo   >= TODAY:

      IF pcBundle BEGINS {&DSS} THEN DO:
      
         FIND FIRST MServiceLimit NO-LOCK WHERE 
                    MServiceLimit.Custnum  = MobSub.Custnum        AND
                    MServiceLimit.DialType = ServiceLimit.DialType AND
                    MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                    MServiceLimit.EndTS   >= ldEndStamp NO-ERROR.

         IF AVAIL MServiceLimit THEN DO:
            IF MServiceLimit.EndTS > ldEndStamp THEN liStat = 1.
            ELSE IF MServiceLimit.EndTS = ldEndStamp THEN liStat = 2. /* cancelled ongoing */
            odeActivationTS = MServiceLimit.FromTS.
         END.
            
      END. /* IF pcBundle BEGINS {&DSS} THEN DO: */

      ELSE DO:
         FIND FIRST MServiceLimit NO-LOCK WHERE 
                    MServiceLimit.MsSeq    = MobSub.MsSeq        AND
                    MServiceLimit.DialType = ServiceLimit.DialType AND
                    MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                    MServiceLimit.EndTS   >= ldEndStamp NO-ERROR.

         IF AVAIL MServiceLimit THEN DO:
            IF MServiceLimit.EndTS > ldEndStamp THEN liStat = 1.
            ELSE IF MServiceLimit.EndTS = ldEndStamp THEN liStat = 2. /* cancelled ongoing */.
            odeActivationTS = MServiceLimit.FromTS.
         END.
      END. /* ELSE DO: */
   END.

   IF pcBundle BEGINS {&DSS} THEN DO:
      /* ongoing DSS cancellation */
      IF fOngoingDSSTerm(MobSub.Custnum,ldEndStamp) THEN liStat = 2.

      /* ongoing DSS activation */
      ELSE IF fOngoingDSSAct(MobSub.Custnum) THEN liStat = 3.
   END. /* IF pcBundle BEGINS {&DSS} THEN DO: */

   /* pending request for MDUB/PMDUB termination */
   ELSE IF CAN-FIND(FIRST MsRequest WHERE MsRequest.MsSeq      = MobSub.MsSeq  AND
                                          MsRequest.ReqType    = {&REQTYPE_CONTRACT_TERMINATION} AND
                                          MsRequest.ReqCParam3 = pcBundle AND
                                          LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0 
                                          USE-INDEX MsSeq) THEN 
      liStat = 2. /* cancelled ongoing */
   /* pending request for bundle activation */
   ELSE IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq      = MobSub.MsSeq  AND
                     MsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
                     MsRequest.ReqCParam3 = pcBundle AND
                     LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0
                     USE-INDEX MsSeq) THEN
                     liStat = 3. /* activation ongoing */

   /* ongoing btc change */
   ELSE IF LOOKUP(pcBundle,lcBONOContracts) > 0 THEN 
   DO:
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq   = MobSub.MsSeq                                        AND
                 MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE}                            AND
                 LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                 LOOKUP(MsRequest.ReqCparam1,lcBONOContracts) > 0                        NO-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN 
      DO:
         ASSIGN 
            liStat = 4
            pcBTCBundleId = MsRequest.ReqCparam2.

         IF MsRequest.ReqCparam5 > "" THEN 
            llUpgradeUpsell = TRUE.
      END. /* IF AVAILABLE MsRequest THEN DO: */
   END. /* ELSE IF LOOKUP(pcBundle,lcBONOContracts) > 0 THEN DO: */
   /* ongoing btc change */
   ELSE IF LOOKUP(pcBundle,lcVoiceBundles) > 0 THEN 
   DO:
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq   = MobSub.MsSeq                                        AND
                 MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE}                            AND
                 LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                 LOOKUP(MsRequest.ReqCparam1,lcVoiceBundles) > 0                         NO-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN 
      DO:
         ASSIGN 
            liStat = 4
            pcBTCBundleId = MsRequest.ReqCparam2.
            
         IF MsRequest.ReqCparam5 > "" THEN 
            llUpgradeUpsell = TRUE.
      END. /* IF AVAILABLE MsRequest THEN DO: */
   END. /* ELSE IF LOOKUP(pcBundle,lcBONOContracts) > 0 THEN DO: */
   ELSE IF LOOKUP(pcBundle,lcSupplementaryDataBundles) > 0 THEN 
   DO:
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq   = MobSub.MsSeq                                        AND
                 MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE}                            AND
                 LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                 LOOKUP(MsRequest.ReqCparam1,lcSupplementaryDataBundles) > 0             NO-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN 
      DO:
         ASSIGN 
            liStat = 4
            pcBTCBundleId = MsRequest.ReqCparam2.
            
         IF MsRequest.ReqCparam5 > "" THEN 
            llUpgradeUpsell = TRUE.
      END. /* IF AVAILABLE MsRequest THEN DO: */
   END. /* ELSE IF LOOKUP(pcBundle,lcBONOContracts) > 0 THEN DO: */
   ELSE IF LOOKUP(pcBundle,lcSupplementaryVoiceBundles) > 0 THEN 
   DO:
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq   = MobSub.MsSeq                                        AND
                 MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE}                            AND
                 LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                 LOOKUP(MsRequest.ReqCparam1,lcSupplementaryVoiceBundles) > 0            NO-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN 
      DO:
         ASSIGN 
            liStat = 4
            pcBTCBundleId = MsRequest.ReqCparam2.
            
         IF MsRequest.ReqCparam5 > "" THEN 
            llUpgradeUpsell = TRUE.
      END. /* IF AVAILABLE MsRequest THEN DO: */
   END. /* ELSE IF LOOKUP(pcBundle,lcBONOContracts) > 0 THEN DO: */
   RETURN liStat. 

END FUNCTION.


DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcId = get_string(pcIDArray, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   piMsSeq = INT(ENTRY(2,pcId,"|")).
   pcBundleId = ENTRY(1,pcId,"|").

   {newton/src/findtenant.i NO OrderCanal MobSub MsSeq piMsSeq}
   
   IF lcBONOContracts = "" THEN 
   DO:
       ASSIGN 
          lcBONOContracts             = fCParamC("BONO_CONTRACTS")
          lcVoiceBundles              = fCParamC("VOICE_BONO_CONTRACTS")
          lcSupplementaryDataBundles  = fCParamC("SUPPLEMENT_DATA_BONO_CONTRACTS")
          lcSupplementaryVoiceBundles = fCParamC("SUPPLEMENT_VOICE_BONO_CONTRACTS").
   END.
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", pcBundleId + "|" + STRING(MobSub.MsSeq)).

   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = pcBundleId NO-LOCK NO-ERROR. 
   IF AVAIL DayCampaign THEN add_string(lcResultStruct, "name", DayCampaign.DCName).

   /* pass number of activations in case of UPSELL */
   IF DayCampaign.InstanceLimit > 1 THEN 
   DO:
       liActivations = fGetUpSellCount(pcBundleId,piMsSeq,MobSub.Custnum,OUTPUT lcError).
       add_int(lcResultStruct, "activations",liActivations).
   END.
   ELSE
   DO:
       liStatus = fGetMDUBStatus(pcBundleId, OUTPUT ldeActivationTS).
       
       add_int(lcResultStruct,"value",liStatus).
       add_string(lcResultStruct, "pending_bundle", pcBTCBundleId).
       add_boolean(lcResultStruct, "upgrade_upsell", llUpgradeUpsell).

       IF ldeActivationTS > 0 THEN
          add_timestamp(lcResultStruct, "activation_stamp", ldeActivationTS).

       lcOptionsStruct = add_struct(lcResultStruct,"options").
       add_boolean(lcOptionsStruct, "upcoming_data_bundle", llUpComingDataBundle).
   END.
END.

FINALLY:
   END.
