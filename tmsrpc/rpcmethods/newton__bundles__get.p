/**
 * Get bundle details.
 *
 * @input  id;array of string;mandatory; bundle ids
 * @output bundles;array of struct;bundle data
 * @bundles        id;string;bundle id
                   name;string;subscription type name
                   status;int;status code (eg: 0=inactive,1=active,2=retired)
                   pay_type;int;payment type (eg: 0=both,1=postpaid,2=prepaid)
                   monthly_cost;double;monthly cost
                   data_amount;double;data amount (MB)
                   voice_amount;double;voice amount (Minute)
                   bundle_type;string;bundle type (eg: Bundle/Upsell/Subscription/Service)
                   subscription_type_id;string;subscription type
                   line_type;int;line type (main=1 or additional=2)
                   fixed_line_type;int;fixed line type (ADSL=1 or FIBER=2)
                   voip_amount;double;bono voip data amount (MB)
                   dss2_compatible;boolean;DSS2 compatible
                   voip_compatible;boolean;Voip Compatible
 */

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
gcBrand = "1".
DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR liCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.
pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

DEF VAR lcIPLContracts     AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts   AS CHAR NO-UNDO.
DEF VAR lcFLATContracts    AS CHAR NO-UNDO.
DEF VAR lcBONOContracts    AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts   AS CHAR NO-UNDO.
DEF VAR lcCONTSFContracts  AS CHAR NO-UNDO.
DEF VAR lcCLIType          AS CHAR NO-UNDO.
DEF VAR lcBundleType       AS CHAR NO-UNDO.
DEF VAR liPayType          AS INT  NO-UNDO.
DEF VAR ldeFee             AS DEC  NO-UNDO.
DEF VAR liLineType         AS INT  NO-UNDO.
DEF VAR llDss2Compatible   AS LOG NO-UNDO. 
DEF VAR liFixedLineType    AS INT  NO-UNDO.
DEF VAR lcVoIPBaseContracts AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
DEF VAR lcAllVoIPNativeBundles AS CHAR NO-UNDO.
DEF VAR llVoIPCompatible   AS LOG NO-UNDO.

{cparam2.i}
{tmsconst.i}
{fprepaidfee.i}

ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
       lcCONTDContracts = fCParamC("CONTD_CONTRACTS")
       lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
       lcBONOContracts  = fCParamC("BONO_CONTRACTS")
       lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
       lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
       lcVoIPBaseContracts = fCParamC("BONO_VOIP_BASE_BUNDLES")
       lcAllVoIPNativeBundles = fCParamC("NATIVE_VOIP_BASE_BUNDLES")
       lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").


DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   FIND FIRST DayCampaign NO-LOCK WHERE
              DayCampaign.Brand   = "1"  AND
              DayCampaign.DCEvent = pcID NO-ERROR.
   IF NOT AVAIL DayCampaign THEN
      RETURN appl_err("Periodical Contract not found: " + pcId).
   
   ASSIGN
      lcCLIType = ""
      lcBundleType = ""
      ldeFee = 0
      liLineType = 0
      liFixedLineType = 0
      llDss2Compatible = FALSE
      llVoIPCompatible = FALSE.

   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", DayCampaign.DCEvent).
   add_string(lcResultStruct,"name", DayCampaign.DCName).
   add_int(lcResultStruct,"status", DayCampaign.StatusCode).

   IF DayCampaign.FeeModel > "" THEN DO:
      liPayType = 1.

      /* temporary quick fix YTS-7421 */
      IF LOOKUP(DayCampaign.DCEvent,
               "PMDUB,PMDUB_UPSELL,TARJ7,TARJ7_UPSELL,TARJ9,TARJ_UPSELL") > 0
      THEN liPayType = 2.

      FIND FIRST FMItem NO-LOCK WHERE
                 FMItem.Brand = gcBrand AND
                 FMItem.FeeModel = DayCampaign.FeeModel AND
                 FMItem.ToDate  >= TODAY NO-ERROR.
      IF AVAIL FMItem THEN ldeFee = FMItem.Amount.
   END. /* IF DayCampaign.FeeModel > "" THEN DO: */
   ELSE DO:
      liPayType = 2.

      IF DayCampaign.DCEvent = "PMDUB" OR 
       DayCampaign.DCEvent = "PMDUB_UPSELL" OR
       DayCampaign.DCEvent = {&TARJ_UPSELL} OR
       DayCampaign.DCEvent = "TARJ7_UPSELL" THEN
       ldeFee = fgetPrepaidFeeAmount(DayCampaign.DCEvent, TODAY).
      ELSE ldeFee = 0.

      IF DayCampaign.DCEvent = "HSPA_ROAM_EU" THEN liPayType = 0.
   END.

   IF LOOKUP(DayCampaign.DCType,"1,4,6,8") > 0 THEN DO:
      FOR EACH ServiceLimit WHERE
               ServiceLimit.GroupCode = DayCampaign.DCEvent NO-LOCK:

          IF ServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN DO:
             IF ServiceLimit.GroupCode = "HSPA_ROAM_EU" THEN
                add_double(lcResultStruct,"data_amount", 20.0).
             ELSE
                add_double(lcResultStruct,"data_amount", ServiceLimit.InclAmt).
          END.
          ELSE IF ServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
             add_double(lcResultStruct,"voice_amount", ServiceLimit.InclAmt).

      END. /* FOR EACH ServiceLimit WHERE */
   END. /* IF LOOKUP(DayCampaign.DCType,"1,4,6,8") > 0 THEN DO: */
   
   IF LOOKUP(DayCampaign.DCEvent,lcBONOContracts) > 0 OR
      DayCampaign.DCEvent = "HSPA_ROAM_EU" THEN
      lcBundleType = "bundle".
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcIPLContracts) > 0 THEN
      ASSIGN lcBundleType = "subscription"
             lcCLIType = "CONTRD".
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcCONTDContracts) > 0 THEN
      ASSIGN lcBundleType = "subscription"
             lcCLIType = "CONTD".
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcFLATContracts) > 0 THEN
      ASSIGN lcBundleType = "subscription"
             lcCLIType = "CONTF".
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcCONTSContracts) > 0 THEN
      ASSIGN lcBundleType = "subscription"
             lcCLIType = "CONTS".
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcCONTSFContracts) > 0 THEN
      ASSIGN lcBundleType = "subscription"
             lcCLIType = "CONTSF".
   ELSE IF LOOKUP(DayCampaign.DCType,"6,8") > 0 THEN
      lcBundleType = "upsell".
   ELSE lcBundleType = "service".

   IF lcCLIType > "" THEN DO:
      FIND FIRST CLIType WHERE
                 CLIType.Brand   = "1" AND
                 CLIType.CLIType = DayCampaign.DCEvent NO-LOCK NO-ERROR.
      IF NOT AVAIL CLIType THEN
         FIND FIRST CLIType WHERE
                    CLIType.Brand   = "1" AND
                    CLIType.CLIType = lcCLIType NO-LOCK NO-ERROR.
      IF AVAIL CLIType THEN
         ASSIGN liLineType = CLIType.LineType
                liFixedLineType = CLIType.FixedLineType
                ldeFee = CLIType.CommercialFee

      llDss2Compatible = LOOKUP(lcCLIType, lcAllowedDSS2SubsType) > 0.
      llVoIPCompatible = LOOKUP(lcCLIType, lcAllVoIPNativeBundles) > 0.
   END. /* IF lcCLIType > "" THEN DO: */

   IF LOOKUP(DayCampaign.DCEvent,lcAllVoIPNativeBundles) > 0 THEN
      llVoIPCompatible = TRUE.

   add_string(lcResultStruct,"bundle_type", lcBundleType).
   add_string(lcResultStruct,"subscription_type_id", lcCLIType).
   add_int(lcResultStruct,"pay_type", liPayType).
   add_double(lcResultStruct,"monthly_cost", ldeFee).
   add_int(lcResultStruct,"line_type", liLineType).
   add_int(lcResultStruct,"fixed_line_type", liFixedLineType).
   add_boolean(lcResultStruct,"dss2_compatible", llDss2Compatible).
   add_boolean(lcResultStruct,"voip_compatible", llVoIPCompatible).

   IF LOOKUP(DayCampaign.DCEvent,lcVoIPBaseContracts) > 0 THEN DO:
      FIND FIRST ServiceLimit WHERE
                 ServiceLimit.GroupCode = "BONO_VOIP" NO-LOCK NO-ERROR.
      IF AVAIL ServiceLimit THEN
         add_double(lcResultStruct,"voip_amount", ServiceLimit.InclAmt).
   END.
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
