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
 * @region taxzone;string; VAT Code Name
           taxinclvalue;decimal; Monthly Cost based on including Zone Tax
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/fprepaidfee.i}
{Func/multitenantfunc.i}

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR pcTenant  As CHAR NO-UNDO.
DEF VAR liCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.
DEF VAR lcRegionArray  AS CHAR NO-UNDO.
DEF VAR lcRegionStruct AS CHAR NO-UNDO.

DEF VAR lcIPLContracts         AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts       AS CHAR NO-UNDO.
DEF VAR lcFLATContracts        AS CHAR NO-UNDO.
DEF VAR lcBONOContracts        AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts       AS CHAR NO-UNDO.
DEF VAR lcCONTSFContracts      AS CHAR NO-UNDO.
DEF VAR lcCLIType              AS CHAR NO-UNDO.
DEF VAR lcBundleType           AS CHAR NO-UNDO.
DEF VAR ldeFee                 AS DEC  NO-UNDO.
DEF VAR liLineType             AS INT  NO-UNDO.
DEF VAR llDss2Compatible       AS LOG NO-UNDO. 
DEF VAR liFixedLineType        AS INT  NO-UNDO.
DEF VAR lcAllowedDSS2SubsType  AS CHAR NO-UNDO.
DEF VAR lcPromotionBundles     AS CHAR NO-UNDO. 
DEF VAR lcVoiceBundles         AS CHAR NO-UNDO.
DEF VAR lcSupplementaryVoiceBundles AS CHAR NO-UNDO.
DEF VAR lcSupplementaryDataBundles  AS CHAR NO-UNDO.
DEF VAR lcSupplementBundles         AS CHAR NO-UNDO.
DEF VAR lcDefaultBundles            AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.

pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   IF NUM-ENTRIES(pcID,"|") > 1 THEN
       ASSIGN
           pcTenant = ENTRY(2, pcID, "|")
           pcID     = ENTRY(1, pcID, "|").
   ELSE
       RETURN appl_err("Invalid tenant information").

   {newton/src/settenant.i pcTenant}

   IF lcBONOContracts = "" THEN
   DO:
       ASSIGN 
           lcIPLContracts              = fCParamC("IPL_CONTRACTS")
           lcCONTDContracts            = fCParamC("CONTD_CONTRACTS")
           lcFLATContracts             = fCParamC("FLAT_CONTRACTS")
           lcBONOContracts             = fCParamC("BONO_CONTRACTS")
           lcCONTSContracts            = fCParamC("CONTS_CONTRACTS")
           lcCONTSFContracts           = fCParamC("CONTSF_CONTRACTS")
           lcAllowedDSS2SubsType       = fCParamC("DSS2_SUBS_TYPE")
           lcPromotionBundles          = fCParamC("PROMOTION_BUNDLES")
           lcVoiceBundles              = fCParamC("VOICE_BONO_CONTRACTS")
           lcSupplementaryVoiceBundles = fCParamC("SUPPLEMENT_VOICE_BONO_CONTRACTS")
           lcSupplementaryDataBundles  = fCParamC("SUPPLEMENT_DATA_BONO_CONTRACTS") 
           lcDefaultBundles            = "MM_DATA600"
           lcSupplementBundles         = TRIM(lcSupplementaryVoiceBundles + "," + lcSupplementaryDataBundles, ",").
   END.    

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
      llDss2Compatible = FALSE.

   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", DayCampaign.DCEvent + "|" + fConvertTenantToBrand(pcTenant)).
   add_string(lcResultStruct, "brand", fConvertTenantToBrand(pcTenant)).
   add_string(lcResultStruct,"name", DayCampaign.DCName).
   add_int(lcResultStruct,"status", DayCampaign.StatusCode).

   FIND FIRST CCN WHERE CCN.Brand = "1" AND CCN.CCN = DayCampaign.CCN NO-LOCK NO-ERROR.
   add_string(lcResultStruct,"category", (IF AVAIL CCN THEN (IF INDEX(CCN.CCNName,"Roaming") > 0 THEN "Roaming" 
                                                             ELSE IF INDEX(CCN.CCNName,"International") > 0 THEN "International" 
                                                             ELSE "National") 
                                          ELSE "National")).

   FIND FIRST FMItem NO-LOCK WHERE
              FMItem.Brand     = Syst.Var:gcBrand              AND
              FMItem.FeeModel  = DayCampaign.FeeModel AND
              FMItem.ToDate   >= TODAY                AND 
              FMItem.FromDate <= TODAY                NO-ERROR.
   IF AVAIL FMItem THEN ldeFee = FMItem.Amount. 
   ELSE ldeFee = 0.

   IF LOOKUP(DayCampaign.DCType,"1,4,6,8") > 0 THEN DO:
      /*YDR-2284 added validfrom AND validto conditions for getting valid record*/
      FOR EACH ServiceLimit WHERE
               ServiceLimit.GroupCode = DayCampaign.DCEvent AND
               ServiceLimit.ValidFrom <= TODAY              AND
               ServiceLimit.ValidTo   >= TODAY NO-LOCK:

          IF ServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN
             add_double(lcResultStruct,"data_amount", ServiceLimit.InclAmt).
          ELSE IF ServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
             add_double(lcResultStruct,"voice_amount", ServiceLimit.InclAmt).
          ELSE IF ServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE} THEN 
             add_double(lcResultStruct,"fixed2mobile_voice_amount", ServiceLimit.InclAmt).   
          ELSE IF ServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE_BDEST} THEN 
             add_double(lcResultStruct,"fixed2fixed_voice_amount", ServiceLimit.InclAmt).   
          ELSE IF ServiceLimit.DialType = {&DIAL_TYPE_SMS} THEN 
             add_double(lcResultStruct,"sms_amount", ServiceLimit.InclAmt).       
      END. /* FOR EACH ServiceLimit WHERE */
   END. /* IF LOOKUP(DayCampaign.DCType,"1,4,6,8") > 0 THEN DO: */
   
   IF  LOOKUP(DayCampaign.DCEvent,lcBONOContracts)     > 0 OR
       LOOKUP(DayCampaign.DCEvent,lcSupplementBundles) > 0 OR 
       LOOKUP(DayCampaign.DCEvent,lcDefaultBundles)    > 0 THEN
   DO:
      IF LOOKUP(DayCampaign.DCEvent,lcSupplementBundles) > 0 THEN   
         lcBundleType = "supplemental_bundle".
      ELSE      
         lcBundleType = "bundle".
   END.
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcVoiceBundles) > 0 THEN 
      lcBundleType = "voice_bundle".
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
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcPromotionBundles) > 0 THEN 
      lcBundleType = "promotional".   
   ELSE IF DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} THEN 
      lcBundleType = "tv_service".
   ELSE IF DayCampaign.BundleTarget EQ {&DC_BUNDLE_TARGET_SVA} THEN
      lcBundleType = "value_added_service".
   ELSE 
      lcBundleType = "service".
   
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
   END. /* IF lcCLIType > "" THEN DO: */

   add_string(lcResultStruct,"bundle_type", lcBundleType).
   add_string(lcResultStruct,"subscription_type_id", lcCLIType).
   add_int(lcResultStruct,"pay_type", DayCampaign.PayType).
   add_double(lcResultStruct,"monthly_cost", ldeFee).
   add_int(lcResultStruct,"line_type", liLineType).
   add_int(lcResultStruct,"fixed_line_type", liFixedLineType).
   add_boolean(lcResultStruct,"dss2_compatible", llDss2Compatible).
   add_boolean(lcResultStruct,"voip_compatible", FALSE).

   lcRegionArray = add_array(lcResultStruct, "region").
   FOR EACH VATCode NO-LOCK WHERE
            VATCode.TaxClass  = "1"   AND
            VATCode.FromDate <= TODAY AND
            VATCOde.ToDate   >= TODAY:
      lcRegionStruct = add_struct(lcRegionArray,"").
      add_string(lcRegionStruct,"taxzone", VATCode.VCName).
      add_double(lcRegionStruct,"taxinclvalue", (1 + VatCode.VatPerc / 100) * ldeFee).
   END.
END.

FINALLY:
   END.
