/**
 * Get subscription type.
 *
 * @input  id;array of string;mandatory; subscription type id
 * @output subscription_type;array of struct;subscription type data
 * @subscription_type id;string;subscription type id
                      name;string;subscription type name
                      status;int;status code (eg: 0=inactive,1=active,2=retired)
                      pay_type;int;payment type (eg: 1=postpaid,2=prepaid)
                      usage_type;string;tariff type(eg: Voice/Data)
                      monthly_cost;double;monthly cost
                      bundled;boolean; bundle based clitype (eg: CONTF/CONTRD/CONTS/CONTD)
                      data_amount;double;data amount for subscriptions with base bundle (CONT6,CONT7,CONT8,CONT9)
                      line_type;int;line type (main=1 or additional=2)
                      fixed_line_type;int;fixed line type (ADSL=1 or FIBER=2)
                      dss2_compatible;boolean;DSS2 compatible
                      voip_compatible;boolean;Voip Compatible
                      region;array;struct
 * @region taxzone;string; VAT Code Name
           taxinclvalue;decimal; Monthly Cost based on including Zone Tax  
 */

{header_get.i}
{Func/transname.i}
{Syst/tmsconst.i}
{Func/cparam2.i}

DEF VAR lcCLITypeTransName     AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType  AS CHAR NO-UNDO.
DEF VAR lcAllVoIPNativeBundles AS CHAR NO-UNDO.
DEF VAR ldaCont15PromoEnd      AS DATE NO-UNDO.
DEF VAR lcRegionArray          AS CHAR NO-UNDO.
DEF VAR lcRegionStruct         AS CHAR NO-UNDO.
DEF VAR lcFixedLineBB          AS CHAR NO-UNDO.

FUNCTION fTMSCodeName RETURNS CHARACTER
   (iTableName AS CHAR,
    iFieldName AS CHAR,
    iCodeValue AS CHAR).

   FIND FIRST TMSCodes NO-LOCK WHERE
              TMSCodes.TableName = iTableName AND
              TMSCodes.FieldName = iFieldName AND
              TMSCodes.CodeValue = iCodeValue NO-ERROR.

   IF AVAILABLE TMSCodes AND TMSCodes.InUse > 0 THEN RETURN TMSCodes.CodeName.
   ELSE RETURN "".

END FUNCTION.

FUNCTION fGetSLAmount RETURNS DECIMAL 
  (icServiceLimitGroup AS CHARACTER,
   iiDialType          AS INTEGER):

  FIND FIRST ServiceLimit NO-LOCK WHERE ServiceLimit.GroupCode = icServiceLimitGroup AND
                                        ServiceLimit.DialType  = iiDialType          AND
                                        ServiceLimit.ValidTo   >= TODAY              NO-ERROR.
  IF AVAIL ServiceLimit THEN 
    RETURN ServiceLimit.InclAmt.

  RETURN 0.                                          
         
END FUNCTION.  

FUNCTION fGetFixedLineBaseBundle RETURNS CHARACTER
  (icCliType AS CHARACTER):

  FIND FIRST RequestAction WHERE RequestAction.Brand      = "1"             AND
                                 RequestAction.ReqType    = 14              AND
                                 RequestAction.CliType    = icCliType       AND
                                 RequestAction.ActionType = "DayCampaign"   AND
                                 RequestAction.Action     = 1               AND
                                 RequestAction.ValidFrom <= TODAY           AND
                                 RequestAction.ValidTo   >= TODAY           NO-LOCK NO-ERROR. 
  IF AVAIL RequestAction THEN 
    RETURN RequestAction.ActionKey.                               

  RETURN "".

END FUNCTION.  

ASSIGN lcAllowedDSS2SubsType  = fCParamC("DSS2_SUBS_TYPE")
       lcAllVoIPNativeBundles = fCParamC("NATIVE_VOIP_BASE_BUNDLES")
       ldaCont15PromoEnd  = fCParamDa("CONT15PromoEndDate").

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   
   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand   = "1" AND
              CLIType.CLIType = pcID AND
              CLIType.WebStatusCode > 0 NO-ERROR.
   IF NOT AVAIL CLIType THEN RETURN appl_err("Subscription Type not found: "+ pcId).

   ASSIGN 
      lcCLITypeTransName = fGetItemName("1","CLIType",CLIType.CLIType,1,TODAY)
      lcFixedLineBB      = fGetFixedLineBaseBundle(CliType.CliType) WHEN CliType.TariffType > 0.

   IF CLIType.CLIType = "CONTFF" THEN
      lcCLITypeTransName = CLIType.CLIName.
   
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", CLIType.CLIType).
   add_string(lcResultStruct,"name", lcCLITypeTransName).
   add_int(lcResultStruct,"pay_type", CLIType.PayType).
   add_int(lcResultStruct,"status", (IF CLIType.CLIType EQ "CONT15" AND 
                                        CLIType.WebStatusCode EQ 2 AND 
                                        ldaCont15PromoEnd NE ? AND
                                        TODAY <= ldaCont15PromoEnd
                                     THEN 1
                                     ELSE CLIType.WebStatusCode)).
   add_int(lcResultStruct,"line_type", CLIType.LineType).
   add_int(lcResultStruct,"fixed_line_type", CLIType.FixedLineType).
   add_double(lcResultStruct,"monthly_cost", CLIType.CommercialFee).
   add_boolean(lcResultStruct,"bundled", CLIType.BundleType).
   add_boolean(lcResultStruct,"dss2_compatible",
               LOOKUP(CLIType.CLIType, lcAllowedDSS2SubsType) > 0).
   add_boolean(lcResultStruct,"voip_compatible",
               LOOKUP(CLIType.CLIType,lcAllVoIPNativeBundles) > 0).

   IF CLIType.BaseBundle > "" THEN
   DO:
      add_double(lcResultStruct,"data_amount" , fGetSLAmount(CLIType.BaseBundle,{&DIAL_TYPE_GPRS} )).
      add_double(lcResultStruct,"voice_amount", fGetSLAmount(CLIType.BaseBundle,{&DIAL_TYPE_VOICE})).
   END.

   IF CLIType.UsageType = 1 THEN
      add_string(lcResultStruct,"usage_type", "voice").
   ELSE
      add_string(lcResultStruct,"usage_type", "data").

   add_string(lcResultStruct,"tariff_type"              , fTMSCodeName("CLIType","TariffType",STRING(CliType.TariffType))).
   add_string(lcResultStruct,"fixed_line_download_speed", CliType.FixedLineDownload).
   add_string(lcResultStruct,"fixed_line_upload_speed"  , CliType.FixedLineUpload).
   add_double(lcResultStruct,"fixed2fixed_voice_amount" , (IF lcFixedLineBB > "" THEN fGetSLAmount(CLIType.BaseBundle,{&DIAL_TYPE_FIXED_VOICE_BDEST} ) ELSE 0)).
   add_double(lcResultStruct,"fixed2mobile_voice_amount", (IF lcFixedLineBB > "" THEN fGetSLAmount(CLIType.BaseBundle,{&DIAL_TYPE_FIXED_VOICE}) ELSE 0)).
   add_string(lcResultStruct,"rateplan_id"              , CliType.Priceplan).

   lcRegionArray = add_array(lcResultStruct, "region").
   FOR EACH VATCode NO-LOCK WHERE
            VATCode.TaxClass  = "1"   AND
            VATCode.FromDate <= TODAY AND
            VATCOde.ToDate   >= TODAY:
      lcRegionStruct = add_struct(lcRegionArray, "").
      add_string(lcRegionStruct,"taxzone", VATCode.VCName).
      add_double(lcRegionStruct,"taxinclvalue", (1 + VatCode.VatPerc / 100) * CLIType.CommercialFee).
   END.
END.
