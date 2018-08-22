&IF "{&BUNDLE_TYPE}" NE "YES" 
&THEN
&GLOBAL-DEFINE BUNDLE_TYPE YES

{Syst/tmsconst.i}
{Func/cparam2.i}

FUNCTION fGetBundles RETURNS CHAR (icBundleType AS CHAR):

   DEF VAR lcContracts      AS CHAR NO-UNDO.

   CASE icBundleType:
      WHEN "BONO"         THEN lcContracts = fCParamC("BONO_CONTRACTS").
      WHEN "CONTRD"       THEN lcContracts = fCParamC("IPL_CONTRACTS").
      WHEN "CONTD"        THEN lcContracts = fCParamC("CONTD_CONTRACTS").
      WHEN "CONTF"        THEN lcContracts = fCParamC("FLAT_CONTRACTS").
      WHEN "CONTS"        THEN lcContracts = fCParamC("CONTS_CONTRACTS").
      WHEN "CONTSF"       THEN lcContracts = fCParamC("CONTSF_CONTRACTS").
      WHEN "DSS"          THEN lcContracts = {&DSS_BUNDLES}.
      WHEN "VOICE"       THEN lcContracts = "VOICE100,VOICE200,VOICE200B,MM_VOICE40,MM_VOICE100,MM_VOICE250". 
      WHEN "FREE100MINUTES" THEN lcContracts = "FREE100MINUTES".
      OTHERWISE RETURN "".
   END CASE. /* CASE icBundleType: */

   RETURN lcContracts.

END FUNCTION.

FUNCTION fConvBundleToBillItem RETURNS CHAR
   (icDataBundle AS CHAR):

   DEF BUFFER DayCampaign FOR DayCampaign.
   DEF BUFFER FeeModel FOR FeeModel.
   DEF BUFFER FMItem FOR FMItem.

   FOR FIRST DayCampaign WHERE
             DayCampaign.Brand   = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = icDataBundle NO-LOCK,
       FIRST FeeModel WHERE
             FeeModel.Brand    = Syst.Var:gcBrand AND
             FeeModel.FeeModel = DayCampaign.FeeModel NO-LOCK,
       FIRST FMItem WHERE
             FMItem.Brand     = Syst.Var:gcBrand AND
             FMItem.FeeModel  = FeeModel.FeeModel AND
             FMItem.FromDate <= TODAY AND
             FMItem.ToDate   >= TODAY NO-LOCK:
       RETURN FMItem.BillCode.
   END.
   RETURN icDataBundle.

END FUNCTION.

&ENDIF

