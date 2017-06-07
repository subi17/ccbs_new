&IF "{&BUNDLE_TYPE}" NE "YES" 
&THEN
&GLOBAL-DEFINE BUNDLE_TYPE YES

{Syst/tmsconst.i}
{Syst/commali.i}
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
      WHEN "BONO_VOIP"    THEN lcContracts = "BONO_VOIP".
      WHEN "HSPA_ROAM_EU" THEN lcContracts = "HSPA_ROAM_EU".
      WHEN "VOICE"       THEN lcContracts = "VOICE100,VOICE200". 
      WHEN "FREE100MINUTES" THEN lcContracts = "FREE100MINUTES".
      OTHERWISE RETURN "".
   END CASE. /* CASE icBundleType: */

   RETURN lcContracts.

END FUNCTION.

&ENDIF
