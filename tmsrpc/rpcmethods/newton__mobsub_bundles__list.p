/**
 * Returns a list of bundles allowed for this subscription id  

 * @input ;struct;mandatory;
          id;int;mandatory;subscription id
 * @output struct;array of bundle/upsell id
 */

{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{tmsconst.i}
{fbundle.i}

DEF VAR lcResultArray       AS CHAR NO-UNDO. 
DEF VAR pcStruct            AS CHAR NO-UNDO. 
DEF VAR lcStruct            AS CHAR NO-UNDO.
DEF VAR piMsSeq             AS INT  NO-UNDO.
DEF VAR lcActiveBundle      AS CHAR NO-UNDO. 
DEF VAR lcActiveBundles     AS CHAR NO-UNDO. 
DEF VAR lcError             AS CHAR NO-UNDO. 
DEF VAR liCount             AS INT  NO-UNDO. 
DEF VAR liUpsellCount       AS INT  NO-UNDO.
DEF VAR ldeCurrTS           AS DEC  NO-UNDO.
DEF VAR lcDSSBundleId       AS CHAR NO-UNDO.
DEF VAR llActiveBonoContract AS LOG  NO-UNDO.
DEF VAR lcAllowedBONOContracts   AS CHAR NO-UNDO.
DEF VAR lcBONOContracts          AS CHAR NO-UNDO.
DEF VAR lcIPLContracts           AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType    AS CHAR NO-UNDO.
DEF VAR lcDayCampBundleUpsells AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_struct(pcStruct, "id!").
IF gi_xmlrpc_error NE 0 THEN RETURN.
 
piMsSeq = get_int(pcStruct,"id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub WHERE
           MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR. 
IF NOT AVAIL MobSub THEN RETURN appl_err("Mobsub not found").

lcResultArray = add_array(response_toplevel_id, "").

ASSIGN ldeCurrTS = fMakeTS()
       lcAllowedBONOContracts = fCParamC("ALLOWED_BONO_CONTRACTS")
       lcBONOContracts        = fCParamC("BONO_CONTRACTS")
       lcIPLContracts         = fCParamC("IPL_CONTRACTS")
       lcAllowedDSS2SubsType  = fCParamC("DSS2_SUBS_TYPE").

IF NOT MobSub.PayType THEN
   lcDSSBundleId = fGetActiveDSSId(INPUT MobSub.CustNum,
                                   INPUT ldeCurrTS).

lcActiveBundles = fGetActiveDataBundle(Mobsub.MsSeq,ldeCurrTS).

DO liCount = 1 TO NUM-ENTRIES(lcActiveBundles):
   
   lcActiveBundle = ENTRY(liCount,lcActiveBundles).
   
   IF lcActiveBundle = "TARJ7" OR lcActiveBundle = "TARJ9" THEN
      add_string(lcResultArray,"", "TARJ7_UPSELL|" + STRING(Mobsub.MsSeq)).
   
   IF LOOKUP(lcActiveBundle, lcBONOContracts + "," + lcIPLContracts + ",BONO_VOIP") = 0 THEN NEXT.
   
   IF LOOKUP(lcActiveBundle, lcBONOContracts) > 0 THEN 
      llActiveBonoContract = TRUE.
   
   add_string(lcResultArray,"", lcActiveBundle + "|" + STRING(Mobsub.MsSeq)).

   FIND FIRST DayCampaign NO-LOCK WHERE
              DayCampaign.Brand = gcBrand AND
              DayCampaign.DCEvent = lcActiveBundle NO-ERROR.
   IF NOT AVAIL DayCampaign OR DayCampaign.BundleUpsell EQ "" THEN NEXT.

   DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):
      add_string(lcResultArray,"",
                 ENTRY(liUpsellCount,DayCampaign.BundleUpsell)
                 + "|" + STRING(Mobsub.MsSeq)).
   END.
END. /* DO liCount = 1 TO NUM-ENTRIES(lcActiveBundles): */
/*  before ILP    
/* Return DSS bundle and upsell if DSS is active */
IF lcDSSBundleId = {&DSS} OR
   (lcDSSBundleId = "DSS2" AND
    LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN DO:
   add_string(lcResultArray,"", lcDSSBundleId + "|" + STRING(Mobsub.MsSeq)).
   add_string(lcResultArray,"", lcDSSBundleId + "_UPSELL" + "|" +
              STRING(Mobsub.MsSeq)).
END. /* IF lcDSSBundleId = {&DSS} OR */
  before ILP ends */
/*ILP 
read from configuration instead of hard coding _UPSELL*/
/* Return DSS bundle and upsell if DSS is active */
IF lcDSSBundleId = {&DSS} OR
   (lcDSSBundleId = "DSS2" AND
    LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN DO:
   add_string(lcResultArray,"", lcDSSBundleId + "|" + STRING(Mobsub.MsSeq)).
   /*Find upsells and add all to reponse*/
   FIND FIRST DayCampaign NO-LOCK WHERE
              DayCampaign.Brand = gcBrand AND
              DayCampaign.DCEvent = lcDSSBundleId NO-ERROR.
   IF AVAIL DayCampaign AND NOT DayCampaign.BundleUpsell EQ "" THEN DO:
      DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):
         add_string(lcResultArray,"",
                    ENTRY(liUpsellCount,DayCampaign.BundleUpsell)
                    + "|" + STRING(Mobsub.MsSeq)).
      END.
   END.
END.
/*ILP ENDS*/
IF MobSub.CliType EQ "CONT15" AND
   fGetCurrentSpecificBundle(
      MobSub.MsSeq,
      "VOICE100") > "" THEN
   add_string(lcResultArray,"", "VOICE100|" + STRING(Mobsub.MsSeq)).

/* if subscription doesn't have a BONO bundle then return the list of 
   bundles that can be activated */
FOR EACH DayCampaign NO-LOCK WHERE 
         DayCampaign.Brand = gcBrand AND
  LOOKUP(DayCampaign.DCEvent,lcAllowedBONOContracts + ",BONO_VOIP") > 0:
   
   IF llActiveBonoContract AND
      LOOKUP(DayCampaign.DCEvent,lcAllowedBONOContracts) > 0 THEN NEXT.

   IF NOT fIsBundleAllowed
      (Mobsub.CLIType,
       DayCampaign.DCEvent,
       OUTPUT lcError) THEN NEXT.

   IF DayCampaign.DCEvent = "BONO_VOIP" AND
      NOT fIsBonoVoIPAllowed(Mobsub.MsSeq, ldeCurrTS) THEN NEXT.
   
   add_string(lcResultArray,"", DayCampaign.DCEvent + "|" + STRING(Mobsub.MsSeq) ).
   IF LOOKUP(DayCampaign.BundleUpsell,lcDayCampBundleUpsells) = 0 THEN DO:
      lcDayCampBundleUpsells = TRIM(lcDayCampBundleUpsells + "," + DayCampaign.BundleUpsell,",").

      DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):
         add_string(lcResultArray,"", 
                    ENTRY(liUpsellCount,DayCampaign.BundleUpsell) 
                    + "|" + STRING(Mobsub.MsSeq)).
      END.
   END.
END.


IF MobSub.CliType = "TARJ6" THEN
   add_string(lcResultArray,"", "TARJ_UPSELL|" + STRING(Mobsub.MsSeq)).
   
add_string(lcResultArray,"", "HSPA_ROAM_EU|" + STRING(Mobsub.MsSeq)).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
