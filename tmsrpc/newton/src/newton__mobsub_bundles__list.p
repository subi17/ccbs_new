/**
 * Returns a list of bundles allowed for this subscription id  

 * @input ;struct;mandatory;
          id;int;mandatory;subscription id
 * @output struct;array of bundle/upsell id
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
/*
{Func/upsellcount.i}
{Mm/active_bundle.i}
{Mm/fbundle.i}
*/
DEF VAR lcResultArray       AS CHAR NO-UNDO. 
DEF VAR pcStruct            AS CHAR NO-UNDO. 
DEF VAR lcStruct            AS CHAR NO-UNDO.
DEF VAR piMsSeq             AS INT  NO-UNDO.
DEF VAR liCount             AS INT  NO-UNDO.
DEF VAR lcBundle            AS CHAR NO-UNDO.
DEF VAR lcAllowedBundles    AS CHAR NO-UNDO.
/*
DEF VAR lcActiveBundle      AS CHAR NO-UNDO. 
DEF VAR lcActiveBundles     AS CHAR NO-UNDO. 
DEF VAR lcError             AS CHAR NO-UNDO.  
DEF VAR liUpsellCount       AS INT  NO-UNDO.
DEF VAR ldeCurrTS           AS DEC  NO-UNDO.
DEF VAR lcDSSBundleId       AS CHAR NO-UNDO.
DEF VAR llActiveBonoContract AS LOG  NO-UNDO.
DEF VAR lcAllowedBONOContracts   AS CHAR NO-UNDO.
DEF VAR lcBONOContracts          AS CHAR NO-UNDO.
DEF VAR lcIPLContracts           AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType    AS CHAR NO-UNDO.
DEF VAR lcDayCampBundleUpsells   AS CHAR NO-UNDO. 
DEF VAR lcVoiceBundles           AS CHAR NO-UNDO. 
DEF VAR lcUpsellBundles          AS CHAR NO-UNDO.
DEF VAR liVoiceCount             AS INTE NO-UNDO.
*/

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_struct(pcStruct, "id!").
IF gi_xmlrpc_error NE 0 THEN RETURN.
 
piMsSeq = get_int(pcStruct,"id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

lcResultArray = add_array(response_toplevel_id, "").

lcAllowedBundles = fGetAllowedBundlesForSubscriptionType(MobSub.CliType).

IF lcAllowedBundles > "" THEN
DO liCount = 1 TO NUM-ENTRIES(lcAllowedBundles):
    
    ASSIGN lcBundle = ENTRY(liCount,lcAllowedBundles).
    
    IF lcBundle = "" OR lcBundle = MobSub.CliType THEN  /* Except Base bundle */
        NEXT.
             
    add_string(lcResultArray,"", ENTRY(liCount,lcAllowedBundles) + "|" + STRING(Mobsub.MsSeq)).
END.

/*
ASSIGN ldeCurrTS = fMakeTS()
       lcAllowedBONOContracts = fCParamC("ALLOWED_BONO_CONTRACTS")
       lcBONOContracts        = fCParamC("BONO_CONTRACTS")
       lcIPLContracts         = fCParamC("IPL_CONTRACTS")
       lcAllowedDSS2SubsType  = fCParamC("DSS2_SUBS_TYPE").

/* All DATA related bundles including BASE, BONO's & Additional Data Bundles */
lcActiveBundles = fGetActiveDataBundle(Mobsub.MsSeq,ldeCurrTS).

/* Only BONO's, Additional Data bundles and IPL Contracts */
DO liCount = 1 TO NUM-ENTRIES(lcActiveBundles):
   
   lcActiveBundle = ENTRY(liCount,lcActiveBundles).
   
   IF lcActiveBundle = "TARJ7" OR lcActiveBundle = "TARJ9" THEN
      add_string(lcResultArray,"", "TARJ7_UPSELL|" + STRING(Mobsub.MsSeq)).

   IF vcTenant = {&TENANT_YOIGO} THEN
   DO:
       IF LOOKUP(lcActiveBundle, lcBONOContracts + "," + lcIPLContracts + ",BONO_VOIP") = 0 THEN 
           NEXT.
   END.
   ELSE IF MobSub.CliType = lcActiveBundle THEN  
       NEXT.
   
   IF LOOKUP(lcActiveBundle, lcBONOContracts) > 0 THEN 
      llActiveBonoContract = TRUE.
   
   add_string(lcResultArray,"", lcActiveBundle + "|" + STRING(Mobsub.MsSeq)).

   FIND FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand = gcBrand AND DayCampaign.DCEvent = lcActiveBundle NO-ERROR.
   IF NOT AVAIL DayCampaign OR DayCampaign.BundleUpsell EQ "" THEN 
       NEXT.

   DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):
      IF LOOKUP(ENTRY(liUpsellCount,DayCampaign.BundleUpsell), lcUpsellBundles) = 0 THEN 
          ASSIGN lcUpsellBundles = lcUpsellBundles + (IF lcUpsellBundles <> "" THEN "," ELSE "") + ENTRY(liUpsellCount,DayCampaign.BundleUpsell).
   END.    
   
END. /* DO liCount = 1 TO NUM-ENTRIES(lcActiveBundles): */

DO liUpsellCount = 1 TO NUM-ENTRIES(lcUpsellBundles):
    add_string(lcResultArray,"",ENTRY(liUpsellCount,lcUpsellBundles) + "|" + STRING(Mobsub.MsSeq)).
END.

/* For postpaid, return DSS bundle and upsell if DSS is active */
IF NOT MobSub.PayType THEN
DO:
    lcDSSBundleId = fGetActiveDSSId(INPUT MobSub.CustNum, INPUT ldeCurrTS).

    IF lcDSSBundleId = {&DSS} OR (lcDSSBundleId = "DSS2" AND LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN 
    DO:
       add_string(lcResultArray,"", lcDSSBundleId + "|" + STRING(Mobsub.MsSeq)).

       /*Find upsells and add all to reponse*/
       FIND FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand = gcBrand AND DayCampaign.DCEvent = lcDSSBundleId NO-ERROR.
       IF AVAIL DayCampaign AND NOT DayCampaign.BundleUpsell EQ "" THEN 
       DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):
           add_string(lcResultArray,"",ENTRY(liUpsellCount,DayCampaign.BundleUpsell) + "|" + STRING(Mobsub.MsSeq)).  
       END.
    END.
END.

/* Voice bundles */
IF vcTenant = {&TENANT_YOIGO} THEN
DO:
    IF MobSub.CliType EQ "CONT15" AND fGetCurrentSpecificBundle(MobSub.MsSeq,"VOICE100") > "" THEN
        add_string(lcResultArray,"", "VOICE100|" + STRING(Mobsub.MsSeq)).
    ELSE IF (MobSub.CliType EQ "CONT9" OR MobSub.CliType EQ "CONT10") AND fGetCurrentSpecificBundle(MobSub.MsSeq,"FREE100MINUTES") > "" THEN
        add_string(lcResultArray,"", "FREE100MINUTES|" + STRING(Mobsub.MsSeq)).
END.
ELSE 
DO:
    FOR EACH bf_Matrix WHERE bf_Matrix.Brand = gcBrand AND bf_Matrix.MXKey = "PERCONTR" NO-LOCK By bf_Matrix.Prior:

        IF bf_Matrix.MXRes <> 1 THEN
            NEXT.

        FOR EACH bf_MxItem WHERE bf_MxItem.MxSeq = bf_Matrix.MxSeq AND bf_MxItem.MxName = "SubsTypeTo" AND bf_MxItem.MxValue = MobSub.CliType NO-LOCK:             
            FOR EACH MxItem WHERE MxItem.MxSeq = bf_MxItem.MxSeq AND MxItem.MXName = "PerContract" NO-LOCK:
                
                FIND FIRST DayCampaign WHERE Daycampaign.Brand = gcBrand AND Daycampaign.DCEvent = MxItem.MxValue NO-LOCK NO-ERROR.
                IF AVAIL DayCampaign AND DayCampaign.DcType = "1" AND DayCampaign.CCN <> 93 AND LOOKUP(DayCampaign.DCEvent, lcVoiceBundles) = 0 THEN
                    ASSIGN lcVoiceBundles = lcVoiceBundles + (IF lcVoiceBundles <> "" THEN "," ELSE "") + DayCampaign.DCEvent.
            END.
        END.
    END.
        
    DO liVoiceCount = 1 TO NUM-ENTRIES(lcVoiceBundles):
        IF fGetCurrentSpecificBundle(MobSub.MsSeq, ENTRY(liVoiceCount,lcVoiceBundles)) > "" THEN
            add_string(lcResultArray,"", (ENTRY(liVoiceCount,lcVoiceBundles) + "|" + STRING(Mobsub.MsSeq))).
    END.
END.

/* if subscription doesn't have a BONO bundle then return the list of bundles that can be activated */
FOR EACH DayCampaign NO-LOCK WHERE DayCampaign.Brand = gcBrand AND LOOKUP(DayCampaign.DCEvent,lcAllowedBONOContracts + ",BONO_VOIP") > 0:
   
   IF llActiveBonoContract AND LOOKUP(DayCampaign.DCEvent,lcAllowedBONOContracts) > 0 THEN 
       NEXT.

   IF NOT fIsBundleAllowed(Mobsub.CLIType,DayCampaign.DCEvent,OUTPUT lcError) THEN 
       NEXT.

   IF DayCampaign.DCEvent = "BONO_VOIP" AND NOT fIsBonoVoIPAllowed(Mobsub.MsSeq, ldeCurrTS) THEN 
       NEXT.
   
   add_string(lcResultArray,"", DayCampaign.DCEvent + "|" + STRING(Mobsub.MsSeq)).

   DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):
      IF LOOKUP(ENTRY(liUpsellCount,DayCampaign.BundleUpsell),lcDayCampBundleUpsells) = 0 THEN 
      DO:
          lcDayCampBundleUpsells = TRIM(lcDayCampBundleUpsells + "," + ENTRY(liUpsellCount,DayCampaign.BundleUpsell),",").

          add_string(lcResultArray,"",ENTRY(liUpsellCount,DayCampaign.BundleUpsell) + "|" + STRING(Mobsub.MsSeq)).
      END.
   END.

END.


IF MobSub.CliType = "TARJ6" THEN
   add_string(lcResultArray,"", "TARJ_UPSELL|" + STRING(Mobsub.MsSeq)).
   
add_string(lcResultArray,"", "HSPA_ROAM_EU|" + STRING(Mobsub.MsSeq)).
*/
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
