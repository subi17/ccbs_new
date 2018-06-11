/**
 * Returns a list of bundles allowed for this subscription id  

 * @input ;struct;mandatory;
          id;int;mandatory;subscription id
 * @output struct;array of bundle/upsell id
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/upsellcount.i}
{Mm/active_bundle.i}
{Mm/fbundle.i}
{Func/fixedlinefunc.i}
{Func/bundleupsells.i}

DEF VAR lcResultArray         AS CHAR NO-UNDO. 
DEF VAR pcStruct              AS CHAR NO-UNDO. 
DEF VAR lcStruct              AS CHAR NO-UNDO.
DEF VAR piMsSeq               AS INT  NO-UNDO.
DEF VAR liCount               AS INT  NO-UNDO.
DEF VAR lcBundle              AS CHAR NO-UNDO.
DEF VAR ldCurrentDateTime     AS DECI NO-UNDO.
DEF VAR llBundleActivated     AS LOGI NO-UNDO INIT FALSE.
DEF VAR lcBundleUpsells       AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_struct(pcStruct, "id!").
IF gi_xmlrpc_error NE 0 THEN RETURN.
 
piMsSeq = get_int(pcStruct,"id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

lcResultArray = add_array(response_toplevel_id, "").

ASSIGN ldCurrentDateTime = Func.Common:mMakeTS().

RUN pAdd_DSS.

RUN pAdd_DataBundle.

RUN pAdd_VoiceBundle.

RUN pAdd_3Gg_flex_upsell. /* YCO-276 */

RUN pAdd_5Gg_flex_upsell. /* YCO-275 */

PROCEDURE pAdd_DataBundle:
    DEF VAR liCount                AS INT  NO-UNDO.
    DEF VAR lcError                AS CHAR NO-UNDO.
    DEF VAR lcActiveBundle         AS CHAR NO-UNDO.
    DEF VAR lcActiveBundles        AS CHAR NO-UNDO.
    DEF VAR lcBONOContracts        AS CHAR NO-UNDO.
    DEF VAR lcAllowedBONOContracts AS CHAR NO-UNDO.
    DEF VAR lcIPLContracts         AS CHAR NO-UNDO.
    DEF VAR lcUpsellList           AS CHAR NO-UNDO.
    DEF VAR liUpsellCount          AS INTE NO-UNDO.
    DEF VAR llActiveBonoContract   AS LOG  NO-UNDO.

    ASSIGN 
        lcBONOContracts = fCParamC("BONO_CONTRACTS")
        lcIPLContracts  = fCParamC("IPL_CONTRACTS")
        lcAllowedBONOContracts = fCParamC("ALLOWED_BONO_CONTRACTS")
        lcBundleUpsells        = "".

    ASSIGN lcActiveBundles = fGetActiveDataBundle(Mobsub.MsSeq,ldCurrentDateTime).

    DO liCount = 1 TO NUM-ENTRIES(lcActiveBundles):
       
       lcActiveBundle = ENTRY(liCount,lcActiveBundles).
       
       IF LOOKUP(lcActiveBundle,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12") > 0 THEN
          add_string(lcResultArray,"", "TARJ7_UPSELL|" + STRING(Mobsub.MsSeq)).
       
       IF LOOKUP(lcActiveBundle, lcBONOContracts + "," + lcIPLContracts) = 0 THEN NEXT.
       
       IF LOOKUP(lcActiveBundle, lcBONOContracts) > 0 THEN 
          llActiveBonoContract = TRUE.
       
       add_string(lcResultArray,"", lcActiveBundle + "|" + STRING(Mobsub.MsSeq)).

       FIND FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand   = Syst.Var:gcBrand        AND
                                            DayCampaign.DCEvent = lcActiveBundle NO-ERROR.
       IF NOT AVAIL DayCampaign 
       THEN NEXT.
                 
       ASSIGN lcBundleUpsells = fGetDayCampaignUpsells(DayCampaign.DCEvent).
       
       IF lcBundleUpsells = "" THEN NEXT.
       
       DO liUpsellCount = 1 TO NUM-ENTRIES(lcBundleUpsells):
          add_string(lcResultArray, "", ENTRY(liUpsellCount,lcBundleUpsells) + "|" + STRING(Mobsub.MsSeq)).
       END.
       
    END.

    IF NOT llActiveBonoContract THEN 
    DO:
        FOR EACH DayCampaign NO-LOCK WHERE DayCampaign.Brand = Syst.Var:gcBrand AND
                                    LOOKUP(DayCampaign.DCEvent,lcAllowedBONOContracts) > 0:

           IF NOT fIsBundleAllowed(Mobsub.CLIType,
                                   DayCampaign.DCEvent,
                                   OUTPUT lcError) THEN 
               NEXT.
          
           add_string(lcResultArray,"", DayCampaign.DCEvent + "|" + STRING(Mobsub.MsSeq) ).
           
            ASSIGN 
                lcBundleUpsells = fGetDayCampaignUpsells(DayCampaign.DCEvent).

           DO liUpsellCount = 1 TO NUM-ENTRIES(lcBundleUpsells):

              IF LOOKUP(ENTRY(liUpsellCount,lcBundleUpsells), lcUpsellList) = 0 THEN 
              DO:
                  lcUpsellList = lcUpsellList + (IF lcUpsellList <> "" THEN "," ELSE "") + ENTRY(liUpsellCount, lcBundleUpsells).

                  add_string(lcResultArray,"", ENTRY(liUpsellCount, lcBundleUpsells) + "|" + STRING(Mobsub.MsSeq)).
              END.
           END.
        END.
    END.

    IF MobSub.CliType = "TARJ6" THEN
        add_string(lcResultArray,"", "TARJ_UPSELL|" + STRING(Mobsub.MsSeq)).

END PROCEDURE.

PROCEDURE pAdd_VoiceBundle:
    
    DEFINE VARIABLE lcVoiceBundle AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcError       AS CHARACTER NO-UNDO.    
    lcVoiceBundle = fGetCurrentSpecificBundle(MobSub.MsSeq, "VOICE").

    IF lcVoiceBundle > "" THEN
       add_string(lcResultArray,"", lcVoiceBundle + "|" + STRING(Mobsub.MsSeq)).
    IF fIsBundleAllowed(Mobsub.CLIType,
                        "VOICE200B",
                        OUTPUT lcError) THEN
       add_string(lcResultArray,"", "VOICE200B" + "|" + STRING(Mobsub.MsSeq)).
    
    IF LOOKUP(MobSub.CliType,"CONT9,CONT10") > 0 AND 
       fGetCurrentSpecificBundle(MobSub.MsSeq, "FREE100MINUTES") > "" THEN
        add_string(lcResultArray,"", "FREE100MINUTES|" + STRING(Mobsub.MsSeq)).
    

END PROCEDURE.

PROCEDURE pAdd_DSS:
    DEF VAR liUpsellCount         AS INTE NO-UNDO.
    DEF VAR lcDSSBundleId         AS CHAR NO-UNDO.
    DEF VAR lcPRODSSUpsellList    AS CHAR NO-UNDO.
    DEF VAR lcMatrixAnalyseResult AS CHAR NO-UNDO.
    DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
    DEF VAR lcAllowedDSS4SubsType AS CHAR NO-UNDO.   
    DEF VAR llProSubscription     AS LOGI NO-UNDO.
    DEF VAR lcUpsell              AS CHAR NO-UNDO.

    ASSIGN 
        llProSubscription     = fIsProSubscription(piMsSeq)
        lcPRODSSUpsellList    = fCParamC("PRO_DSS_FLEX_UPSELL_LIST")
        lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
        lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
        lcBundleUpsells       = "".

    IF NOT MobSub.PayType THEN
        lcDSSBundleId = fGetActiveDSSId(INPUT MobSub.CustNum,INPUT ldCurrentDateTime).

    /* Return DSS bundle and upsell if DSS is active */
    IF lcDSSBundleId EQ {&DSS} OR 
      (lcDSSBundleId EQ {&DSS2} AND LOOKUP(MobSub.CLIType, lcAllowedDSS2SubsType) > 0) OR 
      (lcDSSBundleId EQ {&DSS4} AND LOOKUP(MobSub.CLIType, lcAllowedDSS4SubsType) > 0) THEN
    DO:
        add_string(lcResultArray,"", lcDSSBundleId + "|" + STRING(Mobsub.MsSeq)).
        /*Find upsells and add all to reponse*/
        FIND FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand   = Syst.Var:gcBrand       AND
                                             DayCampaign.DCEvent = lcDSSBundleId NO-ERROR.
        IF AVAIL DayCampaign THEN 
        DO:            
            ASSIGN 
                lcBundleUpsells = fGetDayCampaignUpsells(DayCampaign.DCEvent).
            
            DO liUpsellCount = 1 TO NUM-ENTRIES(lcBundleUpsells):

               lcUpsell = ENTRY(liUpsellCount,lcBundleUpsells).

               IF LOOKUP(lcUpsell, lcPRODSSUpsellList) > 0 THEN DO:
                  IF llProSubscription THEN
                  DO:
                      IF NOT fIsBundleAllowed(Mobsub.CLIType,
                                              lcUpsell,
                                              OUTPUT lcMatrixAnalyseResult) THEN
                         NEXT.
                  END.
                  ELSE NEXT.
               END.

               add_string(lcResultArray,"", lcUpsell + "|" + STRING(Mobsub.MsSeq)).
            END.
        END.
    END.    

END PROCEDURE.

/* YCO-276 Returning bundle list for 3Gb flex upsell compatible tariffs */
PROCEDURE pAdd_3Gg_flex_upsell:      
    DEF VAR liUpsellCount         AS INTE NO-UNDO.
    DEF VAR lcUpsell              AS CHAR NO-UNDO.
    DEF VAR lcResult              AS CHAR NO-UNDO.
    DEF VAR lcUpsell_Id           AS CHAR NO-UNDO.
    DEF VAR lcclitype             AS CHAR NO-UNDO. /* YCO-457 */
    DEF VAR lcBundleCLITypes      AS CHAR NO-UNDO. /* YCO-457 */
 
    lcUpsell_Id  = fCParamC("RETENTION_3GB_UPSELLS").  
    lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES"). /* YCO-457 */

    /* YCO-457
       - some old legacy tariffs store the tariff in mobsub.tariffbundle rather than mobsub.clitype
       - The compatibility matrix have the specific tariffs rather than the "families" 
         because not all members of a "family" are compatible. So I have to pass the
         tariffbundle that contains the specific tariff to the function in charge of the validation */         
    IF LOOKUP(Mobsub.CliType,lcBundleCLITypes) > 0 THEN
        lcclitype = Mobsub.tariffbundle.
    ELSE 
        lcclitype = Mobsub.CliType.      
    /* YCO-457 end */
    
    DO liUpsellCount = 1 TO NUM-ENTRIES(lcUpsell_Id):
       
       lcUpsell = ENTRY(liUpsellCount,lcUpsell_Id).
             
       IF fMatrixAnalyse(Syst.Var:gcBrand,
                         "PERCONTR",
                         "PerContract;SubsTypeTo",
                         lcUpsell + ";" + lcclitype, /* YCO-457 */
                         OUTPUT lcResult) NE 1 AND
          ENTRY(1,lcResult,";") NE "?" THEN 
          NEXT.             
       
       IF INDEX(lcUpsell,lcResultArray) = 0 THEN
          add_string(lcResultArray,"", lcUpsell + "|" + STRING(Mobsub.MsSeq)).
    
    END.    

END PROCEDURE.
/* YCO-276 end */

/* YCO-275 Returning bundle list for 5Gb flex upsell compatible tariffs */
PROCEDURE pAdd_5Gg_flex_upsell:      
    DEF VAR liUpsellCount         AS INTE NO-UNDO.
    DEF VAR lcUpsell              AS CHAR NO-UNDO.
    DEF VAR lcResult              AS CHAR NO-UNDO.
    DEF VAR lcUpsell_Id           AS CHAR NO-UNDO.
    DEF VAR lcclitype             AS CHAR NO-UNDO. /* YCO-457 */
    DEF VAR lcBundleCLITypes      AS CHAR NO-UNDO. /* YCO-457 */
 
    lcUpsell_Id  = fCParamC("RETENTION_5GB_UPSELLS"). 
    lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES"). /* YCO-457 */

    /* YCO-457
       - some old legacy tariffs store the tariff in mobsub.tariffbundle rather than mobsub.clitype
       - The compatibility matrix have the specific tariffs rather than the "families"
         because not all members of a "family" are compatible. So I have to pass the
         tariffbundle that contains the specific tariff to the function in charge of the validation */
    IF LOOKUP(Mobsub.CliType,lcBundleCLITypes) > 0 THEN
        lcclitype = Mobsub.tariffbundle.
    ELSE 
        lcclitype = Mobsub.CliType.      
    /* YCO-457 end */
    
    DO liUpsellCount = 1 TO NUM-ENTRIES(lcUpsell_Id):
       
       lcUpsell = ENTRY(liUpsellCount,lcUpsell_Id).

                    
       IF fMatrixAnalyse(Syst.Var:gcBrand,
                         "PERCONTR",
                         "PerContract;SubsTypeTo",
                         lcUpsell + ";" + lcclitype,  /* YCO-457 */
                         OUTPUT lcResult) NE 1 AND
          ENTRY(1,lcResult,";") NE "?" THEN 
          NEXT.             
       
       IF INDEX(lcUpsell,lcResultArray) = 0 THEN
          add_string(lcResultArray,"", lcUpsell + "|" + STRING(Mobsub.MsSeq)).
    
    END.    

END PROCEDURE.
/* YCO-275 end */

FINALLY:
   END.
