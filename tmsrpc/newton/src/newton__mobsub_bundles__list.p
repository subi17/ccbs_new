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
{Func/upsellcount.i}
{Mm/active_bundle.i}
{Mm/fbundle.i}
{Func/timestamp.i}
{Func/fixedlinefunc.i}

DEF VAR lcResultArray         AS CHAR NO-UNDO. 
DEF VAR pcStruct              AS CHAR NO-UNDO. 
DEF VAR lcStruct              AS CHAR NO-UNDO.
DEF VAR piMsSeq               AS INT  NO-UNDO.
DEF VAR liCount               AS INT  NO-UNDO.
DEF VAR lcBundle              AS CHAR NO-UNDO.
DEF VAR ldCurrentDateTime     AS DECI NO-UNDO.
DEF VAR llBundleActivated     AS LOGI NO-UNDO INIT FALSE.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_struct(pcStruct, "id!").
IF gi_xmlrpc_error NE 0 THEN RETURN.
 
piMsSeq = get_int(pcStruct,"id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

lcResultArray = add_array(response_toplevel_id, "").

ASSIGN ldCurrentDateTime = fMakeTS().

RUN pAdd_DSS.

RUN pAdd_DataBundle.

RUN pAdd_VoiceBundle.

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
        lcAllowedBONOContracts = fCParamC("ALLOWED_BONO_CONTRACTS").

    ASSIGN lcActiveBundles = fGetActiveDataBundle(Mobsub.MsSeq,ldCurrentDateTime).

    DO liCount = 1 TO NUM-ENTRIES(lcActiveBundles):
       
       lcActiveBundle = ENTRY(liCount,lcActiveBundles).
       
       IF LOOKUP(lcActiveBundle,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12") > 0 THEN
          add_string(lcResultArray,"", "TARJ7_UPSELL|" + STRING(Mobsub.MsSeq)).
       
       IF LOOKUP(lcActiveBundle, lcBONOContracts + "," + lcIPLContracts) = 0 THEN NEXT.
       
       IF LOOKUP(lcActiveBundle, lcBONOContracts) > 0 THEN 
          llActiveBonoContract = TRUE.
       
       add_string(lcResultArray,"", lcActiveBundle + "|" + STRING(Mobsub.MsSeq)).

       FIND FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand   = gcBrand        AND
                                            DayCampaign.DCEvent = lcActiveBundle NO-ERROR.
       IF NOT AVAIL DayCampaign OR DayCampaign.BundleUpsell EQ "" THEN 
          NEXT.

       DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):
          add_string(lcResultArray, "", ENTRY(liUpsellCount,DayCampaign.BundleUpsell) + "|" + STRING(Mobsub.MsSeq)).
       END.
       
    END.

    IF NOT llActiveBonoContract THEN 
    DO:
        FOR EACH DayCampaign NO-LOCK WHERE DayCampaign.Brand = gcBrand AND
                                    LOOKUP(DayCampaign.DCEvent,lcAllowedBONOContracts) > 0:

           IF NOT fIsBundleAllowed(Mobsub.CLIType,
                                   DayCampaign.DCEvent,
                                   OUTPUT lcError) THEN 
               NEXT.
          
           add_string(lcResultArray,"", DayCampaign.DCEvent + "|" + STRING(Mobsub.MsSeq) ).

           DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):

              IF LOOKUP(ENTRY(liUpsellCount,DayCampaign.BundleUpsell), lcUpsellList) = 0 THEN 
              DO:
                  lcUpsellList = lcUpsellList + (IF lcUpsellList <> "" THEN "," ELSE "") + ENTRY(liUpsellCount, DayCampaign.BundleUpsell).

                  add_string(lcResultArray,"", ENTRY(liUpsellCount, DayCampaign.BundleUpsell) + "|" + STRING(Mobsub.MsSeq)).
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
    DEF VAR llProSubscription     AS LOGI NO-UNDO.
    
    ASSIGN 
        llProSubscription     = fIsProSubscription(piMsSeq)
        lcPRODSSUpsellList    = fCParamC("PRO_DSS_FLEX_UPSELL_LIST")
        lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

    IF NOT MobSub.PayType THEN
        lcDSSBundleId = fGetActiveDSSId(INPUT MobSub.CustNum,INPUT ldCurrentDateTime).

    /* Return DSS bundle and upsell if DSS is active */
    IF lcDSSBundleId = {&DSS} OR 
       (lcDSSBundleId = "DSS2" AND LOOKUP(MobSub.CLIType, lcAllowedDSS2SubsType) > 0) THEN 
    DO:
        add_string(lcResultArray,"", lcDSSBundleId + "|" + STRING(Mobsub.MsSeq)).
        /*Find upsells and add all to reponse*/
        FIND FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand   = gcBrand       AND
                                             DayCampaign.DCEvent = lcDSSBundleId NO-ERROR.
        IF AVAIL DayCampaign AND DayCampaign.BundleUpsell > "" THEN 
        DO:
            DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):

                IF llProSubscription THEN
                DO:
                    IF NOT fIsBundleAllowed(Mobsub.CLIType,
                                            ENTRY(liUpsellCount,DayCampaign.BundleUpsell),
                                            OUTPUT lcMatrixAnalyseResult) THEN 
                        NEXT.
                END.
                ELSE IF LOOKUP(ENTRY(liUpsellCount,DayCampaign.BundleUpsell),lcPRODSSUpsellList) > 0 THEN 
                    NEXT.
                
                add_string(lcResultArray,"",ENTRY(liUpsellCount, DayCampaign.BundleUpsell) + "|" + STRING(Mobsub.MsSeq)).
            END.
        END.
    END.    

END PROCEDURE.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
