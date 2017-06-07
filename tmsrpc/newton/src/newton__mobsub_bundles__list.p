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
*/
{Mm/fbundle.i}
{Func/timestamp.i}

DEF VAR lcResultArray       AS CHAR NO-UNDO. 
DEF VAR pcStruct            AS CHAR NO-UNDO. 
DEF VAR lcStruct            AS CHAR NO-UNDO.
DEF VAR piMsSeq             AS INT  NO-UNDO.
DEF VAR liCount             AS INT  NO-UNDO.
DEF VAR lcBundle            AS CHAR NO-UNDO.
DEF VAR lcAllowedBundles    AS CHAR NO-UNDO.
DEF VAR ldCurrentDateTime   AS DECI NO-UNDO.
DEF VAR llBundleActivated   AS LOGI NO-UNDO INIT FALSE.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_struct(pcStruct, "id!").
IF gi_xmlrpc_error NE 0 THEN RETURN.
 
piMsSeq = get_int(pcStruct,"id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

lcResultArray = add_array(response_toplevel_id, "").

ASSIGN
	ldCurrentDateTime = fMakeTS()
	lcAllowedBundles  = fGetAllowedBundlesForSubscriptionType(MobSub.CliType).

IF lcAllowedBundles > "" THEN
DO liCount = 1 TO NUM-ENTRIES(lcAllowedBundles):
    
    ASSIGN 
    	lcBundle 		  = ENTRY(liCount,lcAllowedBundles)
    	llBundleActivated = FALSE.

    FIND FIRST DayCampaign WHERE DayCampaign.Brand = gcBrand AND DayCampaign.DCEvent = lcBundle NO-LOCK NO-ERROR.
    IF AVAIL DayCampaign THEN
    DO:
    	/* This is to decide whether bundle is already active on subscription */
    	IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_DCCLI_DCTYPE}) > 0 THEN  
    	DO:
    		FIND FIRST DCCli WHERE DCCli.Brand = gcBrand AND DCCli.DCEvent = DayCampaign.DCEvent AND DCCli.MsSeq = MobSub.MsSeq AND DCCli.ValidTo >= TODAY NO-LOCK NO-ERROR.	
    		IF AVAIL DCCLi THEN 
    			ASSIGN llBundleActivated = TRUE.
    	END.		
    	ELSE 
    	DO:
    		FOR FIRST ServiceLimit WHERE ServiceLimit.GroupCode = DayCampaign.DcEvent NO-LOCK, 
    			FIRST mServiceLimit WHERE mServiceLimit.MsSeq    = MobSub.MsSeq 		 AND 
    									  mServiceLimit.DialType = ServiceLimit.DialType AND 
    									  mServiceLimit.SLSeq    = ServiceLimit.SLSeq	 AND
    									  mServiceLimit.EndTS   >= ldCurrentDateTime     NO-LOCK:
    			ASSIGN llBundleActivated = TRUE.						  
			END.    									   
		END.	
    END.

    FIND FIRST CLIType WHERE CLIType.Brand = gcBrand AND CLIType.CLIType = lcBundle NO-LOCK NO-ERROR.

    IF lcBundle = "" 			 						  							OR 
       lcBundle = MobSub.CliType 						  							OR /* Mobile base bundles */
       (AVAIL CliType AND CliType.BundleType = TRUE)      							OR /* Reject family tariffs from viewing in Newton */		 
       (AVAIL DayCampaign AND DayCampaign.StatusCode = 0) 							OR /* Reject inactive bundles from viewing in Newton */
       (AVAIL DayCampaign AND DayCampaign.StatusCode = 2 AND NOT llBundleActivated) OR /* Non-activated retired bundles on subscription are skipped from Newton view */
       LOOKUP(lcBundle,"CONTDSL,CONTFH50,CONTFH300") > 0  							OR /* Fixedline base bundles */
       LOOKUP(lcBundle,"MM_DATA600") > 0 				  							THEN  /* Default package of Masmovil, MM_DATA600 */
        NEXT.
             
    add_string(lcResultArray,"", ENTRY(liCount,lcAllowedBundles) + "|" + STRING(Mobsub.MsSeq)).
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
