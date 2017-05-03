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

DEF VAR lcResultArray       AS CHAR NO-UNDO. 
DEF VAR pcStruct            AS CHAR NO-UNDO. 
DEF VAR lcStruct            AS CHAR NO-UNDO.
DEF VAR piMsSeq             AS INT  NO-UNDO.
DEF VAR liCount             AS INT  NO-UNDO.
DEF VAR lcBundle            AS CHAR NO-UNDO.
DEF VAR lcAllowedBundles    AS CHAR NO-UNDO.

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
    
    IF lcBundle = "" OR lcBundle = MobSub.CliType OR LOOKUP(lcBundle,"CONTDSL,CONTFH50,CONTFH300") > 0 OR LOOKUP(lcBundle,"MM_DATA600") > 0 THEN  /* Except mobile & fixedline base bundle. Temporaryly blocking MM_DATA600 */
        NEXT.
             
    add_string(lcResultArray,"", ENTRY(liCount,lcAllowedBundles) + "|" + STRING(Mobsub.MsSeq)).
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
