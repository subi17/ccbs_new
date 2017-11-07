/**
 * Get bundle ids.
 *
 * @output struct;array of bundle ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO.

lcQuery = 'FOR EACH DayCampaign NO-LOCK WHERE
                    DayCampaign.Brand = "1" AND
                    LOOKUP(DayCampaign.DCType,"1,4,6,7,8") > 0 AND
                    DayCampaign.StatusCode > 0'.

fListQuery("DayCampaign",lcQuery,"DCEvent").
