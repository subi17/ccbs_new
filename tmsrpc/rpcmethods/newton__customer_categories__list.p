/**
 * Get customer categories.
 *
 * @input struct;no contents
 * @output struct;array of customer categories
*/

{flistrpc.i}

DEF VAR lcQuery  AS CHARACTER NO-UNDO. 
DEF VAR pcTenant AS CHARACTER NO-UNDO. 

lcStruct = validate_request(pcStruct, "brand!").
pcTenant = get_string(pcStruct, "brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{Syst/settenant.i pcTenant}

lcQuery = 'FOR EACH CustCat NO-LOCK WHERE CustCat.Brand = "1"'.

fListQuery(
   "CustCat",
   lcQuery,
   "Category").
