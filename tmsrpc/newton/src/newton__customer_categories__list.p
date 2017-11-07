/**
 * Get customer categories.
 *
 * @input struct;no contents
 * @output struct;array of customer categories
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery  AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH CustCat NO-LOCK WHERE CustCat.Brand = "1"'.

fListQuery(
   "CustCat",
   lcQuery,
   "Category").
