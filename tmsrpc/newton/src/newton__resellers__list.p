/** 
 * Get a list of resellers ids
 *
 * @input struct;mandatory;empty
 * @output array_of_string;reseller ids
*/
{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH Reseller NO-LOCK WHERE Reseller.Brand = ' + QUOTER(gcBrand).

fListQuery(
   "Reseller",
   lcQuery,
   "Reseller").

