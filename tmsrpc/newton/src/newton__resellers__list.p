/** 
 * Get a list of resellers ids
 *
 * @input struct;mandatory;empty
 * @output array_of_string;reseller ids
*/
{newton/src/flistrpc.i}

lcStruct = validate_struct(pcStruct, "").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH Reseller NO-LOCK WHERE Reseller.Brand = ' + QUOTER(gcBrand).

fListQuery(
   "Reseller",
   lcQuery,
   "Reseller").

