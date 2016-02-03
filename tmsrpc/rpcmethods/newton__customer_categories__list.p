/**
 * Get customer categories.
 *
 * @input struct;no contents
 * @output struct;array of customer categories
*/

{rpcmethods/flistrpc.i}

lcStruct = validate_struct(pcStruct, "").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH CustCat NO-LOCK WHERE CustCat.Brand = "1"'.

fListQuery(
   "CustCat",
   lcQuery,
   "Category").
