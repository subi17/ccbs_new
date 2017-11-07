/**
 * Get FAT scheme ids.
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of fat scheme ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH FatGroup NO-LOCK WHERE FatGroup.Brand = "1"'.

fListQuery(
   "FatGroup",
   lcQuery,
   "FtGrp").
