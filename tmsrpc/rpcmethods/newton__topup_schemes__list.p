/**
 * Get topup scheme ids.
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of topup scheme ids
*/

{flistrpc.i}

lcStruct = validate_struct(pcStruct, "").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH TopUpScheme NO-LOCK WHERE TopUpScheme.Brand = "1"'.

fListQuery(
   "TopUpScheme",
   lcQuery,
   "TopupScheme").
