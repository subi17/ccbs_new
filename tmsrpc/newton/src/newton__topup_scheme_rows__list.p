/**
 * Get topup scheme row ids.
 *
 * @input conditions;struct;mandatory;supports topup_scheme_id
 * @output struct;array of topup scheme row ids
*/

{newton/src/flistrpc.i}

lcStruct = validate_struct(pcStruct, "brand,topup_scheme_id").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH TopUpSchemeRow NO-LOCK WHERE TopUpSchemeRow.Brand = "1"'.

IF LOOKUP("topup_scheme_id",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + " AND TopUpSchemeRow.TopUpScheme = " + 
             QUOTER(get_string(pcStruct,"topup_scheme_id")).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

fListQuery(
   "TopUpSchemeRow",
   lcQuery,
   "TopupSchemeRowId").
