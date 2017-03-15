/**
 * Get offer ids.
 *
 * @input conditions;struct;mandatory;supports active
 * @output struct;array of offer ids
*/


{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!,active").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH Offer NO-LOCK WHERE Offer.Brand = "1"'.

IF LOOKUP("active",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + " AND Offer.Active = " + 
             QUOTER(get_bool(pcStruct,"active")).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

fListQuery(
   "Offer",
   lcQuery,
   "Offer").
