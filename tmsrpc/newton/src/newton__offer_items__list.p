/**
 * Get offer item ids.
 *
 * @input conditions;struct;mandatory;supports offer_id
 * @output struct;array of offer item ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!,offer_id").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH OfferItem NO-LOCK WHERE OfferItem.Brand = "1"'.

IF LOOKUP("offer_id",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + " AND OfferItem.Offer = " + 
             QUOTER(get_string(pcStruct,"offer_id")).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

fListQuery(
   "OfferItem",
   lcQuery,
   "OfferItemId").
