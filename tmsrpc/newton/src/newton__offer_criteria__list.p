/**
 * Get offer criteria ids
 *
 * @input conditions;struct;mandatory;supports offer_id,criteria_type
 * @output struct;array of offer criteria ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant  AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!,offer_id,criteria_type").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH OfferCriteria NO-LOCK WHERE OfferCriteria.Brand = "1"'.

IF LOOKUP("offer_id",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + " AND OfferCriteria.Offer = " + 
             QUOTER(get_string(pcStruct,"offer_id")).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.
IF LOOKUP("criteria_type",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + " AND OfferCriteria.CriteriaType = " + 
             QUOTER(get_string(pcStruct,"criteria_type")).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

fListQuery(
   "OfferCriteria",
   lcQuery,
   "OfferCriteriaId").
