/**
 * Get discount plan ids.
 *
 * @input conditions;struct;mandatory;empty struct
 * @output struct;array of discount plan ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHAR NO-UNDO.

lcQuery = 'FOR EACH DiscountPlan NO-LOCK WHERE DiscountPlan.Brand = "1" '.

fListQuery(
   "DiscountPlan",
   lcQuery,
   "DPRuleId").
