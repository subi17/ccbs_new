/**
 * Get discount plan ids.
 *
 * @input conditions;struct;mandatory;empty struct
 * @output struct;array of discount plan ids
*/

{newton/src/flistrpc.i}

DEF VAR lcQuery AS CHAR NO-UNDO.

lcStruct = validate_struct(pcStruct,"").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcQuery = 'FOR EACH DiscountPlan NO-LOCK WHERE DiscountPlan.Brand = "1" '.

fListQuery(
   "DiscountPlan",
   lcQuery,
   "DPRuleId").
