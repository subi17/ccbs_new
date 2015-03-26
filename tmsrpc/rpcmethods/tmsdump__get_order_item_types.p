/**
 * Return all order_item_types for xfera
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.

lcArray = add_array(response_toplevel_id, "").

lcStruct = add_struct(lcArray, "").
add_string(lcStruct, "id", "prepaid").

lcStruct = add_struct(lcArray, "").
add_string(lcStruct, "id", "postpaid").
