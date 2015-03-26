/**
 * Return all subscription_types for xfera
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR liDispOrder AS INT NO-UNDO.

lcArray = add_array(response_toplevel_id, "").
liDispOrder = 0.

FUNCTION create_subscription_type RETURN LOGICAL
      ( pcId AS CHAR,
        pcOrderItemType AS CHAR ):
    DEF VAR lcStruct AS CHAR NO-UNDO.

    lcStruct = add_struct(lcArray, "").
    add_string(lcStruct, "id", pcId).
    add_string(lcStruct, "order_item_type_id", pcOrderItemType).
    add_int(lcStruct, "disporder", liDispOrder).
    add_boolean(lcStruct, "available", true).
    liDispOrder = liDispOrder + 1.
END FUNCTION.

create_subscription_type("TARJ", "prepaid").
create_subscription_type("CONT", "postpaid").
