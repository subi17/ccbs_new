/**
 * Return all devices for xfera
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR lcc AS CHAR NO-UNDO.

lcArray = add_array(response_toplevel_id, "").

FOR EACH BillItem NO-LOCK WHERE
        BillItem.Brand = "1" AND
        BillItem.BiGroup = "7":
  lcStruct = add_struct(lcArray, "").
  add_string(lcStruct, "id", BillItem.BillCode).
  lcc = BillItem.BIName.
  add_string(lcStruct, "manufacturer", ENTRY(1, lcc, " ")).
  add_string(lcStruct, "model", ENTRY(2, lcc, " ")).
  add_string(lcStruct, "color", ENTRY(3, lcc, " ")).
  add_int(lcStruct, "available", 1).
END.
