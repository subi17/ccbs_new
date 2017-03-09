/**
 * Return all devices for xfera
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

DEF VAR lcArray  AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR lcc      AS CHAR NO-UNDO.
DEF VAR pcTenant AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcArray = add_array(response_toplevel_id, "").

{newton/src/settenant.i pcTenant}

FOR EACH BillItem WHERE BillItem.Brand = "1" AND BillItem.BiGroup = "7" NO-LOCK:
  lcStruct = add_struct(lcArray, "").
  add_string(lcStruct, "id", BillItem.BillCode).
  lcc = BillItem.BIName.
  add_string(lcStruct, "manufacturer", ENTRY(1, lcc, " ")).
  add_string(lcStruct, "model", ENTRY(2, lcc, " ")).
  add_string(lcStruct, "color", ENTRY(3, lcc, " ")).
  add_int(lcStruct, "available", 1).
END.
