


{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEFINE VARIABLE liCustNum       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcStatus        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResultStruct  AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int,string") EQ ? THEN RETURN.
liCustNum = get_int(param_toplevel_id, "0").
lcStatus  = get_string(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcResultStruct = add_struct(response_toplevel_id, "").
add_string(lcResultStruct,"status","Emitida").

add_boolean(response_toplevel_id,?,TRUE).


