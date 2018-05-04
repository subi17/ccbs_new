


{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
liCustNum = get_int(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.


add_boolean(response_toplevel_id, "", TRUE).