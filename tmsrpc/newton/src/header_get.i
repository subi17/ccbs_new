{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR Syst.CUICommon:gcBrand            AS CHAR      NO-UNDO INIT "1".
DEF VAR lcResultStruct     AS CHAR      NO-UNDO. 
DEF VAR pcId               AS CHAR      NO-UNDO. 
DEF VAR pcTenant           AS CHAR 		NO-UNDO.
DEF VAR pcIdArray          AS CHAR      NO-UNDO. 
DEF VAR liCounter          AS INTEGER   NO-UNDO. 
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.

pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").
