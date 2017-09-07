/**
 * Test whether an order exists
 *
 * @input   orderid;int
 *
 * @output  exists;bool
 *
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/commpaa.i}
gcBrand = "1".


/* Input */
DEF VAR piOrderId AS INT NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piOrderId = get_int(param_toplevel_id, "0").
if gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

add_boolean(response_toplevel_id, "", AVAILABLE order).
