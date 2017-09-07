/**
 * Adding a single memo.
 *
 * @input   reference;string;mandatory;key of table referenced by type
            type;string;mandatory;type of the memo (mobsub, service, customer or invoice)
            custnum;int;mandatory;customer number if any
            creator;string;mandatory;id of logged in user
            title;string;mandatory;title of memo
            content;string;mandatory;contents of memo
            priority;int;mandatory;0 = not important, 1 = important
 * @output  success;boolean
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1} 

{Func/timestamp.i}
{Syst/commpaa.i}
gcBrand = "1".

/* Input parameters */
DEF VAR pcReference AS CHAR NO-UNDO.
DEF VAR pcType AS CHAR NO-UNDO.
DEF VAR piCustnum AS INT NO-UNDO.
DEF VAR pcCreator AS CHAR NO-UNDO.
DEF VAR pcTitle AS CHAR NO-UNDO.
DEF VAR pcContent AS CHAR NO-UNDO.
DEF VAR piOrdeRId AS INTEGER NO-UNDO.
DEFINE VARIABLE lcErrFile AS CHARACTER NO-UNDO. 
/* Local variables */
DEF VAR lcType AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR result AS LOGICAL.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piOrderId  = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

RUN Mc/sendorderreq.p(piOrderId, "antti.savolainen@qvantel.com", OUTPUT lcErrFile).

add_boolean(response_toplevel_id, "", true).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
