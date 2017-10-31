/**
 * Request for official paper invoice copy (duplicate invoice)
 *
 * @input       string;mandatory;invoice number
 * @output      boolean;true
 *
 * @Exceptions  1;Invoice not found
                2;Invoice includes multiple subscriptions
                3;Wrong invoice type
                4;Invoice is not delivered
                5;Invoice printing is denied
                6;Incorrect invoice due date
                7;Customer was not found
                8;Function not allowed due to existing requests
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.

/* Input parameters */
DEF VAR pcExtInviId AS CHAR NO-UNDO.

{Syst/commpaa.i}
ASSIGN
   katun = "IVR_" + ghAuthLog::EndUserId.
   Syst.CUICommon:gcBrand = "1".

{Syst/tmsconst.i}
{Func/duplicate_invoice.i}

DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR liRequestId AS INT NO-UNDO.
DEF VAR liSubInvoices AS INT NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcExtInviId = get_string(param_toplevel_id,"0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i YES Common Invoice ExtInvId pcExtInviId}

FOR EACH SubInvoice WHERE
         SubInvoice.InvNum = Invoice.InvNum NO-LOCK:
   liSubInvoices = liSubInvoices + 1.
END.
IF liSubInvoices > 1 THEN
   RETURN appl_err("Invoice includes multiple subscriptions").

liRequestID = fDuplicateInvoiceRequest
   (Invoice.InvNum,
    ?,  /* when request should be handled */
    "", /* creator */
    {&REQUEST_SOURCE_EXTERNAL_API},
    OUTPUT lcError).

IF liRequestID = 0 THEN RETURN appl_err(lcError).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   END.
