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
{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

/* Input parameters */
DEF VAR pcExtInviId AS CHAR NO-UNDO.

{commpaa.i}
ASSIGN
   katun = "IVR_" + gbAuthLog.EndUserId.
   gcBrand = "1".

{tmsconst.i}
{duplicate_invoice.i}

DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR liRequestId AS INT NO-UNDO.
DEF VAR liSubInvoices AS INT NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcExtInviId = get_string(param_toplevel_id,"0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* check invoice */
FIND FIRST Invoice WHERE 
           Invoice.Brand = gcBrand AND
           Invoice.ExtInvId = pcExtInviId NO-LOCK NO-ERROR.
IF NOT AVAIL Invoice THEN RETURN appl_err("Invoice not found").

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
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
