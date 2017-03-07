/**
 * Create duplicate invoice request
 *
 * @input int;mandatory;invoice number
          struct;mandatory;input data
 * @data username;string;mandatory;
         memo;struct;mandatory;
 * @memo title;string;mandatory;
         content;string;mandatory
 * @output boolean;true
  
*/

{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}
{Func/duplicate_invoice.i}
{Syst/tmsconst.i}

DEF VAR pcStruct AS CHAR NO-UNDO.
DEF VAR liRequestID AS INT  NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.
DEF VAR piInvNum AS INT NO-UNDO.

DEF VAR pcMemoStruct AS CHARACTER NO-UNDO. 
DEF VAR pcMemoTitle AS CHARACTER NO-UNDO. 
DEF VAR pcMemoContent AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,struct") = ? THEN RETURN.

piInvNum = get_int(param_toplevel_id,"0").
pcStruct = get_struct(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF validate_request(pcStruct, "username!,memo!") = ? THEN RETURN.

katun = "VISTA_" + get_string(pcStruct,"username").
pcMemoStruct = get_struct(pcStruct,"memo").
pcMemoTitle = get_string(pcMemoStruct,"title").
pcMemoContent = get_string(pcMemoStruct,"content").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

/* check invoice */
FIND Invoice WHERE 
     Invoice.InvNum = piInvNum NO-LOCK NO-ERROR.
IF NOT AVAIL Invoice THEN DO:
   RETURN appl_err("Invalid invoice number").
END.

liRequestID = fDuplicateInvoiceRequest
   (Invoice.InvNum,
    ?,  /* when request should be handled */
    "", /* creator */
    {&REQUEST_SOURCE_NEWTON},
    OUTPUT lcError).

IF liRequestID = 0 THEN DO:
   RETURN appl_err(lcError).
END.

DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "Invoice",
                 STRING(Invoice.InvNum),
                 Invoice.Custnum,
                 pcMemoTitle,
                 pcMemoContent).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
