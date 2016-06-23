/**
 * RPC to return Invoice Rows for a specific subinvoice of an invoice.
 *
 * @input: invnum;int;mandatory;invoice number
           sub_invoice;int;mandatory;SubInvoice number
           language;int;mandatory;Needed for Billing code translation name
 * @output InvRows;array;containing invrow details
 * @struct inv_row;int;
           billcode;string;
           billcode_name;string;Billing code translation name
           amount;double; (VAT excluded amount)
           vat_per;double
           credited;boolean;
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/transname.i}

/* Input parameters */
DEF VAR piInvoiceNum     AS INT  NO-UNDO.
DEF VAR piSubInvoiceNum  AS INT  NO-UNDO.
DEF VAR piLanguage       AS INT  NO-UNDO.
/* Output parameters */
DEF VAR top_array        AS CHAR NO-UNDO.
DEF VAR invrow_struct    AS CHAR NO-UNDO.
DEF VAR lcBillCodeName   AS CHAR NO-UNDO.

DEF BUFFER bInvoice      FOR Invoice.
DEF BUFFER bSubInvoice   FOR SubInvoice.
DEF BUFFER bInvRow       FOR InvRow.

IF validate_request(param_toplevel_id, "int,int,int") EQ ? THEN RETURN.
piInvoiceNum    = get_int(param_toplevel_id, "0").
piSubInvoiceNum = get_int(param_toplevel_id, "1").
piLanguage      = get_int(param_toplevel_id, "2").
IF piInvoiceNum = ? OR piInvoiceNum = 0 OR
   piSubInvoiceNum = ? OR piSubInvoiceNum = 0 THEN RETURN.

FIND Invoice WHERE
     Invoice.Brand = "1" AND
     Invoice.InvNum = piInvoiceNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Invoice THEN
   RETURN appl_err("Invoice not found: " + STRING(piInvoiceNum)).

FIND FIRST SubInvoice WHERE
           SubInvoice.InvNum    = piInvoiceNum AND
           SubInvoice.subinvnum = piSubInvoiceNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE SubInvoice THEN
   RETURN appl_err("SubInvoice not found: " + STRING(piSubInvoiceNum)).

IF piLanguage = 0 OR piLanguage = ? THEN piLanguage = 1.

top_array = add_array(response_toplevel_id, "").

FOR EACH InvRow OF SubInvoice NO-LOCK:

   invrow_struct = add_struct(top_array, "").

   add_int(invrow_struct, "inv_row", InvRow.invrow).
   add_string(invrow_struct, "billcode", InvRow.billcode).
   add_double(invrow_struct, "vat_per", InvRow.VATPerc).

   IF InvRow.CreditInvNum > 0 THEN DO:
      add_boolean(invrow_struct, "credited", true).

      /* Return Credited Amount */
      FOR FIRST bInvoice WHERE
                bInvoice.Brand = "1" AND
                bInvoice.InvNum = InvRow.CreditInvNum NO-LOCK,
          FIRST bSubInvoice OF bInvoice WHERE
                bSubInvoice.SubInvNum = InvRow.SubInvNum NO-LOCK,
          EACH  bInvRow WHERE
                bInvRow.Invnum = bInvoice.InvNum NO-LOCK:
          IF bInvRow.BillCode = InvRow.BillCode THEN DO:
             add_double(invrow_struct, "amount", bInvRow.amt).
             LEAVE.
          END. /* IF bInvRow.BillCode = InvRow.BillCode THEN DO: */
      END. /* FOR FIRST bInvoice WHERE */
   END. /* IF InvRow.CreditInvNum > 0 THEN DO: */
   ELSE DO:
      add_boolean(invrow_struct, "credited", false).
      add_double(invrow_struct, "amount", InvRow.amt).
   END. /* ELSE DO: */

   lcBillCodeName = fTranslationName("1", 1, InvRow.billcode,
                                     piLanguage, TODAY).
   IF lcBillCodeName = ? THEN lcBillCodeName = "".
   add_string(invrow_struct, "billcode_name", lcBillCodeName).

END. /* FOR EACH InvRow OF SubInvoice NO-LOCK: */

