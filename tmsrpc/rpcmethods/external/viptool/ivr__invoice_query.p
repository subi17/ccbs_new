/**
 * Information of the latest invoice(s)
 *
 * @input       string;mandatory;invoice number
                integer;mandatory;type (1 = last invoice, 3 = latest invoices)
 * @output      array;array of invoice_struct
 * @invoice_struct invoice_number;string;
                invoice_date;date;
                invoice_due_date;date;
                invoice_amount;int;invoice amount in cents
 * @Exceptions  1;Subscription not found
                2;Incorrect type value
                3;Invoice includes multiple subscriptions
 */
{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

/* Input parameters */
DEF VAR pcMSISDN AS CHAR NO-UNDO.
DEF VAR piMaxCount AS INT NO-UNDO. 

{commpaa.i}
ASSIGN
   katun = "IVR_" + gbAuthLog.EndUserId.
   gcBrand = "1".

{tmsconst.i}

DEF VAR lcRespArray AS CHAR NO-UNDO. 
DEF VAR lcRespStruct AS CHAR NO-UNDO. 
DEF VAR i AS INT NO-UNDO. 
DEF VAR liSubInvoices AS INT NO-UNDO. 
DEF VAR llFound AS LOG NO-UNDO. 

IF validate_request(param_toplevel_id, "string,int") EQ ? THEN RETURN.
pcMSISDN = get_string(param_toplevel_id,"0").
piMaxCount = get_int(param_toplevel_id,"1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST mobsub NO-LOCK WHERE 
           mobsub.brand = gcBrand AND
           mobsub.cli = pcMSISDN NO-ERROR.
IF NOT AVAILABLE mobsub THEN RETURN appl_err("Subscription not found").

IF piMaxCount NE 1 AND piMaxCount NE 3 THEN
   RETURN appl_err("Incorrect type value").

lcRespArray = add_array(response_toplevel_id,"").

INVOICE_LOOP:
FOR EACH Invoice WHERE 
         Invoice.Brand = gcBrand AND
         Invoice.Custnum = MobSub.Custnum AND
         Invoice.InvType = 1 NO-LOCK USE-INDEX Custnum:

   ASSIGN
      liSubInvoices = 0
      llFound = FALSE.

   FOR EACH SubInvoice WHERE
            SubInvoice.Invnum = Invoice.Invnum NO-LOCK:
      liSubInvoices = liSubInvoices + 1.
      IF liSubInvoices > 1 THEN
         RETURN appl_err("Invoice includes multiple subscriptions").
      IF SubInvoice.MsSeq EQ MobSub.MsSeq AND
         SubInvoice.Custnum EQ Mobsub.Custnum THEN llFound = TRUE.
   END.
   IF NOT llFound THEN NEXT.

   lcRespStruct = add_struct(lcRespArray,"").
   
   add_string(lcRespStruct,"invoice_number", Invoice.ExtInvId).
   add_datetime(lcRespStruct,"invoice_date", Invoice.InvDate).
   add_datetime(lcRespStruct,"invoice_due_date", Invoice.DueDate).
   add_int(lcRespStruct,"invoice_amount",INT(Invoice.InvAmt * 100)).
   
   i = i + 1.
   IF i >= piMaxCount THEN LEAVE INVOICE_LOOP.

END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
