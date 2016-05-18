/**
 * Get Fusion invoice
 *
 * @input int;mandatory;subscription id
 * @output array_of_struct;mandatory;Fusion invoice links from past 6 months (in descending order)
 * @struct invoice_date;date;mandatory;invoice date
           fusion_invnum;int;mandatory;fusion invoice id
           invoice_link;string;mandatory;link to fusion invoice (newton.fusion_invoices_get)
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{Syst/tmsconst.i}
{Inv/fusioninvoice.i}

DEF VAR lcHash AS CHAR NO-UNDO. 
DEF VAR piMsSeq AS INT NO-UNDO. 

DEF VAR lcResultArray AS CHAR NO-UNDO. 
DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR ldaMonthFrom AS DATE NO-UNDO. 
DEF VAR ldaCurrentMonthFrom AS DATE NO-UNDO. 
DEF VAR liMonths AS INT NO-UNDO. 
DEF VAR lcLink AS CHAR NO-UNDO. 
   
IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Mobsub NO-LOCK WHERE
           Mobsub.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAIL Mobsub THEN RETURN appl_err("Subscription not found").

FIND FIRST Customer NO-LOCK WHERE
           Customer.Custnum = Mobsub.Custnum NO-ERROR.
IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found").

ldaCurrentMonthFrom = DATE(MONTH(TODAY),1,YEAR(TODAY)).

IF CAN-FIND(FIRST FusionInvoice NO-LOCK WHERE
                  FusionInvoice.Custnum = Mobsub.Custnum AND
                  FusionInvoice.InvDate >= ldaCurrentMonthFrom) 
THEN liMonths = -5.
ELSE liMonths = -6.

ldaMonthFrom = ADD-INTERVAL(ldaCurrentMonthFrom, liMonths, "months").

lcResultArray = add_array(response_toplevel_id, "").

FOR EACH FusionInvoice NO-LOCK WHERE
         FusionInvoice.Custnum = Customer.Custnum AND
         FusionInvoice.InvDate >= ldaMonthFrom:

   lcResultStruct = add_struct(lcResultArray,"").

   lcHash = fGenerateFusionInvoiceHash(
               FusionInvoice.InvNum,
               FusionInvoice.InvoiceNum,
               FusionInvoice.CustomerID).
   lcLink = fGenerateFusionInvoiceLink(FusionInvoice.FuInvNum,lcHash).
   
   add_datetime(lcResultStruct,"invoice_date",FusionInvoice.InvDate).
   add_int(lcResultStruct,"fusion_invnum",FusionInvoice.FuInvNum).
   add_string(lcResultStruct,"invoice_link",lcLink).
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
