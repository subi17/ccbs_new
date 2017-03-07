/**
 * Fetch Invoice PDF
 *
 * @input   transaction_id;string;mandatory;transaction id
            msisdn;string;mandatory;subscription msisdn number
            inv_ext_id;string;mandatory;External Invoice id
 * @output   struct;mandatory;response struct
 * @response transaction_id;string;transaction id
             pdf_link;string;PDF link
 * @exceptions 1;Subscription not found
               2;Invoice not found
               3;Application Id does not match
               4;MSISDN does not belong to this invoice
               5;Invalid invoice PDF link
 */


{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
{Func/fexternalapi.i}
{Func/cparam2.i}

/* Input parameters */
DEF VAR pcTransId       AS CHAR NO-UNDO.
DEF VAR pcCLI           AS CHAR NO-UNDO.
DEF VAR pcExtInvId      AS CHAR NO-UNDO.

/* Output parameters */
DEF VAR top_struct      AS CHAR NO-UNDO.
DEF VAR llInvoiceExits  AS LOG  NO-UNDO.
DEF VAR lcResult        AS CHAR NO-UNDO.
DEF VAR lcCommand       AS CHAR NO-UNDO.
DEF VAR lcMiYoigoURL    AS CHAR NO-UNDO.
DEF VAR lcApplicationId AS CHAR NO-UNDO. 

DEF STREAM sInput.

IF validate_request(param_toplevel_id, "string,string,string") EQ ? THEN RETURN.
ASSIGN pcTransId  = get_string(param_toplevel_id,"0")
       pcCLI      = get_string(param_toplevel_id,"1")
       pcExtInvId = get_string(param_toplevel_id,"2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(gbAuthLog.UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").
       
katun = lcApplicationId + "_" + gbAuthLog.EndUserId.

lcMiYoigoURL = fCParam("URL","MiYoigoURL").
IF lcMiYoigoURL = "" OR lcMiYoigoURL = ? THEN
   RETURN appl_err("Missing system configuration").

FIND FIRST MobSub WHERE
           MobSub.CLI = pcCLI NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").

FIND FIRST Invoice WHERE
           Invoice.Brand    = gcBrand AND
           Invoice.ExtInvID = pcExtInvId NO-LOCK NO-ERROR.
IF NOT AVAIL Invoice THEN
   RETURN appl_err("Invoice not found").

FOR EACH SubInvoice OF Invoice NO-LOCK:
   IF SubInvoice.CLI <> MobSub.CLI THEN NEXT.

   llInvoiceExits = TRUE.
   LEAVE.
END. /* FOR EACH  Invoice WHERE */

IF NOT llInvoiceExits THEN RETURN appl_err("MSISDN does not belong to this invoice").

lcCommand = "wget --no-check-certificate -t 1 -T 5 -q -O - " +
            "--header='Host: miyoigo-b.yoigo.com' " +
            "'" + lcMiYoigoUrl + "/api/fetch_pdf?invnum=" + STRING(Invoice.InvNum) + "'".
INPUT STREAM sInput THROUGH VALUE(lcCommand).

REPEAT:
   IMPORT STREAM sInput UNFORMATTED lcResult.
   LEAVE.
END.

IF NOT lcResult BEGINS "http" THEN
   RETURN appl_err("Invalid invoice PDF link").

top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_string(top_struct, "pdf_link", lcResult).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
