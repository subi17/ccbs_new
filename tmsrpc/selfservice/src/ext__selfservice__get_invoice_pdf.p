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


{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
ASSIGN Syst.Var:gcBrand = "1"
       Syst.Var:katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId.
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

{selfservice/src/findtenant.i NO ordercanal MobSub Cli pcCLI}

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(ghAuthLog::UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").
       
Syst.Var:katun = lcApplicationId + "_" + ghAuthLog::EndUserId.

lcMiYoigoURL = fCParam("URL","MiYoigoURL").
IF lcMiYoigoURL = "" OR lcMiYoigoURL = ? THEN
   RETURN appl_err("Missing system configuration").

FIND FIRST Invoice WHERE
           Invoice.Brand    = Syst.Var:gcBrand AND
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
   ghAuthLog::TransactionId = pcTransId.

   END.
