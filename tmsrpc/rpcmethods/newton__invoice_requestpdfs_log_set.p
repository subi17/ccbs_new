/**
 * Save log about invoice PDF requests
 *
 * @input       invoicedata;struct;contain all pdf request information
   @invoicedata username;string;mandatory;MiYoigo username
                systemid;string;from where request was made
                eventtype;string;mandatory;What kind of event
                reasoncode;int;Reason information
                cli;string;requested msisdn
                invnum;string;Invoice number
                accesstype;string;Read or Write
 * @output      success;boolean; 
*/

{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}
{Func/dpl_log.i}

DEF VAR top_array AS CHAR NO-UNDO.
DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR pcUserName AS CHAR NO-UNDO.
DEF VAR pcSystemId AS CHAR NO-UNDO. 
DEF VAR pcEventType AS CHAR NO-UNDO.
DEF VAR piReasonCode AS INT NO-UNDO.
DEF VAR pcInvNum AS CHAR NO-UNDO.
DEF VAR pcAccessType AS CHAR NO-UNDO.
DEF VAR llLogStatus AS LOG NO-UNDO.
DEF VAR pcSearchRule AS CHAR NO-UNDO.
DEF VAR pcCli AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_request(pcStruct, "username,systemid,eventtype,reasoncode,cli,invnum,accesstype").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUserName = get_string(pcStruct,"username").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcSystemId   = get_string(pcStruct, "systemid")
   pcEventType  = get_string(pcStruct, "eventtype")
   piReasonCode = get_int(pcStruct, "reasoncode")
   pcCli        = get_string(pcStruct, "cli")
   pcInvNum     = get_string(pcStruct, "invnum")
   pcAccessType = get_string(pcStruct, "accesstype").


IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP(pcEventType, "PDF_Invoice_certified,PDF_Invoice_non_certified,PDF_Call_Itemization,MiYoigo_password_change") = 0 
   THEN RETURN  appl_err("Invalid EventType").

FIND FIRST Invoice NO-LOCK WHERE
           Invoice.Brand = gcBrand AND
           Invoice.ExtInvID = pcInvNum NO-ERROR.

IF NOT AVAIL Invoice THEN RETURN appl_err("Requested Invoice not available").

IF pcEventType NE "PDF_Call_Itemization" THEN DO:

   FOR EACH SubInvoice NO-LOCK WHERE
            SubInvoice.InvNum = Invoice.InvNum:
      pcSearchRule = pcSearchRule + SubInvoice.CLI + ",".
   END.
END. /* IF pcEventType NE "PDF_Call_Itemization" THEN DO  */
ELSE pcSearchRule = pcCLI.

pcSearchRule = TRIM(pcSearchRule, ",").

llLogStatus = fCreateDPLLog(pcUserName, 
                            pcSystemId,
                            pcEventType, 
                            piReasonCode,
                            pcInvNum, 
                            Invoice.FromDate, 
                            Invoice.ToDate,
                            "r",
                            pcSearchRule).


IF llLogStatus EQ TRUE THEN add_boolean(response_toplevel_id,"",TRUE).
ELSE add_boolean(response_toplevel_id,"",FALSE).


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

