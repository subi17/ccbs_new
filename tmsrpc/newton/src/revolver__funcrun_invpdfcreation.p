/* ----------------------------------------------------------------------
  Module .......: rpcmethods/revolver__funcrun_invpdfcreation.p
  Task .........: Receive output message for auto pdf creation from funcrun process
  Application ..: TMS
  Author .......: Subhash Sanjeevi
  Created ......: 11.10.15
  Version ......: Yoigo
---------------------------------------------------------------------- */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.CUICommon:gcBrand = "1".

{Syst/tmsconst.i}
{Func/email.i}
{Func/cparam2.i}

DEFINE VARIABLE pcTenant      AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUsername    AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAddrConfDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcToday       AS CHARACTER NO-UNDO.

DEFINE STREAM strout.

IF validate_request(param_toplevel_id, "string,string,string") EQ ? THEN RETURN.

ASSIGN pcTenant      = get_string(param_toplevel_id, "0")
       pcUserName    = get_string(param_toplevel_id, "1")
       pcMessage     = get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").
IF TRIM(pcMessage) EQ "" THEN RETURN appl_err("Message is empty").

{newton/src/settenant.i pcTenant}

ASSIGN 
       lcToday       = STRING(YEAR(TODAY),"9999") +
                       STRING(MONTH(TODAY),"99")  +
                       STRING(DAY(TODAY),"99")
       lcAddrConfDir = fCParamC("RepConfDir")
       lcLogFile     = fCParamC("FuncRunLogDir")
       lcAddrConfDir = lcAddrConfDir + "funcrunpdfcreation.email"
       lcLogFile     = lcLogFile + "funcrun_pdf_" + lcToday + STRING(TIME) + ".log".

katun = "VISTA_" + pcUserName.

OUTPUT STREAM strout TO VALUE (lcLogFile).

PUT STREAM strout UNFORMATTED 
   pcMessage SKIP.

OUTPUT STREAM strout CLOSE.

/* Mail recipients */
   GetRecipients(lcAddrConfDir).

/* Send via mail */
   SendMail(lcLogFile,"").

add_boolean(response_toplevel_id,?,TRUE).
