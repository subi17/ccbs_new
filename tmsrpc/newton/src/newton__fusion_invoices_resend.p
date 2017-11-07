/**
 * Resend Fusion invoice summary email
 *
 * @input brand;string;mandatory;tenant
          fusion_invnum;int;mandatory;fusion invoice id
          username;string;mandatory;Newton username
 * @output boolean;mandatory;True
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:katun = "NewtonRPC".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/fmakemsreq.i}

DEF VAR pcStruct       AS CHAR NO-UNDO. 
DEF VAR piFusionInvnum AS INT  NO-UNDO. 
DEF VAR pcUserName     AS CHAR NO-UNDO. 
DEF VAR liRequest      AS INT  NO-UNDO. 
DEF VAR lcResult       AS CHAR NO-UNDO. 
DEF VAR lcEmail        AS CHAR NO-UNDO. 
DEF VAR pcTenant       AS CHAR NO-UNDO.
   
IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_struct(pcStruct,"fusion_invnum!,username!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   piFusionInvNum = get_int(pcStruct, "fusion_invnum")
   pcUsername = get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcUsername EQ "" THEN RETURN appl_err("empty username").

{newton/src/settenant.i pcTenant}

Syst.Var:katun = "VISTA_" + pcUsername.

FIND FusionInvoice NO-LOCK WHERE
     FusionInvoice.FuInvNum = piFusionInvNum NO-ERROR.

IF NOT AVAIL FusionInvoice THEN
   RETURN appl_err("Fusion invoice not found").

lcEmail = FusionInvoice.Email.

IF FusionInvoice.Custnum > 0 THEN DO:

   FIND FIRST Customer NO-LOCK WHERE
              Customer.Custnum = FusionInvoice.Custnum NO-ERROR.

   IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found").

   IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.Brand = Syst.Var:gcBrand AND
                     MsRequest.ReqType = {&REQTYPE_EMAIL_SENDING} AND
                     MsRequest.Custnum = FusionInvoice.Custnum AND
                     MsRequest.ReqCParam1 = "FusionEmail" AND
       LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) THEN
      RETURN appl_err("Pending email request").

   IF Customer.Email > "" THEN lcEmail = Customer.Email.
END.

IF lcEmail EQ "" OR lcEmail EQ ? THEN
   RETURN appl_err("Email not defined").

liRequest = fEmailSendingRequest(INPUT Func.Common:mMakeTS(),
                                 INPUT Syst.Var:katun,
                                 INPUT FusionInvoice.Custnum,
                                 INPUT "", /* msisdn */
                                 INPUT lcEmail,
                                 INPUT "FusionEmail",
                                 INPUT "", /* tokens */
                                 INPUT FusionInvoice.FuInvNum,
                                 INPUT {&REQUEST_SOURCE_EXTERNAL_API},
                                 OUTPUT lcResult).
IF liRequest = 0 THEN RETURN appl_err("Email request creation failed").

add_boolean(response_toplevel_id, "", True).

FINALLY:
   END.
