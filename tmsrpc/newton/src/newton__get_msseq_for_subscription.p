/**
 * Returns subscription ID (msseq) for a given MSISDN
 *
 * @input       brand;string;mandatory;Used brand (yoigo or masmovil)
 *              subscription-number;string;mandatory;MSISDN or fixed number in case of 2P subscription
 * @output      int;subscription ID
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcTenant AS CHAR NO-UNDO. 
DEF VAR pcMSISDN AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF validate_struct(pcStruct, "brand!,subscription-number!") EQ ? 
   THEN RETURN.
   
ASSIGN
   pcTenant = get_string(pcStruct, "brand") 
   pcMSISDN = get_string(pcStruct, "subscription-number").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.CLI = pcMSISDN NO-ERROR.
IF NOT AVAIL MobSub THEN RETURN appl_err("Subscription not found").

add_int(response_toplevel_id, "", MobSub.MsSeq).
