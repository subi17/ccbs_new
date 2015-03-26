/**
 * Get subscription billing permission 
 *
 * @input       id;int;mandatry;id of subscription
 * @output      status;int;billing permission status
 * @Exceptions  1;Subscription not found
 */
{xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST mobsub NO-LOCK WHERE 
           mobsub.msseq = piMsSeq NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err("Subscription not found").

{commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{tmsconst.i}
{flimitreq.i}

IF gbAuthLog.UserName = "viptool" THEN DO:
{vip_check.i}
END.

fGetLimit(  MobSub.InvCust, 
            MobSub.MsSeq,
            {&LIMIT_TYPE_BILLPERM},
            0, /* Limit ID */
            0, /* TMRuleSeq */
            TODAY).
IF NOT AVAIL Limit THEN add_int(response_toplevel_id, "", 0).
ELSE add_int(response_toplevel_id,"", INT(Limit.LimitAmt)).
