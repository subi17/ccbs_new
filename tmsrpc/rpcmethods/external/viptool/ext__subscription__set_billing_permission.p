/**
 * Set subscription billing permission 
 *
 * @input       id;int;mandatory;id of subscription
                status;int;mandatry;billing permission status 
 * @output      response;boolean;True
 * @Exceptions  1;Subscription not found
                2;Unknown status code
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR piBillingPermission AS INT NO-UNDO.
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.

IF validate_request(param_toplevel_id, "int,int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
piBillingPermission = get_int(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND mobsub NO-LOCK WHERE 
     mobsub.msseq = piMsSeq NO-ERROR.
IF NOT AVAILABLE mobsub THEN
    RETURN appl_err("Subscription not found").

FIND TMSCodes WHERE
     TMSCodes.TableName = "Limit" AND
     TMSCodes.FieldName = "Billing Permission" AND
     TMSCodes.CodeValue = STRING(piBillingPermission) NO-LOCK NO-ERROR.
IF NOT AVAIL TMSCodes THEN
    RETURN appl_err(SUBST("Unknown status code: &1", piBillingPermission)).

{Syst/commpaa.i}
katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId.
gcBrand = "1".
{Syst/tmsconst.i}
{Func/flimitreq.i}

IF ghAuthLog::UserName = "viptool" THEN DO:
{vip_check.i}
END.

FIND FIRST Limit WHERE
   Limit.MsSeq = MobSub.MsSeq AND
   Limit.LimitType = {&LIMIT_TYPE_BILLPERM} AND
   Limit.TMRuleSeq = 0 AND
   Limit.Todate >= TODAY AND
   Limit.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
   
IF piBillingPermission = 0 THEN DO:
   IF AVAIL Limit THEN 
   fSetLimit (
      ROWID(Limit),
      Limit.LimitAmt,
      FALSE, /* default value */ 
      Limit.FromDate,
      TODAY - 1).
END.
ELSE
fCreateLimitHistory(
   MobSub.InvCust,
   MobSub.MsSeq,
   {&LIMIT_TYPE_BILLPERM},
   piBillingPermission,
   0, /* limit id */
   0, /* tmruleseq */
   FALSE, /* default value */
   TODAY,
   12/31/2049).

add_boolean(response_toplevel_id,"", TRUE).

IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
