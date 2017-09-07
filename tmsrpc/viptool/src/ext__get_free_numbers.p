/** Get a list of 9 still free numbers.
 * Optional search: If a non-empty parameter is provided, the first matching
   numbers will be returned. No random choice here.
 * @input search_key;string;optional
 * @output numbers;array of strings
 * @Exceptions  1;Input Channel should be: VIP

 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}
{Func/multitenantfunc.i}

/* Input parameters */
DEF VAR pcSearch AS CHAR NO-UNDO.
DEF VAR pcFor AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_array AS CHAR NO-UNDO.
/* Local variables */
DEF VAR ldTS AS DECIMAL NO-UNDO.
DEF VAR lii AS INT NO-UNDO.

&GLOBAL-DEFINE count 9

top_array = validate_request(param_toplevel_id, "string,[string]").

pcFor = get_string(param_toplevel_id, "0").

IF NUM-ENTRIES(top_array) > 1 THEN
    pcSearch = get_string(param_toplevel_id, "1").
    
IF gi_xmlrpc_error NE 0 then RETURN.

IF pcFor <> {&MSISDN_STOCK_VIP} THEN
  RETURN appl_err("Input Channel should be: VIP").

IF NOT fsetEffectiveTenantForAllDB({&TENANT_YOIGO}) THEN RETURN
   int_err("Tenant change failed").

ldTS = {&nowTS}.

top_array = add_array(response_toplevel_id, "").

IF pcSearch NE "" THEN 
DO:
    pcSearch = "*" + pcSearch + "*".

    FOR EACH msisdn NO-LOCK WHERE MSISDN.Brand      EQ      "1"
                              AND msisdn.statuscode EQ      1
                              AND msisdn.LockedTo   LT      ldTS
                              AND MSISDN.ValidTo    GE      ldTS
                              AND MSISDN.POS        EQ      pcFor
                              AND msisdn.cli        MATCHES pcSearch
                              BY msisdn.cli
      lii = 1 TO {&count}:
        add_string(top_array, "", msisdn.cli).
    END.
END. 
ELSE 
DO:
    FOR EACH msisdn NO-LOCK WHERE MSISDN.Brand      EQ "1"
                              AND msisdn.statuscode EQ 1
                              AND msisdn.LockedTo   LT ldTS
                              AND MSISDN.ValidTo    GE ldTS
                              AND MSISDN.POS        EQ pcFor
                              BY msisdn.cli
      lii = 1 TO {&count}:
         add_string(top_array, "", msisdn.cli).
    END.
END.
