/**
 * Get Customer Satisfaction Value History  
 *
 * @input int;mandatory;customer id

 * @output history;array of satisfaction values for that customer;

 * @history value;string;mandatory;satisfaction value
            updated_at;datetime;mandatory; update datetime 
            additional_info;string;optional; additional info if exist 
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEF VAR gcBrand   AS CHAR NO-UNDO INIT "1".
{Syst/tmsconst.i}
{newton/src/fpindicator.i}

/* Input parameters */
DEF VAR piCustNum AS INTEGER NO-UNDO. 

/* local variables */
DEF VAR lcResultArray AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcResultArray = add_array(response_toplevel_id, "").

{newton/src/findtenant.i YES Common Customer CustNum piCustNum}

fGetPIndicatorHistory("Customer",
                      STRING(piCustNum),
                      {&P_INDICATOR_TYPE_SATISFACTION_VALUE},
                      lcResultArray) .

