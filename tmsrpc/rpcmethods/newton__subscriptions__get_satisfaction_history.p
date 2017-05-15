/**
 * Get Subscription Satisfaction Value History  
 *
 * @input int;mandatory;msseq

 * @output history;array of satisfaction values for that subscription;

 * @history value;string;mandatory;satisfaction value
            updated_at;datetime;mandatory; update datetime 
            additional_info;string;optional; additional info if exist 
 */

{xmlrpc/xmlrpc_access.i}
DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
{Syst/tmsconst.i}
{fpindicator.i}

/* Input parameters */
DEF VAR piMsSeq AS INTEGER NO-UNDO. 

/* local variables */
DEF VAR lcResultArray AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcResultArray = add_array(response_toplevel_id, "").

fGetPIndicatorHistory("MobSub",
                      STRING(piMsSeq),
                      {&P_INDICATOR_TYPE_SATISFACTION_VALUE},
                      lcResultArray) .

