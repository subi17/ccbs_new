/**
 * Get Customer Satisfaction Value History  
 *
 * @input int;mandatory;customer id

 * @output history;array of satisfaction values for that customer;

 * @history value;string;mandatory;satisfaction value
            updated_at;datetime;mandatory; update datetime 
            additional_info;string;optional; additional info if exist 
 */

{xmlrpc/xmlrpc_access.i}
DEF VAR gcBrand   AS CHAR NO-UNDO INIT "1".
{tmsconst.i}
{fpindicator.i}

/* Input parameters */
DEF VAR piCustNum AS INTEGER NO-UNDO. 

/* local variables */
DEF VAR lcResultArray AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcResultArray = add_array(response_toplevel_id, "").

/* check Customer */
FIND Customer WHERE 
     Customer.Brand = gcBrand AND
     Customer.CustNum = piCustNum  NO-LOCK NO-ERROR. 

IF NOT AVAIL Customer THEN
RETURN appl_err("Customer not found ").

fGetPIndicatorHistory("Customer",
                      STRING(piCustNum),
                      {&P_INDICATOR_TYPE_SATISFACTION_VALUE},
                      lcResultArray) .

