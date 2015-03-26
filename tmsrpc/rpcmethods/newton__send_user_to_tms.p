/**
 * Adds or updates salesman
 * @input   string;mandatory;saleman id (must be non-empty)
            string;mandatory;salesman name
            boolean;mandatory;active
            string;mandatory;reseller id (must be non-empty)
 *          
 * @output  result;boolean;true/false
 */
{xmlrpc/xmlrpc_access.i}

/* Dependencies ... */

/* Input parameters */
DEF VAR pcUsername   AS CHAR  NO-UNDO.
DEF VAR pcFullName   AS CHAR  NO-UNDO.
DEF VAR pcReseller   AS CHAR  NO-UNDO.
DEF VAR plActive     AS LOG   NO-UNDO.

/* Output parameters */
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,boolean,string") 
   EQ ? THEN RETURN.
   
ASSIGN
   pcUsername  = get_nonempty_string(param_toplevel_id, "0")
   pcFullname  = get_string(param_toplevel_id, "1")
   plActive    = get_bool(param_toplevel_id, "2")
   pcReseller  = get_nonempty_string(param_toplevel_id, "3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF NOT CAN-FIND(FIRST Reseller NO-LOCK WHERE
                      Reseller.Brand = "1" AND
                      Reseller.Reseller = pcReseller) THEN
   RETURN appl_err(SUBST("Reseller &1 not found",pcReseller)).

FIND FIRST salesman WHERE
           salesman.brand = "1" and
           salesman.salesman = pcUsername EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAIL salesman THEN DO:
   CREATE Salesman.
   ASSIGN
      Salesman.Brand    = "1"
      Salesman.Salesman = pcUsername
      Salesman.SmName   = pcFullName
      Salesman.Reseller = pcReseller
      Salesman.Active   = plActive.
END.
ELSE DO:
   ASSIGN
      Salesman.SmName = pcFullName
      Salesman.Reseller = pcReseller
      Salesman.Active = plActive.
END.


add_boolean(response_toplevel_id, "", TRUE).
