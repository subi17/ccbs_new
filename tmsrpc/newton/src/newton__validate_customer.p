/**
 * Validate a Customer.
 *
 * @input   person_id;string;mandatory;
            id_type;string;mandatory;
            msisdn;string;mandatory;
 *          
 * @output  invalid;boolean false ; if invalid customer information
            name;string; customer full name 
 *
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcTenant   AS CHAR NO-UNDO.
DEF VAR pcPersonId AS CHAR NO-UNDO.
DEF VAR pcIdType   AS CHAR NO-UNDO.
DEF VAR pcMSISDN   AS CHAR NO-UNDO.
/* Local variable */
DEF VAR lcBrand   AS CHAR NO-UNDO INIT "1".
DEF VAR lcName AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string,string") EQ ? THEN RETURN.
pcTenant   = get_string(param_toplevel_id, "0").
pcPersonId = get_string(param_toplevel_id, "1").
pcIdType   = get_string(param_toplevel_id, "2").
pcMSISDN   = get_string(param_toplevel_id, "3").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FIND FIRST Customer
WHERE Customer.Brand           EQ lcBrand
  AND Customer.CustIdType      EQ pcIdType
  AND Customer.OrgId           EQ pcPersonId 
  AND Customer.Roles           NE "inactive"
  NO-LOCK NO-ERROR.

IF NOT AVAIL Customer THEN DO:
    add_boolean(response_toplevel_id, "",FALSE).
    RETURN.
END.

FIND FIRST MobSub NO-LOCK WHERE 
           MobSub.Brand = lcBrand AND
           MobSub.AgrCust = Customer.CustNum AND
           MobSub.CLI = pcMSISDN NO-ERROR.

IF NOT AVAIL MobSub THEN DO:
    add_boolean(response_toplevel_id, "",FALSE).
    RETURN.
END.

{Syst/commpaa.i}
Syst.Var:gcBrand = lcBrand.
lcName =  Func.Common:mDispCustName(BUFFER Customer).

add_string(response_toplevel_id,"",lcName).

FINALLY:
   END.
