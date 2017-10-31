/**
 * Get mnp operators.
 *
 * @input brand;string;mandatory
 * @output array;array of mnp operator
 * @mnpoperator brand;string;brand name
      name;string;operator name
      code;string;operator code (RRC)
      icc_prefix;string;icc prefix
      active;boolean;active in order channels
      group;string;operator brand
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".

DEF VAR pcTenant    AS CHARACTER NO-UNDO.
DEF VAR resp_array  AS CHARACTER NO-UNDO.
DEF VAR resp_struct AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "string") = ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

resp_array = add_array(response_toplevel_id, "").

FOR EACH MNPOperator WHERE
         MNPOperator.Brand EQ gcBrand AND 
         MNPOperator.Active NE ? NO-LOCK:
   resp_struct = add_struct(resp_array, "").
   add_string(resp_struct, "brand", fConvertTenantToBrand(pcTenant)).
   add_string(resp_struct, "name", MNPOperator.OperName).
   add_string(resp_struct, "code", MNPOperator.OperCode).
   add_string(resp_struct, "icc_prefix", MNPOperator.ICCPrefix).
   add_boolean(resp_struct, "active", MNPOperator.Active).
   add_string(resp_struct, "group", MNPOperator.OperBrand).
END.

FINALLY:      
   END.
