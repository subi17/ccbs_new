/**
 * Get mnp operators.
 *
 * @input empty;
 * @output array;array of mnp operator
 * @mnpoperator name;string;operator name
      code;string;operator code (RRC)
      icc_prefix;string;icc prefix
      active;boolean;active in order channels
      brand;string;operator brand
*/

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "Newton".
gcBrand = "1".

DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAR resp_struct AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "") = ? THEN RETURN.
IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

FOR EACH MNPOperator WHERE
         MNPOperator.Brand EQ gcBrand AND 
         MNPOperator.Active NE ? NO-LOCK:
   resp_struct = add_struct(resp_array, "").
   add_string(resp_struct, "name", MNPOperator.OperName).
   add_string(resp_struct, "code", MNPOperator.OperCode).
   add_string(resp_struct, "icc_prefix", MNPOperator.ICCPrefix).
   add_boolean(resp_struct, "active", MNPOperator.Active).
   add_string(resp_struct, "brand", MNPOperator.OperBrand).
END.

FINALLY:      
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
