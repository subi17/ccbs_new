/**
 * Get IMEIs to be released.
 *
 * @input  brand;string;mandatory;Tenant
 * @output array;array of result_structs
 * @result_struct struct;;
    order_id;int;
    imei;string;
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}
DEFINE VARIABLE gcBrand  AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE pcTenant AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcArray AS CHARACTER NO-UNDO. 
lcArray = add_array(response_toplevel_id, "").

DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0"). 

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FOR EACH OrderAccessory NO-LOCK WHERE  
         OrderAccessory.Brand = gcBrand AND
         OrderAccessory.IMEIStatus = ({&IMEI_STATUS_TO_BE_RELEASED})
   i = 1 to 500:

   lcStruct = add_struct(lcArray,"").
   add_int(lcStruct, "order_id", OrderAccessory.OrderId).
   add_string(lcStruct, "imei", OrderAccessory.IMEI).
END.

