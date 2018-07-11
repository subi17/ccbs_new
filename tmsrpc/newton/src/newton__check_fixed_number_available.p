/**
 * Check if customer exists and return possible customer data.
 *
 * @input   brand;string;mandatory;brand to search for customer
            fixed_number;string;mandatory;
 *
 * @output  false;boolean;if fixednumber is already assigned

*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}

DEF VAR pcTenant      AS CHAR NO-UNDO. 
DEF VAR pcFixedNumber AS CHAR NO-UNDO.
DEF VAR llgAvail      AS LOG  NO-UNDO. 

IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.

ASSIGN pcTenant      = get_string(param_toplevel_id, "0")
       pcFixedNumber = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcFixedNumber) EQ "" THEN 
   RETURN appl_err("Fixed Number value is empty").

{newton/src/settenant.i pcTenant}

IF NOT CAN-FIND(FIRST MobSub WHERE
                      MobSub.Brand       EQ Syst.Var:gcBrand AND
                      MobSub.FixedNumber EQ pcFixedNumber)   THEN DO:

   IF CAN-FIND(FIRST OrderFusion NO-LOCK WHERE 
                     OrderFusion.FixedNumber EQ pcFixedNumber) THEN DO:

      FOR EACH OrderFusion NO-LOCK WHERE
               OrderFusion.FixedNumber EQ pcFixedNumber,
          FIRST Order NO-LOCK WHERE
                Order.Brand   EQ Syst.Var:gcBrand    AND
                Order.OrderId EQ OrderFusion.OrderId AND
         LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) EQ 0:
      
         llgAvail = FALSE. 
         LEAVE.

      END.

   END.
   ELSE llgAvail = TRUE.

END.
ELSE llgAvail = FALSE.

add_boolean(response_toplevel_id, "", llgAvail).

FINALLY:
END.
