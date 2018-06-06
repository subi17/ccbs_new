/* ----------------------------------------------------------------------
  MODULE .......: newton__publish_ifs.p
  TASK .........: Creates msrequest request to publish invoices to
                  ifs
  APPLICATION ..:
  AUTHOR .......: subhash sanjeevi,Surbhi,Ramesh
  CREATED ......: 23.05.2018
  CHANGED ......:
  Version ......:
  ---------------------------------------------------------------------- */

/* Check if fixed number is already exsisiting in TMS.
 * @input string;mandatory;fixed number
 * @output success;boolean;true or false
   NOTES : If Fixed number is already assigned (means it is not free) in TMS 
           for an existing Subscription or Order then return FALSE, else return TRUE.  
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}

DEF VAR pcFixedNumber AS CHAR NO-UNDO.
DEF VAR llgAvail      AS LOG  NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcFixedNumber = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcFixedNumber) EQ "" THEN 
   RETURN appl_err("Fixed Number value is empty").

IF NOT CAN-FIND(FIRST MobSub WHERE
                      MobSub.Brand       EQ Syst.Var:gcBrand AND
                      MobSub.FixedNumber EQ pcFixedNumber)   THEN DO:

   IF CAN-FIND(FIRST OrderFusion NO-LOCK WHERE 
                     OrderFusion.FixedNumber EQ pcFixedNumber) THEN DO:

      FOR EACH OrderFusion NO-LOCK WHERE
               OrderFusion.FixedNumber EQ pcFixedNumber,
          FIRST Order NO-LOCK WHERE
                Order.Brand   EQ Syst.Var:gcBrand     AND
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
