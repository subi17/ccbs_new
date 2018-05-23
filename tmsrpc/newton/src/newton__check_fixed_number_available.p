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
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}

DEFINE BUFFER bMobSub      FOR MobSub.
DEFINE BUFFER bOrderFusion FOR OrderFusion.
DEFINE BUFFER bOrder       FOR Order.

DEF VAR pcFixedNumber AS CHAR NO-UNDO.
DEF VAR llgAvail      AS LOG  NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcFixedNumber = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcFixedNumber) EQ "" THEN 
   RETURN appl_err("Fixed Number value is empty").

IF NOT CAN-FIND(FIRST bMobSub WHERE
                      bMobSub.Brand       EQ Syst.Var:gcBrand AND
                      bMobSub.FixedNumber EQ pcFixedNumber)   THEN DO:

   IF CAN-FIND(FIRST bOrderFusion NO-LOCK WHERE 
                     bOrderFusion.FixedNumber EQ pcFixedNumber) THEN DO:

      FOR FIRST bOrderFusion NO-LOCK WHERE
                bOrderFusion.FixedNumber EQ pcFixedNumber,
          FIRST bOrder NO-LOCK WHERE
                bOrder.Brand   EQ Syst.Var:gcBrand     AND
                bOrder.OrderId EQ bOrderFusion.OrderId AND
         LOOKUP(bOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) EQ 0:
      
         llgAvail = FALSE. 

      END.

   END.
   ELSE llgAvail = TRUE.

END.
ELSE llgAvail = FALSE.

add_boolean(response_toplevel_id, "", llgAvail).

FINALLY:

END.
