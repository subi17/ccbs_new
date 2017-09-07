/**
 * Get invoice delivery type
 *
 * @input      string;mandatory;MSISDN
 * @output     integer;mandatory;Delivery Type
 * @Examples   Delivery Type;Delivery Type Name
               1;Paper Invoice
               2;Email Invoice
               4;SMS Invoice
               10;No Delivery
               11;Email-Wating for activation
 * @Exceptions 1;Subscription not found
               2;Customer not found
               3;Unknown invoice delivery type
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO. 

/* Input parameters */
DEF VAR pcMSISDN        AS CHAR NO-UNDO.
DEF VAR liDeliveryType  AS INT  NO-UNDO. 

{Syst/commpaa.i}
ASSIGN
   katun = "IVR_" + ghAuthLog::EndUserId. 
   gcBrand = "1".
{Syst/tmsconst.i}

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcMSISDN = get_string(param_toplevel_id,"0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i NO Ordercanal MobSub CLI pcMSISDN}

FIND FIRST Customer WHERE 
           Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found").

CASE Customer.DelType:
   WHEN {&INV_DEL_TYPE_PAPER}         THEN liDeliveryType = 1.
   WHEN {&INV_DEL_TYPE_EMAIL}         THEN liDeliveryType = 2.
   WHEN {&INV_DEL_TYPE_SMS}           THEN liDeliveryType = 4.
   WHEN {&INV_DEL_TYPE_NO_DELIVERY}   THEN liDeliveryType = 10.
   WHEN {&INV_DEL_TYPE_EMAIL_PENDING} THEN liDeliveryType = 11.
   OTHERWISE RETURN appl_err("Unknown invoice delivery status").
END.

add_int(response_toplevel_id,"",liDeliveryType).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
