/**
 * Get Order Logistic history.
 *
 * @input  orderid;int;mandatory;id of order
 * @output orderdelivery;struct;a single order logistic history data
 * @orderdelivery timestamp;datetime;timestamp of logistic event
                  courier_id;int;
                  lo_id;int;
                  lo_status_id;int;
                  courier_shipping_id;string;
                  incident_info_id;int;
                  measures_info_id;int;
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.CUICommon:gcBrand = "1".
{Syst/tmsconst.i}

DEFINE VARIABLE piOrderId              AS INTEGER   NO-UNDO. 
DEFINE VARIABLE top_array            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE od_struct            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError               AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piOrderId = get_pos_int(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

/* validation ends */
top_array = add_array(response_toplevel_id, "").

FOR EACH OrderDelivery WHERE
   OrderDelivery.Brand = "1"         AND 
   OrderDelivery.OrderId = piOrderId NO-LOCK:
   
   IF STRING(OrderDelivery.LOStatusID) BEGINS {&LO_STATUS_ROUTER_PREFIX} OR 
      STRING(OrderDelivery.LOStatusID) BEGINS {&LO_STATUS_TV_STB_PREFIX} THEN
      NEXT.
   od_struct = add_struct(top_array, "").

   add_datetime(od_struct, "timestamp", OrderDelivery.LOTimeStamp ). 
   add_int(od_struct, "courier_id" , OrderDelivery.CourierId ). 
   add_int(od_struct, "lo_id", OrderDelivery.LOId ). 
   add_int(od_struct, "lo_status_id", OrderDelivery.LOStatusId ). 
   add_string(od_struct,"courier_shipping_id",OrderDelivery.CourierShippingId). 
   add_int(od_struct, "incident_info_id", OrderDelivery.IncidentInfoId ). 
   add_int(od_struct, "measures_info_id", OrderDelivery.MeasuresInfoId ). 

END.

FINALLY:
   END.

