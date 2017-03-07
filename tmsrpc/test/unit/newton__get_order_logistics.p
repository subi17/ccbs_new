/**
 * This is the test set for the corresponding newton__get_order_logistics
 * XML RPC method.
 *
 * The fixture usage of this test set:
 *
 * - fixtures fetched: 
 *   order:
 *      OrdersForGetOrderDetails2 (OrderId:12)
 *      OrdersForGetOrderDetails3 (OrderId:13)
 *    
 *   OrderDelivery:
 *      OneOrderDeliveryFound     (OrderId:12)
 *      TwoOrdersDeliveryFound1st (OrderId:13)
 *      TwoOrdersDeliveryFound2nd (OrderId:13)
 *
 * - restrictions for building other fixtures:
 *    There must not be order with orderid 99
 */

{test_xmlrpc_includes.i}
{unit/checkutils.i}
{Func/timestamp.i}

gcFixtures = "order,OrderDelivery".

/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_get_order_logistics RETURN LOGICAL:
    run_rpc_method("newton.get_order_logistics").
    RETURN TRUE.
END FUNCTION.

DEFINE VARIABLE gcRetArray         AS CHARACTER NO-UNDO. 

DEFINE VARIABLE dtStamp            AS DATETIME  NO-UNDO.
DEFINE VARIABLE iCourierId         AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iLoId              AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iLoStatusId        AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cCourierShippingId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iIncidentInfoId    AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iMeasuresInfoId    AS INTEGER   NO-UNDO. 

FUNCTION fGetStructData RETURN LOGICAL (INPUT pcStruct AS CHARACTER):
   dtStamp            = get_datetime( pcStruct, "timestamp"           ). 
   iCourierId         = get_int(      pcStruct, "courier_id"          ).
   iLoId              = get_int(      pcStruct, "lo_id"               ).
   iLoStatusId        = get_int(      pcStruct, "lo_status_id"        ).
   cCourierShippingid = get_string(   pcStruct, "courier_shipping_id" ).
   iIncidentInfoId    = get_int(      pcStruct, "incident_info_id"    ).
   iMeasuresInfoId    = get_int(      pcStruct, "measures_info_id"    ).
   RETURN TRUE.
END.


FUNCTION fCheckOrderDelivery RETURN LOGICAL 
   (INPUT pcStruct AS CHARACTER, INPUT pcFixture AS CHARACTER):
   
   fetch_fixture(pcFixture, BUFFER OrderDelivery:HANDLE).


   fGetStructdata(pcStruct).
   checkDateTime("OrderDelivery.LOTimeStamp", OrderDelivery.LOTimeStamp,
      dtStamp). 
   checkInt(     "OrderDelivery.CourierId"  , OrderDelivery.CourierId,
      iCourierId).
   checkInt(     "OrderDelivery.LoId"       , OrderDelivery.LOId,
      iLoId).
   checkInt(     "OrderDelivery.LOStatusId" , OrderDelivery.LOStatusId,
      iLoStatusId).
   checkChar(    "OrderDelivery.CourierShippingId", 
      OrderDelivery.CourierShippingId,
      cCourierShippingId).
   checkInt(     "OrderDelivery.IncidentInfoId"   , 
      OrderDelivery.IncidentInfoId,
      iIncidentInfoId).
   checkInt(     "OrderDelivery.MeasuresInfoId"   ,
      OrderDelivery.MeasuresInfoId,
      iMeasuresInfoId).

   RETURN TRUE.
END.

PROCEDURE test_get_one_order_delivery:
   add_int("", "", 12).  
   call_get_order_logistics().
   assert_success().
   gcRetArray = get_array("", "").
   assert(get_paramcount(gcRetArray) eq 1, 
      "Amount of OrderDelivery records was not 1 as expected").
   fCheckOrderDelivery(get_struct(gcRetArray, "0"), "OneOrderDeliveryFound").
END.


PROCEDURE test_get_two_order_deliveries:
   add_int("", "", 13).  
   call_get_order_logistics().
   assert_success().
   gcRetArray = get_array("", "").
   assert(get_paramcount(gcRetArray) eq 2, 
      "Amount of OrderDelivery records was not 2 as expected").
   fCheckOrderDelivery(get_struct(gcRetArray, "0"), "TwoOrdersDeliveryFound1st").
   fCheckOrderDelivery(get_struct(gcRetArray, "1"), "TwoOrdersDeliveryFound2nd").
END.


PROCEDURE test_not_found_order:
   add_int("", "", 99).  
   call_get_order_logistics().
   assert_fault({&APPLICATION_ERROR},"Order 99 not found").
END.

