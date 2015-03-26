
/**
 * This is the test of corresponding dextra__update_order_status
 * XML RPC method.
 *
 * The fixture usage of this test:
 *
 * - fixtures fetched: none
 * - The RPC method is assumed to fetch following fixtures: none. 
 *
 */

{test_xmlrpc_includes.i}
{unit/checkutils.i}

gcFixtures = "order,OrderDelivery".

DEF VAR gcArray               AS CHARACTER NO-UNDO. 
DEF VAR gcParamStruct         AS CHARACTER NO-UNDO. 
DEF VAR liOrderId             AS INTEGER   NO-UNDO. 
DEF VAR liLoStatusId          AS INTEGER   NO-UNDO. 
DEF VAR lcLoStatusDescription AS CHARACTER NO-UNDO. 
DEF VAR liLoId                AS INTEGER   NO-UNDO. 
DEF VAR lcLoDescription       AS CHARACTER NO-UNDO. 
DEF VAR liCourierId           AS INTEGER   NO-UNDO. 
DEF VAR lcCourierDescription  AS CHARACTER NO-UNDO. 
DEF VAR lcCourierShippingId   AS CHARACTER NO-UNDO. 
DEF VAR liCourierIncidenceId  AS INTEGER NO-UNDO. 
DEF VAR lcCourierIncidenceDesc AS CHARACTER NO-UNDO. 
DEF VAR liCourierMeasureId    AS INTEGER NO-UNDO. 
DEF VAR lcCourierMeasureDesc  AS CHARACTER NO-UNDO. 



FUNCTION fdextra__notify_order_status RETURNS LOGICAL:
    run_rpc_method("dextra.notify_order_information").
    assert_success().
/*    gcArray = get_Array("", "0").
    assert_response(gcArray, "int").  */
    RETURN TRUE.
END.



FUNCTION fAddMaxParams RETURN LOGICAL:
   DEFINE VARIABLE lcIMEIArray AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcIMEIStruct AS CHARACTER NO-UNDO. 

   gcParamStruct = add_struct("", "").
   add_string(   gcParamStruct, "Order_Id"                     , 
      STRING(liOrderId)).
   add_datetime( gcParamStruct, "timestamp"                    , 
      DATETIME(TODAY, 0)).
   add_int(      gcParamStruct, "LO_Status_ID"                 , 
      liLoStatusId).
   add_string(   gcParamStruct, "LO_Status_Description"        , 
      lcLoStatusDescription).
   add_int(      gcParamStruct, "LO_Id"                        , 
      liLoId).
   add_string(   gcParamStruct, "LO_Description"               , 
      lcLoDescription).
   add_int(      gcParamStruct, "Courier_ID"                   , 
      liCourierId).
   add_string(   gcParamStruct, "Courier_Description"          , 
      lcCourierDescription).
   add_string(   gcParamStruct, "Courier_Shipping_ID"          , 
      lcCourierShippingId).
   add_int(      gcParamStruct, "Courier_Incidence_ID"         , 
      liCourierIncidenceId).
   add_string(   gcParamStruct, "Courier_Incidence_Description", 
      lcCourierIncidenceDesc).
   add_int(      gcParamStruct, "Courier_Measure_ID"           , 
      liCourierMeasureId).
   add_string(   gcParamStruct, "Courier_Measure_Description"  , 
      lcCourierMeasureDesc).
   
   RETURN TRUE.
END.


FUNCTION fAddMinParams RETURN LOGICAL (INPUT piLackingParam AS INTEGER):
   gcParamStruct = add_struct("", "").
   IF piLackingParam NE 1 THEN
      add_string(gcParamStruct, "Order_Id", STRING(liOrderId)).
   IF piLackingParam NE 2 THEN
      add_datetime(gcParamStruct, "timestamp", DATETIME(TODAY,0)).
   IF piLackingParam NE 3 THEN
      add_int(gcParamStruct, "LO_Id", liLoId).
   IF piLackingParam NE 4 THEN
      add_int(gcParamStruct, "LO_Status_ID", liLoStatusId).
   IF piLackingParam NE 5 THEN
      add_int(gcParamStruct, "Courier_ID", liCourierId).
   IF piLackingParam NE 6 THEN
      add_int(gcParamStruct, "Courier_Measure_Id", liCourierMeasureId).
   IF piLackingParam NE 7 THEN
      add_int(gcParamStruct, "Courier_Incidence_Id", liCourierIncidenceId).
END.



FUNCTION fSetParamVars RETURN LOGICAL (INPUT plMinParams AS LOGICAL):
   ASSIGN
      liLoStatusId          = 132718
      lcLoStatusDescription = "StatusDesc"
      liLoId                = 2423444 
      lcLoDescription       = "loDisc"
      liCourierId           = 123123
      lcCourierDescription  = "CourierDesc"
      lcCourierShippingId   = "CourS353244"
      liCourierIncidenceId   = 4237444 
      lcCourierIncidenceDesc = "IncidenceDesc"
      liCourierMeasureId     = 347777
      lcCourierMeasureDesc   = "MeasureDesc".

   IF plMinParams THEN
      ASSIGN lcLoDescription      = ""
             lcCourierShippingId  = ""
             lcCourierDescription = ""
             lcCourierMeasureDesc = ""
             lcCourierIncidenceDesc = "".

   RETURN TRUE.
END.

FUNCTION fFindRecords RETURN LOGICAL:
   FIND FIRST OrderDelivery WHERE 
      OrderDelivery.Brand = "1" AND 
      OrderDelivery.OrderId = liOrderId  AND
      OrderDelivery.LOTimeStamp = DATETIME(TODAY, 0) NO-LOCK NO-ERROR.
   IF NOT AVAIL OrderDelivery THEN
      assert(FALSE, "OrderDelivery with OrderId " + STRING(liOrderId) 
                    + " and timestamp " + STRING(DATETIME(TODAY, 0)) 
                    + " cannot be found.").
   RETURN TRUE.
END.


FUNCTION fCheckRecordValues RETURN LOGICAL:
   /* OrderDelivery */
   checkChar("OrderDelivery.Brand"          , OrderDelivery.Brand, "1").
   checkInt( "OrderDelivery.CourierId"      , OrderDelivery.CourierId     , 
             liCourierId).
   checkChar("OrderDelivery.CourierShippingId", 
             OrderDelivery.CourierShippingId, lcCourierShippingId).
   checkInt( "OrderDelivery.LOId"           , OrderDelivery.LoId          , 
             liLoId). 
   checkInt( "OrderDelivery.LoStatusId"     , OrderDelivery.LoStatusId    , 
             liLoStatusId).
   checkInt( "OrderDelivery.IncidentInfoId" , OrderDelivery.IncidentInfoId,
             liCourierIncidenceId).
   checkInt( "OrderDelivery.MeasureInfoId"  , OrderDelivery.MeasuresInfoId ,
             liCourierMeasureId).

   RETURN TRUE.
END.



PROCEDURE test_max_params:
   liOrderId = 14.
   fSetParamVars(FALSE).
   fAddMaxParams().

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 0, "There was an unexpected error returned " + STRING(iRetVal)). 

   fFindRecords().
   fCheckRecordValues().
END.



PROCEDURE test_min_params:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(0).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 0, "There was an unexpected error returned " + STRING(iRetVal)). 

   fFindRecords().
   fCheckRecordValues().
END.


PROCEDURE test_lacking_orderid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(1).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 1, "The orderid lacking was not recognized"). 
END.






PROCEDURE test_lacking_timestamp:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(2).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 2, "The timestamp lacking was not recognized"). 
END.



PROCEDURE test_lacking_loid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(3).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 5, "The loid lacking was not regocnized"). 
END.


PROCEDURE test_lacking_lostatusid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(4).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 3, "The loid lacking was not regocnized"). 
END.



PROCEDURE test_lacking_courierid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(5).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 7, "The courierid lacking was not regocnized"). 
END.



PROCEDURE test_lacking_courier_measure_id:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(6).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 12, "The courier measureid lacking was not regocnized"). 
END.



PROCEDURE test_lacking_courier_incidence_id:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(7).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 10, "The courier incidenceid lacking was not regocnized"). 
END.



PROCEDURE test_unexisting_orderid:
   liOrderId = 9999999.
   fSetParamVars(TRUE).
   fAddMinParams(0).

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 20, "Not existing order failed"). 
END.


PROCEDURE test_illegal_orderid:
   gcParamStruct = add_struct("", "").
   add_string(   gcParamStruct, "Order_Id" , "ASAHHA").

   fdextra__notify_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 1, "Illegal orderid not recoginized"). 
END.


