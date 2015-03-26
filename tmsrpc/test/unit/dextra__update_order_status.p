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

gcFixtures = "order,OrderAccessory,OrderDelivery".

DEF VAR gcParamStruct         AS CHARACTER NO-UNDO.
DEF VAR gcArray               AS CHARACTER NO-UNDO. 
DEF VAR liOrderId             AS INTEGER   NO-UNDO. 
DEF VAR liLoStatusId          AS INTEGER   NO-UNDO. 
DEF VAR lcLoStatusDescription AS CHARACTER NO-UNDO. 
DEF VAR liLoId                AS INTEGER   NO-UNDO. 
DEF VAR lcLoDescription       AS CHARACTER NO-UNDO. 
DEF VAR liCourierId           AS INTEGER   NO-UNDO. 
DEF VAR lcCourierDescription  AS CHARACTER NO-UNDO. 
DEF VAR lcCourierShippingId   AS CHARACTER NO-UNDO. 
DEF VAR lcIMEI                AS CHARACTER NO-UNDO. 


FUNCTION fdextra__update_order_status RETURNS LOGICAL:
    run_rpc_method("dextra.update_order_status").
    assert_success().
/*    gcArray = get_Array("", "0").
    assert_response(gcArray, "int").  */
    RETURN TRUE.
END.


FUNCTION fAddMaxParams RETURN LOGICAL:
   DEFINE VARIABLE lcIMEIArray AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcIMEIStruct AS CHARACTER NO-UNDO. 

   gcParamStruct = add_struct("", "").
   add_string(   gcParamStruct, "Order_Id"             , STRING(liOrderId)).
   add_datetime( gcParamStruct, "timestamp"            , DATETIME(TODAY, 0)).
   add_int(      gcParamStruct, "LO_Status_ID"         , liLoStatusId).
   add_string(   gcParamStruct, "LO_Status_Description", lcLoStatusDescription).
   add_int(      gcParamStruct, "LO_Id"                , liLoId).
   add_string(   gcParamStruct, "LO_Description"       , lcLoDescription).
   add_int(      gcParamStruct, "Courier_ID"           , liCourierId).
   add_string(   gcParamStruct, "Courier_Description"  , lcCourierDescription).
   add_string(   gcParamStruct, "Courier_Shipping_ID"  , lcCourierShippingId).

   lcIMEIArray = add_array(gcParamStruct, "IMEI_COL").
   lcIMEIStruct = add_struct(lcIMEIArray, "0").
   add_string(lcIMEIStruct, "IMEI", lcIMEI).
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
END.



FUNCTION fFindRecords RETURN LOGICAL (INPUT plOrderAccNeeded AS LOGICAL):
   IF plOrderAccNeeded THEN
   DO:
      FIND OrderAccessory WHERE OrderAccessory.Brand = "1" AND 
                                OrderAccessory.OrderId = liOrderId 
                                NO-LOCK NO-ERROR.
      IF NOT AVAIL OrderAccessory THEN
         assert(FALSE, "OrderAccessory with OrderId = " + 
                       STRING(liOrderId) + " was not available").
   END.
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


FUNCTION fSetParamVars RETURN LOGICAL (INPUT plMinParams AS LOGICAL):
   ASSIGN
      liLoStatusId          = 132718
      lcLoStatusDescription = "StatusDesc"
      liLoId                = 2423444 
      lcLoDescription       = "loDisc"
      liCourierId           = 123123
      lcCourierDescription  = "CourierDesc"
      lcCourierShippingId   = "CourS353244"
      lcIMEI                = "2542525252".

   IF plMinParams THEN
      ASSIGN lcLoDescription      = ""
             lcCourierShippingId  = ""
             lcCourierDescription = "".

   RETURN TRUE.
END.


FUNCTION fCheckRecordValues RETURN LOGICAL (INPUT plOrderAccNeeded AS LOGICAL):
   IF plOrderAccNeeded THEN
   DO:
      /* OrderAccessory */
      checkChar("OrderAccessory.IMEI"     , OrderAccessory.IMEI, lcIMEI).
   END.

   /* OrderDelivery */
   checkChar("OrderDelivery.Brand"     , OrderDelivery.Brand, "1").
   checkInt( "OrderDelivery.CourierId" , OrderDelivery.CourierId , 
             liCourierId).
   checkChar("OrderDelivery.CourierShippingId", 
             OrderDelivery.CourierShippingId, lcCourierShippingId).
   checkInt( "OrderDelivery.LOId"      , OrderDelivery.LoId      , 
             liLoId). 
   checkInt( "OrderDelivery.LoStatusId", OrderDelivery.LoStatusId, 
             liLoStatusId).

   RETURN TRUE.
END.


PROCEDURE test_max_params:
   liOrderId = 14.
   fSetParamVars(FALSE).
   fAddMaxParams().
   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 0, "There was an unexpected error returned " + STRING(iRetVal)). 

   fFindRecords(TRUE).
   fCheckRecordValues(TRUE).
END.


PROCEDURE test_min_params:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(0).

   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 0, "There was an unexpected error returned " + STRING(iRetVal)). 

   fFindRecords(FALSE).
   fCheckRecordValues(FALSE).
END.


PROCEDURE test_lacking_orderid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(1).

   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 1, "The orderid lacking was not recognized"). 
END.



PROCEDURE test_lacking_timestamp:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(2).

   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 2, "The timestamp lacking was not recognized"). 
END.



PROCEDURE test_lacking_loid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(3).

   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 5, "The loid lacking was not regocnized"). 
END.


PROCEDURE test_lacking_lostatusid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(4).

   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 3, "The loid lacking was not regocnized"). 
END.



PROCEDURE test_lacking_courierid:
   liOrderId = 14.
   fSetParamVars(TRUE).
   fAddMinParams(5).

   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 7, "The courierid lacking was not regocnized"). 
END.



PROCEDURE test_unexisting_orderid:
   liOrderId = 9999999.
   fSetParamVars(TRUE).
   fAddMinParams(0).

   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 20, "Not existing order failed"). 
END.


PROCEDURE test_illegal_orderid:
   gcParamStruct = add_struct("", "").
   add_string(   gcParamStruct, "Order_Id" , "ASAHHA").
   fdextra__update_order_status().

   /* Zero error code: no error */
   DEFINE VARIABLE iRetVal AS INTEGER NO-UNDO. 
   iRetVal = get_int(gcArray, "0").
   assert(iRetVal = 1, "Illegal orderid not recoginized"). 
END.

