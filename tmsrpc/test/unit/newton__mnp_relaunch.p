/**
 * This is the test set for the corresponding newton__mnp_relaunch
 * XML RPC method.
 *
 * The fixture usage of this test set:
 *
 * - fixtures fetched: 
 *
 * - The RPC method is assumed to fetch in addition following records:
 *
 *
 * - restrictions for building other fixtures: 
 */

{test_xmlrpc_includes.i}
{unit/checkutils.i}
{timestamp.i}

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 

gcFixtures = "order,ordercustomer,TMSCodes,mnpprocess". 


/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_mnp_relaunch RETURN LOGICAL:
    run_rpc_method("newton.mnp_relaunch").
    RETURN TRUE.
END FUNCTION.

DEFINE VARIABLE piOrderId AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcOldOperator AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcOldICC AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcIdType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCustomerID AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCreator AS CHARACTER NO-UNDO. 


FUNCTION fAddParams RETURN LOGICAL:
   def var lcStruct as char no-undo.

   lcStruct = add_struct("","").
   add_int(lcStruct, "order_id", piOrderId).
   add_string(lcStruct, "old_operator", pcOldOperator).
   add_string(lcStruct, "old_icc", pcOldICC).
   add_string(lcStruct, "id_type", pcIdType).
   add_string(lcStruct, "customer_id", pcCustomerId).
   add_string(lcStruct, "username", pcCreator).
   add_string(lcStruct, "company", "").
   add_string(lcStruct, "first_name", "").
   add_string(lcStruct, "surname1", "").
   add_string(lcStruct, "surname2", "").
   /*add_string(lcStruct, "company", "").*/

   lcStruct = add_struct(lcStruct, "memo").
   add_string(lcStruct, "title", "").
   add_string(lcStruct, "content", "").
   RETURN TRUE.
END.

FUNCTION fSetParamValues RETURN LOGICAL 
  (INPUT iOrderId AS INTEGER, INPUT cOldOperator AS CHARACTER,
   INPUT cOldICC AS CHARACTER, INPUT cIDType AS CHARACTER,
   INPUT cCustomerID AS CHARACTER, INPUT cCreator AS CHARACTER):

   ASSIGN piOrderId = iOrderId
          pcOldOperator = cOldOperator
          pcOldICC = cOldICC
          pcIDType = cIDType
          pcCustomerID = cCustomerID
          pcCreator = cCreator.
   RETURN TRUE.
END.


FUNCTION fCheckOrder RETURN LOGICAL:
   FIND Order WHERE Order.Brand = "1" AND Order.OrderId = piOrderId NO-LOCK NO-ERROR.
   IF AVAIL Order THEN
   DO:
      checkChar("Order.CurrOper", Order.CurrOper, pcOldOperator).
      checkChar("Order.OldICC"  , Order.OldICC  , pcOldICC).
      checkChar("Order.StatusCode", Order.StatusCode, "3").
   END.
   ELSE
      assert(FALSE, "Expected Order did not exist").
 
   RETURN TRUE.
END.

FUNCTION fCheckOrderCustomer RETURN LOGICAL:
   FIND OrderCustomer WHERE OrderCustomer.Brand = "1" AND 
                            OrderCustomer.OrderId = piOrderId AND
                            OrderCustomer.RowType = 1
                            NO-LOCK NO-ERROR.
   IF AVAIL OrderCustomer THEN
   DO:
      checkChar("OrderCustomer.CustId", OrderCustomer.CustId, 
                pcCustomerId).
      checkChar("OrderCustomer.CustIdType", OrderCustomer.CustIdType,
                pcIdType).                  
   END.
   ELSE
      assert(FALSE, "Expected OrderCustomer did not exist").

   RETURN TRUE.
END.



PROCEDURE test_normal_relaunch:
   fSetParamValues(20, "OldOper", "85412331920999", "NIE", "X23923456Y", "cr").
   fAddParams().
   call_mnp_relaunch().
   assert_success().
   assert(get_bool("", "0"), "Return value was not TRUE").
   fCheckOrder().
   fCheckOrderCustomer().
END.

PROCEDURE test_not_found_order:
   fSetParamValues(999999, "OldOper", "85412331920999", "NIE", "X23923456Y", "cr").
   fAddParams().
   call_mnp_relaunch().
   assert_fault({&APPLICATION_ERROR}, "Order 999999 not found!").
END.


PROCEDURE test_not_found_ordercustomer:
   fSetParamValues(21, "OldOper", "85412331920999", "NIE", "X23923456Y", "cr").
   fAddParams().
   call_mnp_relaunch().
   assert_fault({&APPLICATION_ERROR}, "OrderCustomer not found!").
END.


PROCEDURE test_invalid_order_status:
   fSetParamValues(22, "OldOper", "85412331920999", "NIE", "X23923456Y", "cr").
   fAddParams().
   call_mnp_relaunch().
   assert_fault({&APPLICATION_ERROR}, 
     "Cannot create new MNP process with order status 2" ).
END.


PROCEDURE test_unknown_operator:
   fSetParamValues(20, "OldOperX", "85412331920999", "NIE", "X23923456Y", "cr").
   fAddParams().
   call_mnp_relaunch().
   assert_fault({&APPLICATION_ERROR}, "Unknown operator OldOperX").
END.


PROCEDURE test_unknown_idtype:
   fSetParamValues(20, "OldOper", "85412331920999", "NIX", "X23923456Y", "cr").
   fAddParams().
   call_mnp_relaunch().
   assert_fault({&APPLICATION_ERROR}, "Unknown ID type NIX").
END.


PROCEDURE test_invalid_customer_id:
   fSetParamValues(20, "OldOper", "85412331920999", "NIE", "X23923456Z", "cr").
   fAddParams().
   call_mnp_relaunch().
   assert_fault({&APPLICATION_ERROR}, "Invalid customer ID").
END.

