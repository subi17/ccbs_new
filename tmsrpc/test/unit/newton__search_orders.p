/**
 * This is the test set for the corresponding newton__get_order_details
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


gcFixtures = "order,ordercustomer".

DEFINE VARIABLE gcParamArray           AS CHARACTER NO-UNDO. 
gcParamArray = "".

DEFINE VARIABLE gcOrderArray           AS CHARACTER NO-UNDO.  
DEFINE VARIABLE gcReturnOrderStruct    AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iOrderId               AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cCustId                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCustIdType            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iCustNum               AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cCLI                   AS CHARACTER NO-UNDO. 


DEFINE VARIABLE liOrderId              AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liCustNum              AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcCLI                  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcfname                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lclname                AS CHARACTER NO-UNDO. 


DEFINE VARIABLE liExpectedFixtureNames AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcOrderFixtureNames         AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE lcOrderCustomerFixtureNames AS CHARACTER EXTENT 3 NO-UNDO.

DEFINE VARIABLE lLimit AS LOGICAL NO-UNDO. 


DEF VAR lLogOn AS LOGICAL NO-UNDO. 
lLogOn = FALSE.

FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF lLogOn THEN MESSAGE pcMsg.
   RETURN TRUE.
END.


FUNCTION fLogReturn RETURN LOGICAL:
   FOR EACH tt_param:
      fLog("pos: " + STRING(tt_param.position) + "," + 
           "parent: " + tt_param.parent + "," + 
           "name: " + tt_param.name + "," + 
           "type: " + tt_param.type + "," + 
           "cvalue: " + tt_param.cvalue ).
   END.
END.


/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_search_orders RETURN LOGICAL:
    fLog("before call...").
    run_rpc_method("newton.search_orders").
    fLog("gi_xmlrpc_error: " + STRING(gi_xmlrpc_error) + 
         "gc_xmlrpc_error: " + gc_xmlrpc_error).
    fLog("end call... ").
    RETURN TRUE.
END FUNCTION.


FUNCTION fCheckOrder RETURN LOGICAL
   (INPUT pcOrderFixture AS CHARACTER, INPUT pcOrderCustomerFixture AS CHARACTER ):

    
   fLog("ChkOrderAfterFixtures").
   fLog("OrderFixture: " + pcOrderFixture + ", OrderCustomerFixture: " 
        + pcOrderCustomerFixture).

   fetch_fixture(pcOrderFixture, BUFFER Order:HANDLE).
   fetch_fixture(pcOrderCustomerFixture, BUFFER OrderCustomer:HANDLE).


   liOrderId = get_int(   gcReturnOrderStruct, "tms_id" ).
   
   liCustNum = get_int(   gcReturnOrderStruct, "custnum").
   lcCli     = get_string(gcReturnOrderStruct, "cli"    ).
   lcfname   = get_string(gcReturnOrderStruct, "fname"  ).
   lclname   = get_string(gcReturnOrderStruct, "lname"  ).

   /*message "struct" liOrderId skip liCustNum skip lcCli skip lcfname   skip lclname   view-as alert-box.
   message "DB" Order.OrderId skip Order.CustNum skip Order.CLI skip OrderCustomer.FirstName   skip OrderCustomer.SurName1   view-as alert-box.*/

   checkInt( "Order.OrderId"          , Order.OrderId          , liOrderid).
   checkInt( "Order.CustNum"          , Order.CustNum          , liCustNum).
   checkChar("Order.CLI"              , Order.CLI              , lcCLI    ).


   fLog("OrderCustomer.OrderId: " + STRING(OrderCustomer.OrderId)).
   checkChar("OrderCustomer.FirstName", OrderCustomer.FirstName, lcfname).
   checkChar("OrderCustomer.SurName1" , OrderCustomer.SurName1 , lclname).

   fLog("Checks passed").
END.


FUNCTION fCheckOrderIdData RETURN LOGICAL:
   gcReturnOrderStruct = get_struct(gcOrderArray, "").
   fLog("CheckOrderIdData").
   fLog("gcReturnOrderStruct: " + gcReturnOrderStruct ).
   CASE iOrderId:
      WHEN 7 THEN fCheckOrder("OrderForSearchOrder1","SearchOrderOrderCustomer1").
   END.

   RETURN TRUE.
END.


FUNCTION fGetFixtureNames RETURN LOGICAL (INPUT piCase AS INTEGER):
  DEFINE VARIABLE lFound AS LOGICAL NO-UNDO. 

  CASE piCase:

     WHEN 1 THEN
        DO:
        ASSIGN liExpectedFixtureNames = 1
               lcOrderFixtureNames[1]         = "OrderForSearchOrder4"
               lcOrderCustomerFixtureNames[1] = "SearchOrderOrderCustomer3"

               lFound = TRUE.
        fLog("fGetFixtureNames1").
        END.
     WHEN 2 THEN
        DO:
        ASSIGN liExpectedFixtureNames = 2
               lcOrderFixtureNames[1]         = "OrderForSearchOrder1"
               lcOrderCustomerFixtureNames[1] = "SearchOrderOrderCustomer1"
      
               lcOrderFixtureNames[2]         = "OrderForSearchOrder3"
               lcOrderCustomerFixtureNames[2] = "SearchOrderOrderCustomer2"

               lFound = TRUE.
        fLog("fGetFixtureNames2").
        END.
    WHEN 3 THEN
        DO:
        ASSIGN liExpectedFixtureNames = 1
               lcOrderFixtureNames[1]         = "OrderForSearchOrder1"
               lcOrderCustomerFixtureNames[1] = "SearchOrderOrderCustomer1"

               lFound = TRUE.
        fLog("fGetFixtureNames3").
        END.
  END.
  fLog("getFixtureNames, liExpectedFixtureNames = " + STRING(liExpectedFixtureNames)).
END.


FUNCTION fCheckDataWithFixtureNames RETURN LOGICAL:
   DEFINE VARIABLE iFixName AS INTEGER NO-UNDO. 
   fLog("Number of fixtures: " + STRING(liExpectedFixtureNames)).
   REPEAT iFixName = 1 TO liExpectedFixtureNames:
      gcReturnOrderStruct = get_struct(gcOrderArray, STRING(iFixName - 1)).
      fLog("gcReturnOrderStruct: " + gcReturnOrderStruct ).

      /*message "testing >>>>>" lcOrderFixtureNames[iFixName] " " lcOrderCustomerFixtureNames[iFixName] view-as alert-box.*/

      fCheckOrder(lcOrderFixtureNames[iFixName], lcOrderCustomerFixtureNames[iFixName]).
   END.
END.


FUNCTION fCheckCustIdData RETURN LOGICAL:
   gcOrderArray = get_array("","").

   IF NOT lLimit THEN
      CASE cCustId:
         WHEN "SeekCustId" THEN fGetFixtureNames(2). 
         WHEN "SeekCustId2" THEN fGetFixtureNames(1). 
      END.
   ELSE
      CASE cCustId:
        WHEN "SeekCustId" THEN fGetFixtureNames(3).
      END.


   fCheckDataWithFixtureNames().

   RETURN TRUE.
END.


FUNCTION fCheckMSISDNData RETURN LOGICAL:
   DEFINE VARIABLE iExpectedOrderCount AS INTEGER NO-UNDO. 
   gcOrderArray = get_array("","").

   IF NOT lLimit THEN
      CASE cCLI:
         WHEN "622000000" THEN fGetFixtureNames(2).
         WHEN "633000000" THEN fGetFixtureNames(1).
      END.
   ELSE
      CASE cCLI:
         WHEN "622000000" THEN fGetFixtureNames(3).
      END.

   fCheckDataWithFixtureNames().

   RETURN TRUE.
END.


PROCEDURE test_search_orders_by_orderid_not_found_order:
   ASSIGN iOrderId = 111000
          cCustId  = ""
          cCustIdType = ""
          cCLI     = ""
          lLimit   = FALSE.


   add_string(gcParamArray, "", STRING(iOrderId)).
   add_string(gcParamArray, "", "tms_id").
   add_string(gcParamArray, "", "").
   add_int(gcParamArray, "", 5).
   call_search_orders().
   assert_success().
    
   gcOrderArray = get_array("","").
   IF get_paramcount(gcOrderArray) <> 0 THEN
      assert(TRUE, "Not existing Order returned order").
END.




PROCEDURE test_search_orders_by_orderid_not_found_ordercustomer:

   ASSIGN iOrderId = 8
          cCustId  = ""
          cCustIdType = ""
          cCLI     = ""
          lLimit   = FALSE.

   add_string(gcParamArray, "", STRING(iOrderId)).
   add_string(gcParamArray, "", "tms_id").
   add_string(gcParamArray, "", ""). 
   add_int(gcParamArray, "", 5).
   
   call_search_orders().
   assert_success().
  
   gcOrderArray = get_array("","").
   IF get_paramcount(gcOrderArray) <> 0 THEN
      assert(TRUE, "Order without OrderCustomer returned order").
END.



PROCEDURE test_search_orders_by_orderid_found_ordercustomer:
   ASSIGN iOrderId = 7
          cCustId  = ""
          cCustIdType = ""
          cCLI     = ""
          lLimit   = FALSE.

   fLog("here").
   add_string(gcParamArray, "", STRING(iOrderId)).
   add_string(gcParamArray, "", "tms_id").
   add_string(gcParamArray, "", "").
   add_int(gcParamArray, "", 5).

   call_search_orders().
   assert_success().

/*   fLogReturn(). */
   gcOrderArray = get_array("","").
   gcReturnOrderStruct = get_struct(gcOrderArray, "0").
   fCheckOrderIdData().
END.



PROCEDURE test_search_orders_by_custid_found_one:
   ASSIGN iOrderId = 0
          cCustId =  "SeekCustId2"
          cCustIdType = "SeekCustIdType2"
          cCLI     = ""
          lLimit   = FALSE.

   add_string(gcParamArray, "", cCustId).
   add_string(gcParamArray, "", "custid").
   add_string(gcParamArray, "", cCustIdType).
   add_int(gcParamArray, "", 5).
   
   call_search_orders().
   assert_success().
   
   fCheckCustIdData().
END.


PROCEDURE test_search_orders_by_custid_found_one_with_limit:
   ASSIGN iOrderId = 0
          cCustId  = "SeekCustId"
          cCustIdType = "SeekCustIdType"
          cCLI     = ""
          lLimit   = TRUE.

   fLog("test_search_orders_by_custid").

   add_string(gcParamArray, "", cCustId).
   add_string(gcParamArray, "", "custid").
   add_string(gcParamArray, "", cCustIdType).
   add_int(gcParamArray, "", 1).
   
   call_search_orders().
   assert_success().
   
   fCheckCustIdData().
END.





PROCEDURE test_search_orders_by_custid_found_two_not_limited:
   ASSIGN iOrderId = 0
          cCustId = "SeekCustId"
          cCustIdType = "SeekCustIdType"
          cCLI     = ""
          lLimit   = false.

   add_string(gcParamArray, "", cCustId).
   add_string(gcParamArray, "", "custid").
   add_string(gcParamArray, "", cCustIdType).
   add_int(gcParamArray, "", 5).

   call_search_orders().
   assert_success().
   
   fCheckCustIdData().
END.



PROCEDURE test_search_orders_by_msisdn_found_one:
   ASSIGN iOrderId = 0
          cCustId = ""
          cCustIdType = ""
          cCLI     = "633000000"
          lLimit   = FALSE.

   add_string(gcParamArray, "", cCLI).
   add_string(gcParamArray, "", "msisdn").
   add_string(gcParamArray, "", "").
   add_int(gcParamArray, "", 5).

   call_search_orders().
   assert_success().
   
   fCheckMSISDNData().
END.


PROCEDURE test_search_orders_by_msisdn_found_one_with_limit:
   ASSIGN iOrderId = 0
          cCustId  = ""
          cCustIdType = ""
          cCLI     = "633000000"
          lLimit   = TRUE.

   add_string(gcParamArray, "", cCLI).
   add_string(gcParamArray, "", "msisdn").
   add_string(gcParamArray, "", "").
   add_int(gcParamArray, "", 1).

   call_search_orders().
   assert_success().
   
   fCheckMSISDNData().
END.



PROCEDURE test_search_orders_by_msisdn_found_two:
   ASSIGN iOrderId = 0
          cCustId  = ""
          cCustIdType = ""
          cCLI     = "622000000"
          lLimit   = FALSE.

   add_string(gcParamArray, "", cCLI).
   add_string(gcParamArray, "", "msisdn").
   add_string(gcParamArray, "", "").
   add_int(gcParamArray, "", 5).
   
   call_search_orders().
   assert_success().

   fCheckMSISDNData().
END.


