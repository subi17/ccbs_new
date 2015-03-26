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
{xmlfunction.i}

gcFixtures = "order,ordercustomer,OrderAccessory,orderpayment,BillItem,mnpprocess,mnpmessage".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 

/* Order fields in variables */
DEFINE VARIABLE lcCampaign AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcContactNum AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcContractID AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCreditRate AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCrStamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCurrOper AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustNum AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDeviceID AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcICC AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMNPNumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMNPStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldIcc AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOrderChannel AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOrderer AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOrdererID AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOrdererIDType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOrdererIP AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOrderType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPayType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPNPNumbers AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSalesman AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSource AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStatusCode AS CHARACTER NO-UNDO.

DEFINE VARIABLE lOldPayType AS LOGICAL NO-UNDO.
DEFINE VARIABLE ldeOrderCreationTime AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcReferee AS CHARACTER NO-UNDO. 

/* mnpprocess fields in variables */
DEFINE VARIABLE ldeMnpProcessCreatedTS AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeMnpProcessUpdateTs  AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcMnpProcessPortRequest AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMnpProcessFormRequest AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMnpProcessUserCode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMnpProcessStatus AS CHARACTER NO-UNDO. 


/* mnpmessage fields in variables */
DEFINE VARIABLE ldeMnpMessageCreatedTs AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcMnpMessageType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMnpMessageFrom    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcMnpMessageStatus  AS CHARACTER NO-UNDO. 

/* orderpayment */
DEFINE VARIABLE lcCCReference AS CHARACTER NO-UNDO. 

/* orderaccessory */
DEFINE VARIABLE lcCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcImei AS CHARACTER NO-UNDO. 


DEFINE VARIABLE lcAmount AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcVatAmount AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDiscount  AS CHARACTER NO-UNDO. 



/* delivery address */
DEFINE VARIABLE lcDelivTitle AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelivFirstName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelivSurName1 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelivSurName2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelivZipCode  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelivRegion   AS CHARACTER NO-UNDO. 


/* structs and logic variables */
DEFINE VARIABLE liOrderId AS INTEGER NO-UNDO.


DEFINE VARIABLE gcSubscriptionStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcMnpStruct          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcArrayMnpMessages   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iNumberOfMnpMessages AS INTEGER NO-UNDO. 

DEFINE VARIABLE iMnpMessage AS INTEGER NO-UNDO. 
DEFINE VARIABLE gcMnpMessage AS CHARACTER NO-UNDO. 


DEF VAR lLogOn AS LOGICAL NO-UNDO. 
lLogOn = FALSE.

FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF lLogOn THEN MESSAGE pcMsg.
   RETURN TRUE.
END.


FUNCTION fLogLong RETURN LOGICAL (INPUT pcMsg AS CHARACTER):

   DEFINE VARIABLE iRow AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cRow AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE iRowLength AS INTEGER NO-UNDO. 
   iRowLength = 80.

   DEFINE VARIABLE iRows AS INTEGER NO-UNDO. 
   iRows = LENGTH(pcMsg) / iRowLength + 1.
 

   IF lLogOn THEN
   DO:
      MESSAGE "Length : " LENGTH(pcMsg).
      MESSAGE "Rows:    " STRING(iRows).
      REPEAT iRow = 1 TO iRows:
           IF iRow * iRowLength > LENGTH(pcMsg) THEN
              MESSAGE SUBSTRING(pcMsg, (iRow - 1) * iRowLength + 1).
           ELSE
              MESSAGE SUBSTRING(pcMsg, (iRow - 1) * iRowLength + 1, iRowLength).
      END.
   END.
   RETURN TRUE.
END.


/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_get_order_details RETURN LOGICAL:
    fLog("before call...").
    run_rpc_method("newton.get_order_details").
    fLog("end call... ").
    RETURN TRUE.
END FUNCTION.

/** 
 * This function checks data returned from XML RPC from order struct
 * concerning OrderPayment record. There exist 3 orders (orderid 11-13)
 * with different OrderPayment opportunities, two different existing
 * OrderPayment (orderid:11-12) and one order without orderpayment 
 * (orderid:13)
 *
 * @param orderstruct identifier
 */
FUNCTION fCheckOrderPayment RETURN LOGICAL
   (INPUT pcOrderStruct AS CHARACTER):

   DEFINE VARIABLE  lcOrderPaymentFixture AS CHARACTER NO-UNDO.
   lcCCReference = get_string(pcOrderStruct, "creditcard_ref").

   CASE iOrderid:
      WHEN 11 THEN lcOrderPaymentFixture = "OrderDetail1Payment".
      WHEN 12 THEN lcOrderPaymentFixture = "OrderDetail2Payment".
      WHEN 13 THEN lcOrderPaymentFixture = "".

   END.

   IF lcOrderPaymentFixture <> "" THEN
   DO:
      fetch_fixture(lcOrderPaymentFixture, BUFFER OrderPayment:HANDLE).
      checkChar("OrderPayment.CCReference",  OrderPayment.CCReference, lcCCReference).
   END.
   ELSE
      checkChar("OrderPayment.CCReference", "", lcCCReference).
 
   RETURN TRUE.
END.


FUNCTION fGetOrderFieldsToVars RETURN LOGICAL
   (INPUT pcStruct AS CHARACTER):

   liOrderId      = get_int(   pcStruct, "tms_id").
   lcStatusCode   = get_string(pcStruct, "status").
   lcOrderChannel = get_string(pcStruct, "order_channel").
   lcContractId   = get_string(pcStruct, "contract_number").
   lcSalesman     = get_string(pcStruct, "salesman_id").
   lcCampaign     = get_string(pcStruct, "campaign").
   lcPayType      = get_string(pcStruct, "payment_type").
   lcOrdererIP    = get_string(pcStruct, "orderer_ip").
   lcReferee            = get_string( pcStruct, "referee").
   ldeOrderCreationTime = get_timestamp( pcStruct, "creation_time").

   gcSubscriptionStruct = get_struct(pcStruct, "subscription").

   lOldPayType   = get_bool(  gcSubscriptionStruct, "old_payment_method").
   lcCurrOper    = get_string(gcSubscriptionStruct, "old_operator").
   lcOldIcc      = get_string(gcSubscriptionStruct, "old_icc").
   lcICC     = get_string(gcSubscriptionStruct, "icc").
   lcCLI     = get_string(gcSubscriptionStruct, "cli").
   lcCLIType = get_string(gcSubscriptionStruct, "subscription_type").
   lcPaytype  = get_string(  gcSubscriptionStruct, "number_type").
 
   RETURN TRUE.
END.




FUNCTION fGetOrderAccessoryToVars RETURN LOGICAL
   (INPUT pcOrderAccessoryStruct AS CHARACTER):
   
   lcCode      = get_string(pcOrderAccessoryStruct, "billing_item_id").
   lcAmount    = string(get_double(pcOrderAccessoryStruct, "amount")).
   /*lcName      = get_string(pcOrderAccessoryStruct, "name").*/
   lcImei      = get_string(pcOrderAccessoryStruct, "imei").
   lcVatAmount = string(get_double(pcOrderAccessoryStruct, "vat_amount")).
   lcDiscount  = string(get_double(pcOrderAccessoryStruct, "discount")).

   

   RETURN TRUE.
END.




FUNCTION fGetDeliveryCustomerToVars RETURN LOGICAL
   (INPUT pcDeliveryCustStruct AS CHARACTER):
   
   lcDelivTitle     = get_string(pcDeliveryCustStruct, "title").
   lcDelivFirstName = get_string(pcDeliveryCustStruct, "first_name").
   lcDelivSurName1  = get_string(pcDeliveryCustStruct, "surname_1").
   lcDelivSurName2  = get_string(pcDeliveryCustStruct, "surname_2").
   lcDelivZipCode   = get_string(pcDeliveryCustStruct, "zip_code").
   lcDelivRegion    = get_string(pcDeliveryCustStruct, "region").

   RETURN TRUE.
END.




FUNCTION fGetMnpMessageFieldsToVars RETURN LOGICAL
   (INPUT pcMnpMessageStruct AS CHARACTER):

   lcMnpMessageType        = get_string(   pcMnpMessageStruct, "type").
   lcMnpMessageFrom        = get_string(   pcMnpMessageStruct, "from").
   lcMnpMessageStatus      = get_string(   pcMnpMessageStruct, "status").
   ldeMnpMessageCreatedTs  = get_timestamp( pcMnpMessageStruct, "date_time").
 
   fLog("MnpMessageStatus: " + lcMnpMessageStatus ).


  RETURN TRUE.
END.



FUNCTION fGetExpectedMnpMessageStatus RETURN CHARACTER
   (INPUT pcFixName AS CHARACTER):

   DEFINE VARIABLE lcRetVal AS CHARACTER NO-UNDO. 

   CASE pcFixName:

      WHEN "OrderDetails1MnpMessage1" THEN lcRetVal = "AENV".
      WHEN "OrderDetails1MnpMessage2" THEN lcRetVal = "ASOL".
      WHEN "OrderDetails1MnpMessage3" THEN lcRetVal = "".
      WHEN "OrderDetails2MnpProcess1MnpMessage" THEN lcRetVal = "NEW".
      WHEN "OrderDetails2MnpProcess2MnpMessage" THEN lcRetVal = "ACON".
   END.
   RETURN lcRetVal.

END.



FUNCTION fCheckMnpMessage RETURN LOGICAL 
  (INPUT pcMnpMessageStruct AS CHARACTER,
   INPUT pcMnpMessageFixture AS CHARACTER,
   INPUT piMnpMessage AS INTEGER):


  fGetMnpMessageFieldsToVars(pcMnpMessageStruct).

  DEFINE VARIABLE ldCreatedTs AS DECIMAL NO-UNDO. 
  fetch_fixture(pcMnpMessageFixture, BUFFER MnpMessage:HANDLE). 

  fLog("MnpMessageFixture: " + pcMnpMessageFixture).

  IF AVAIL MnpMessage THEN
  DO:
     fLog("ldeMnpMessageCreatedTs: " + STRING(ldeMnpMessageCreatedTs)). 

     IF MnpMessage.Sender = 1 THEN
        checkChar( "MnpMessage.Sender", "TMS"     , lcMnpMessageFrom).
     ELSE
        checkChar( "MnpMessage.Sender", "MNP"     , lcMnpMessageFrom).  


     DEFINE VARIABLE lcExpStatus AS CHARACTER NO-UNDO. 
     lcExpStatus = fGetExpectedMnpMessageStatus(pcMnpMessageFixture).
     checkChar("MnpMessage.XMLMessage", lcExpStatus, lcMnpMessageStatus).

     fLog("MnpMessage.CreatedTs: " + STRING(MnpMessage.CreatedTs)).
     checkDecimal("MnpMessage.CreatedTs", MnpMessage.CreatedTs, ldeMnpMessageCreatedTs). 
     checkChar( "NmpMessage.MessageType", MnpMessage.MessageType , lcMnpMessageType).

  END.
  ELSE
     fLog("MnpMessage not found.").

  RETURN TRUE.
END.


FUNCTION fGetMnpProcessFieldsToVars RETURN LOGICAL
   (INPUT pcMnpProcessStruct AS CHARACTER):

   lcMnpProcessFormRequest  = get_string(pcMnpProcessStruct, "tms_request_id").
   lcMnpProcessPortRequest    = get_string(pcMnpProcessStruct, "mnp_request_id").
END.


FUNCTION fCheckMnpProcess RETURN LOGICAL
  (INPUT pcMnpProcessStruct AS CHARACTER,
   INPUT piProcessStruct AS INTEGER,
   INPUT piNumberOfMessages AS INTEGER):

  DEFINE VARIABLE lcMnpMessageArray AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE lcMnpMessageStruct AS CHARACTER NO-UNDO. 

  fGetMnpProcessFieldsToVars(pcMnpProcessStruct).

  /* check mnp process fields */
  checkChar("MnpProcess.FormRequest", MnpProcess.FormRequest, lcMnpProcessFormRequest).
  checkChar("MnpProcess.PortRequest", MnpProcess.PortRequest, lcMnpProcessPortRequest).

  lcMnpMessageArray = get_array(pcMnpProcessStruct, "mnpmessages").

  IF piNumberOfMessages = 1 THEN
  DO:
     lcMnpMessageStruct = get_struct(lcMnpMessageArray, "0").
     IF piProcessStruct = 1 THEN
        fCheckMnpMessage(lcMnpMessageStruct, "OrderDetails2MnpProcess1MnpMessage",
           1).
     ELSE
        fCheckMnpMessage(lcMnpMessageStruct, "OrderDetails2MnpProcess2MnpMessage",
           2).
  END.
  ELSE /* Expected 3 */
  DO:
     lcMnpMessageStruct = get_struct(lcMnpMessageArray, "0").
     fCheckMnpMessage(lcMnpMessageStruct, "OrderDetails1MnpMessage1", 3).
     lcMnpMessageStruct = get_struct(lcMnpMessageArray, "1").
     fCheckMnpMessage(lcMnpMessageStruct, "OrderDetails1MnpMessage2", 4).
     lcMnpMessageStruct = get_struct(lcMnpMessageArray, "2").
     fCheckMnpMessage(lcMnpMessageStruct, "OrderDetails1MnpMessage3", 5).

  END.

  RETURN TRUE.
END.


FUNCTION fCheckMnpProcesses RETURN LOGICAL
   (INPUT pcOrderStruct AS CHARACTER):


   DEFINE VARIABLE lcMnpProcessesArray AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMnpProcessStruct AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMnpProcessFixture AS CHARACTER NO-UNDO. 

   lcMnpProcessesArray = get_array(pcOrderStruct, "mnp").


   CASE iOrderId:
       WHEN 11 THEN
       DO: 
           /* 1 MnpProcess, 3 MnpMessage */
           lcMnpProcessStruct = get_struct(lcMnpProcessesArray, "0").
           fetch_fixture("OrderDetails1MnpProcess1", 
                        BUFFER MnpProcess:HANDLE).
           fCheckMnpProcess(lcMnpProcessStruct, 1, 3). 
       END.
      
       WHEN 12 THEN
       DO:
           /* 2 MnpProcess, each has 1 MnpMessage */
           lcMnpProcessStruct = get_struct(lcMnpProcessesArray, "0").
           fetch_fixture("OrderDetails2MnpProcess1", 
                        BUFFER MnpProcess:HANDLE).
           fCheckMnpProcess(lcMnpProcessStruct, 1, 1). 
           fetch_fixture("OrderDetails2MnpProcess2", 
                        BUFFER MnpProcess:HANDLE).
           lcMnpProcessStruct = get_struct(lcMnpProcessesArray, "1").
           fCheckMnpProcess(lcMnpProcessStruct, 2, 1). 
       END.

       WHEN 13 THEN
       DO:
          /* 0 MnpProcess, 0 MnpMessage */
           assert(get_paramcount(lcMnpProcessesArray) eq 0, 
                  "There was mnp processes for the order although there should not be.").
       END.
   END.

   RETURN TRUE.
END.


/**
 * This function checks data returned from XML RPC orderstruct concerning
 * the Order record itself. The fields checked include OrderId, StatusCode,
 * OrderChannel, ContractId, Salesman, Campaign, OrdererIP, CurrOper,
 * OldICC, OldPayType, ICC, CLI, CLIType and PayType. The data returned
 * from order struct is first got to variables using function
 * fGetOrderFieldsToVars.
 *
 * @param orderstruct
 */
FUNCTION fCheckOrderFields RETURN LOGICAL
   (INPUT pcOrderStruct AS CHARACTER):

   DEFINE VARIABLE lcOrderFixtureName AS CHARACTER NO-UNDO. 
   CASE iOrderId:
      WHEN 11 THEN lcOrderFixtureName = "OrdersForGetOrderDetails1".
      WHEN 12 THEN lcOrderFixtureName = "OrdersForGetOrderDetails2".
      WHEN 13 THEN lcOrderFixtureName = "OrdersForGetOrderDetails3".
   END.

   fGetOrderFieldsToVars(pcOrderStruct).
   fetch_fixture(lcOrderFixtureName, BUFFER Order:HANDLE).
 
   fLog("Order.DepoFeeAmt: " + STRING(Order.DepoFeeAmt)).

  /* message liOrderId " " lcStatusCode " " lcOrderChannel view-as alert-box. */

   checkInt("Order.OrderId", Order.OrderId, liOrderId).
   checkChar("Order.StatusCode", Order.StatusCode, lcStatusCode).
   checkChar("Order.OrderChannel", Order.OrderChannel, lcOrderChannel).
   checkChar("Order.ContractId", Order.ContractId, lcContractId).
   checkChar("Order.Salesman", Order.Salesman, lcSalesman).
   checkChar("Order.Campaign", Order.Campaign, lcCampaign).
   checkChar("Order.ContractId", Order.ContractId, lcContractId).
   checkChar("Order.OrdererIP", Order.OrdererIP, lcOrdererIP).
   checkChar("Order.CurrOper", Order.CurrOper, lcCurrOper).
   checkChar("Order.OldICC", Order.OldICC, lcOldICC).
   checkLogical("Order.OldPayType", Order.OldPayType, false,
      "OldPaytype did not match").
   checkChar("Order.Referee", Order.Referee, lcReferee).
   checkDecimal("Order.CrStamp", Order.CrStamp, ldeOrderCreationTime).

   /* subscription struct */
   checkChar("Order.ICC", Order.ICC, lcICC).
   checkChar("Order.CLI", Order.CLI, lcCLI).
   checkChar("Order.CLIType", Order.CLIType, lcCLIType).
   
   IF Order.PayType eq FALSE THEN
      checkChar("Order.PayType", "PostPaid", lcPayType).
   ELSE
      checkChar("Order.PayType", "PrePaid", lcPayType).

   RETURN TRUE.
END.


FUNCTION fCheckOrderAccessoryFields RETURN LOGICAL
   (INPUT pcOrderAccessoryStruct AS CHARACTER,
    INPUT pcOrderAccessoryFixture AS CHARACTER,
    INPUT pcBillItemFixture AS CHARACTER):

   fGetOrderAccessoryToVars(pcOrderAccessoryStruct).
   fetch_fixture(pcOrderAccessoryFixture, BUFFER OrderAccessory:HANDLE).

   /*message lcCode skip lcIMEI skip lcAmount skip lcVatAmount skip lcDiscount view-as alert-box.*/

   checkChar("OrderAccessory.ProductCode", OrderAccessory.ProductCode, lcCode). 
   checkChar("OrderAccessory.IMEI"       , OrderAccessory.IMEI       , lcIMEI). 
   checkChar("OrderAccessory.Amount"     , 
        STRING(OrderAccessory.Amount)     , lcAmount).
   checkChar("OrderAccessory.VatAmount"  , 
        STRING(OrderAccessory.VatAmount)  , lcVatAmount).
   checkChar("OrderAccessory.Discount"   , 
        STRING(OrderAccessory.Discount)   , lcDiscount). 


   /*IF pcBillItemFixture <> "" THEN
   DO:
       fetch_fixture(pcBillItemFixture, BUFFER BillItem:HANDLE).
       checkChar("BillItem.Name", BillItem.BIName, lcName).
   END.
   ELSE
       checkChar("BillItem.Name", "", lcName).*/
END.


FUNCTION fCheckOrderAccessories RETURN LOGICAL 
   (INPUT pcOrderStruct AS CHARACTER):

   DEFINE VARIABLE lcTerminalsArray AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcOrderAccessoryStruct AS CHARACTER NO-UNDO. 

   lcTerminalsArray = get_array(pcOrderStruct, "order_terminals").
   CASE iOrderId:
       WHEN 11 THEN
       DO:
           /* One OrderAccessory */
           lcOrderAccessoryStruct = get_struct(lcTerminalsArray, "0").
           fCheckOrderAccessoryFields(lcOrderAccessoryStruct, 
              "OrderDetails1Terminal", "OrderDetailsFirstProd").
       END.
       WHEN 12 THEN
       DO:
          /* Two OrderAccessories */
           lcOrderAccessoryStruct = get_struct(lcTerminalsArray, "0").
           fCheckOrderAccessoryFields(lcOrderAccessoryStruct, 
             "OrderDetails2Terminal1", "OrderDetailsSecondProd").

           lcOrderAccessoryStruct = get_struct(lcTerminalsArray, "1").
           fCheckOrderAccessoryFields(lcOrderAccessoryStruct, 
             "OrderDetails2Terminal2", "OrderDetailsFirstProd").
       END.
       WHEN 13 THEN
       DO:
           /* No OrderAccessories */
           assert(get_paramcount(lcTerminalsArray) eq 0, 
                  "There was terminals for the order although there should not be.").
       END.
   END.

END.


FUNCTION fCheckDeliveryAddressFields RETURN LOGICAL
   (INPUT pcOrderStruct AS CHARACTER):


   DEFINE VARIABLE lcOrderCustomerStruct AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOrderCustomerFixture AS CHARACTER NO-UNDO.

   CASE iOrderId:
      WHEN 11 THEN
      DO:
         lcOrderCustomerStruct = get_struct(pcOrderStruct, "delivery_address").
         lcOrderCustomerFixture = "OrderDetailOrderCustomer1".
      END.

      WHEN 12 THEN
      DO:
         lcOrderCustomerStruct = get_struct(pcOrderStruct, "delivery_address").
         lcOrderCustomerFixture = "OrderDetailOrderCustomer2".
      END.

      WHEN 13 THEN
      DO:
         lcOrderCustomerFixture = "".
      END.
   END.

   IF lcOrderCustomerFixture <> "" THEN
   DO:
      fGetDeliveryCustomerToVars(lcOrderCustomerStruct).
      fetch_fixture(lcOrderCustomerFixture, BUFFER OrderCustomer:HANDLE).

      checkChar("OrderCustomer.CustTitle", OrderCustomer.CustTitle, lcDelivTitle).
      checkChar("OrderCustomer.FirstName", OrderCustomer.FirstName, lcDelivFirstName).
      checkChar("OrderCustomer.SurName1" , OrderCustomer.SurName1 , lcDelivSurName1).
      checkChar("OrderCustomer.SurName2" , OrderCustomer.SurName2 , lcDelivSurName2).
      checkChar("OrderCustomer.ZipCode"  , OrderCustomer.ZipCode  , lcDelivZipCode).
      checkChar("OrderCustomer.Region"   , OrderCustomer.Region   , lcDelivRegion).
   END.
   
   RETURN TRUE.
END.




FUNCTION fCheckData RETURN LOGICAL:
   DEFINE VARIABLE lcMainStruct AS CHARACTER NO-UNDO. 

   lcMainStruct = get_struct("", "").
   fLog("Beginning checkData, orderid = " + STRING(iOrderId)).

   fCheckOrderFields(lcMainStruct).
   fLog("Passed order fields").
   
   fCheckOrderAccessories(lcMainStruct).
   fLog("Passed OrderAccessories").

   fCheckOrderPayment(lcMainStruct).
   fLog("Passed OrderPayment").

   fCheckDeliveryAddressFields(lcMainStruct).
   fLog("Passed delivery address").

   fCheckMnpProcesses(lcMainStruct).
   fLog("Passed mnp processes").

   fLog("End checkdata").
END.



PROCEDURE test_get_not_existing_order:
   iOrderId = 33773449.
   add_int(gcParamArray, "tms_id", iOrderId).
   call_get_order_details().

   assert_fault({&APPLICATION_ERROR},
      SUBST("Order with OrderId &1 does not exist", iOrderId )).
END.



PROCEDURE test_get_order_with_no_terminal:
   iOrderId = 13.
   add_int(gcParamArray, "tms_id", iOrderId).
   call_get_order_details().
   assert_success().
   fCheckData().
END.


PROCEDURE test_get_order_with_one_terminal:
   iOrderId = 11.
   add_int(gcParamArray, "tms_id", iOrderId).
   call_get_order_details().

   assert_success().

   fCheckData().
END.


PROCEDURE test_get_order_with_multiple_terminals:
   iOrderId = 11.
   add_int(gcParamArray, "tms_id", iOrderId).
   call_get_order_details().
   assert_success().
   fCheckData().
END.
