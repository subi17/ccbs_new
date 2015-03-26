{test_xmlrpc_includes.i}
{unit/checkutils.i}

gcFixtures = "msrequest,customer".

DEFINE VARIABLE gcReturnArray  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 


FUNCTION requests__get RETURN LOGICAL:
    run_rpc_method("newton.requests__get").
    RETURN TRUE.
END FUNCTION.

FUNCTION fCheckReturnValueNotExist RETURN LOGICAL
   (INPUT pcParent AS CHARACTER, INPUT pcName AS CHARACTER):
  FIND tt_param WHERE tt_param.parent = pcParent AND
     tt_param.name = pcName NO-ERROR.
  IF AVAIL tt_param THEN
     ASSERT(FALSE, "Unexpected returnvalue with name " + pcName 
            + " existed in response").

  RETURN TRUE.
END.


FUNCTION fCheckRequestData RETURN LOGICAL
   (INPUT pcRequestStruct AS CHARACTER, 
    INPUT pcMsRequestFixture AS CHARACTER,
    INPUT pcCustomerFixture AS CHARACTER):

   fetch_fixture(pcMsRequestFixture, BUFFER MsRequest:HANDLE).
   IF pcCustomerFixture NE "" THEN
      fetch_fixture(pcCustomerFixture, BUFFER Customer:HANDLE).

   checkInt("MsRequest.MsRequest", MsRequest.MsRequest, 
     get_int(pcRequestStruct, "id")).
   checkInt("MsRequest.MsSeq", MsRequest.MsSeq, 
     get_int(pcRequestStruct, "mobsub_id")).
   checkChar("MsRequest.CLI", MsRequest.CLI, 
     get_string(pcRequestStruct, "msisdn")).
   /*checkDecimal("MsRequest.ActStamp", MsRequest.ActStamp,
     get_timestamp(pcRequestStruct, "activated_at")).
   checkDecimal("MsRequest.DoneStamp", MsRequest.DoneStamp,
     get_timestamp(pcRequestStruct, "handled_at")).
   checkDecimal("MsRequest.UpdateStamp", MsRequest.UpdateStamp,
     get_timestamp(pcRequestStruct, "updated_at")).
   checkDecimal("MsRequest.DoneStamp", MsRequest.CreStamp,
     get_timestamp(pcRequestStruct, "created_at")).*/
   checkInt("MsRequest.ReqType", MsRequest.ReqType,
     get_int(pcRequestStruct, "type")).
   checkDecimal("MsRequest.ReqStatus", MsRequest.ReqStatus,
     get_int(pcRequestStruct, "status")).
   checkChar("MsRequest.ReqSource", MsRequest.ReqSource,
     get_string(pcRequestStruct, "request_source")).
   checkChar("MsRequest.UserCode", MsRequest.UserCode,
     get_string(pcRequestStruct, "user_id")).
   checkLogical("MsRequest.CreateFees", MsRequest.CreateFees,
      get_bool(pcRequestStruct, "create_fees"), "").
   checkInt("MsRequest.SendSMS", MsRequest.SendSMS,
      get_int(pcRequestStruct, "send_sms")).
   checkChar("MsRequest.SMSText", MsRequest.SMSText,
      get_string(pcRequestStruct, "sms_text")).

   IF pcCustomerFixture NE "" THEN
   DO:
      IF Customer.CustIDType = "CIF" AND Customer.CompanyName > "" THEN
         checkChar("name", Customer.CompanyName + 
                           (IF Customer.CoName > "" 
                           THEN " " + Customer.CoName ELSE ""),
                           get_string(pcRequestStruct, "name")).
      ELSE 
          checkChar("name", Customer.FirstName + " " + Customer.CustName + 
                  (IF Customer.SurName2 > "" THEN " " + Customer.SurName2
                   ELSE ""), get_string(pcRequestStruct, "name")).
      checkInt("Customer.CustNum", Customer.CustNum, 
          get_int(pcRequestStruct, "customer_number")).
   END.
   ELSE
   DO:
      fCheckReturnValueNotExist(pcRequestStruct, "name").
      fCheckReturnValueNotExist(pcRequestStruct, "customer_number").
   END.

   RETURN TRUE.
END.


PROCEDURE test_existing_request_with_corp_cust:
   def var cArray as char no-undo.
   def var cGetArray as char no-undo.
   cArray = add_array("", "").
   add_string(cArray, "", "3604").
  
   requests__get().
   assert_success().

   gcReturnArray = get_array("", "").
   
   fCheckRequestData(get_struct(gcReturnArray, "0"),
      "GetHistoryReqType1SubSMS", "CustomerForDetailsCorp").
END.


PROCEDURE test_existing_request_usual_cust:
   def var cArray as char no-undo.
   def var cGetArray as char no-undo.
   cArray = add_array("", "").
   add_string(cArray, "", "3606").
   
   requests__get().
   assert_success().

   gcReturnArray = get_array("", "").

   fCheckRequestData(get_struct(gcReturnArray, "0"),
      "GetHistoryVms1", "CustomerForDetails").
END.



PROCEDURE test_existing_request_no_cust:
   def var cArray as char no-undo.
   def var cGetArray as char no-undo.
   cArray = add_array("", "").
   add_string(cArray, "", "3611").

   requests__get().
   assert_success().
   
   gcReturnArray = get_array("", "").
   fCheckRequestData(get_struct(gcReturnArray, "0"),
      "NoCustomer", "").
END.



PROCEDURE test_not_existing_request:
   def var cArray as char no-undo.
   def var cGetArray as char no-undo.
   cArray = add_array("", "").
   add_string(cArray, "", "999999").
   
   requests__get().

   assert_fault({&APPLICATION_ERROR}, 
      "Request with id 999999 was not found").
END.

