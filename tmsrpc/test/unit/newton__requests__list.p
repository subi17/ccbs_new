
{test_xmlrpc_includes.i}
{unit/checkutils.i}


gcFixtures = "msrequest,MsReqStatistic,customer".

DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
gcParamStruct = "".

DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcRequestArray AS CHARACTER NO-UNDO. 


FUNCTION request__list RETURN LOGICAL:
    run_rpc_method("newton.requests__list").
    RETURN TRUE.
END FUNCTION.

FUNCTION fAddParams RETURN LOGICAL
  (INPUT piReqType AS INTEGER,
   INPUT piReqStatus AS INTEGER,
   INPUT piOffSet AS INTEGER,
   INPUT piLimit AS INTEGER):
   gcParamStruct = add_struct("", "").
   add_int(gcParamStruct, "type", piReqType).
   add_int(gcParamStruct, "status", piReqStatus).
   add_int(gcParamStruct, "offset", piOffSet).
   add_int(gcParamStruct, "limit", piLimit).
   
   RETURN TRUE.
END.


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
   checkChar("MsRequest.MSISDN", MsRequest.CLI, 
     get_string(pcRequestStruct, "msisdn")).
   checkDecimal("MsRequest.ActStamp", MsRequest.ActStamp,
     get_timestamp(pcRequestStruct, "activated_at")).
   checkDecimal("MsRequest.DoneStamp", MsRequest.DoneStamp,
     get_timestamp(pcRequestStruct, "handled_at")).
   checkInt("MsRequest.ReqType", MsRequest.ReqType,
     get_int(pcRequestStruct, "type")).
   checkDecimal("MsRequest.ReqStatus", MsRequest.ReqStatus,
     get_int(pcRequestStruct, "status")).

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
   END.
   ELSE
      fCheckReturnValueNotExist(pcRequestStruct, "name").

   RETURN TRUE.
END.


FUNCTION fCheckTotal RETURN LOGICAL 
  (INPUT pcRespStruct AS CHARACTER, INPUT pcMsReqStatFixture AS CHARACTER):

  IF pcMsReqStatFixture NE "" THEN
  DO:
     fetch_fixture(pcMsReqStatFixture, BUFFER MsReqStatistic:HANDLE).
     checkInt("MsReqStatistic.ReqStatusCount", MsReqStatistic.ReqStatusCount,
        get_int(pcRespStruct, "total")).
  END.
  ELSE
     checkInt("MsReqStatistic.ReqStatusCount", 0,
        get_int(pcRespStruct, "total")).

  RETURN TRUE.
END.


PROCEDURE test_requests_from_beginning:
   fAddParams(1, 2, 0, 2).
   request__list().
   assert_success().
   gcReturnStruct = get_struct("", "").
   fCheckTotal(gcReturnStruct, "ReqType1Status2").
   gcRequestArray = get_array(gcReturnStruct, "requests").
   checkInt("count of array", 2, get_paramcount(gcRequestArray)).
   fCheckRequestData(get_struct(gcRequestArray, "0"),
      "GetHistoryVms2", "CustomerForDetailsCorp").
   fCheckRequestData(get_struct(gcRequestArray, "1"),
      "GetHistoryVms3", "").
END.


PROCEDURE test_requests_from_offset:
   fAddParams(1, 2, 1, 2).
   request__list().
   assert_success().
   gcReturnStruct = get_struct("", "").
   fCheckTotal(gcReturnStruct, "ReqType1Status2").
   gcRequestArray = get_array(gcReturnStruct, "requests").
   checkInt("count of array", 2, get_paramcount(gcRequestArray)).
   fCheckRequestData(get_struct(gcRequestArray, "0"),
      "GetHistoryVms3", "").
   fCheckRequestData(get_struct(gcRequestArray, "1"),
      "GetHistoryVms1", "CustomerForDetails").
END.



PROCEDURE test_all_requests_looped:
   fAddParams(1, 2, 0, 7).
   request__list().
   assert_success().
   gcReturnStruct = get_struct("", "").
   fCheckTotal(gcReturnStruct, "ReqType1Status2").
   gcRequestArray = get_array(gcReturnStruct, "requests").
   checkInt("count of array", 5, get_paramcount(gcRequestArray)).
   fCheckRequestData(get_struct(gcRequestArray, "0"),
      "GetHistoryVms2", "CustomerForDetailsCorp").
   fCheckRequestData(get_struct(gcRequestArray, "1"),
      "GetHistoryVms3", "").
   fCheckRequestData(get_struct(gcRequestArray, "2"),
      "GetHistoryVms1", "CustomerForDetails").
   fCheckRequestData(get_struct(gcRequestArray, "3"),
      "GetHistoryReqType1SubSMS", "CustomerForDetailsCorp").
   fCheckRequestData(get_struct(gcRequestArray, "4"),
      "GetHistoryReqType1NoSubSMS", "CustomerForDetails").
END.




PROCEDURE test_unproper_limit:
   fAddParams(1, 2, 1, 1001).
   request__list().
   assert_fault({&APPLICATION_ERROR}, 
      "Given limit 1001 is bigger than maximum limit 1000").
END.


PROCEDURE test_msreqstatistic_not_found:
   fAddParams(1, 1000, 1, 2).
   request__list().
   assert_success().
   gcReturnStruct = get_struct("", "").
   fCheckTotal(gcReturnStruct, "").
END.




