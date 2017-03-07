
{test_xmlrpc_includes.i}
{unit/checkutils.i}
{Func/timestamp.i}


gcFixtures = "mobsub,msrequest".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".

DEFINE VARIABLE gcReturnArray  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE piMsSeq AS INTEGER NO-UNDO. 


FUNCTION call_get_history RETURN LOGICAL:
    run_rpc_method("newton.get_history").
    RETURN TRUE.
END FUNCTION.



FUNCTION fCheckAddressRequest RETURN LOGICAL (
   INPUT pcMsRequestFixture AS CHARACTER):
   fetch_fixture(pcMsRequestFixture, BUFFER MsRequest:HANDLE).
   gcReturnStruct = get_struct(gcReturnArray, "0").
   
   checkChar("change_type", "address", get_string(gcReturnStruct, 
      "change_type")).
   checkChar("new_value"  , 
               MsRequest.ReqCParam1 + ";" +
               MsRequest.ReqCParam2 + ";" +
               MsRequest.ReqCParam3 + ";" +
               MsRequest.ReqCParam4 + ";" +
               STRING(MsRequest.ReqiParam1) + ";" +
               STRING(MsRequest.ReqiParam2) + ";" +
               STRING(MsRequest.ReqiParam3) + ";" +
               STRING(MsRequest.ReqiParam4), 
               get_string(gcReturnStruct, "new_value")).
    checkInt("MsRequest.MsRequest", MsRequest.MsRequest, 
       get_int(gcReturnStruct, "request_id")).
    checkInt("MsRequest.ReqStatus", MsRequest.ReqStatus,
       get_int(gcReturnStruct, "status")).
    checkChar("MsRequest.UserCode", MsRequest.UserCode, 
       get_string(gcReturnStruct, "actor")).
    IF MsRequest.DoneStamp NE ? AND MsRequest.DoneStamp NE 0 THEN
       checkDecimal("MsRequest.DoneStamp", MsRequest.DoneStamp, 
          get_timestamp(gcReturnStruct, "change_time")).
    ELSE
       checkDecimal("MsRequest.ActStamp", MsRequest.ActStamp, 
          get_timestamp(gcReturnStruct, "change_time")).

    RETURN TRUE.
END.


FUNCTION fCheckOwnerRequest RETURN LOGICAL ( 
   INPUT pcMsRequestFixture AS CHARACTER, INPUT iOfArray AS INTEGER ):

   fetch_fixture(pcMsRequestFixture, BUFFER MsRequest:HANDLE).
   gcReturnStruct = get_struct(gcReturnArray, STRING(iOfArray)).

   checkChar("change_type", "contract_letter", get_string(gcReturnStruct, 
      "change_type")).
   checkChar("new_value", "pc_lettersent", get_string(gcReturnStruct, 
      "new_value")).

   checkInt("MsRequest.MsRequest", MsRequest.MsRequest, 
      get_int(gcReturnStruct, "request_id")).
   checkInt("MsRequest.ReqStatus", MsRequest.ReqStatus,
      get_int(gcReturnStruct, "status")).
   checkChar("MsRequest.UserCode", MsRequest.UserCode, 
      get_string(gcReturnStruct, "actor")).

   IF MsRequest.DoneStamp NE ? AND MsRequest.DoneStamp NE 0 THEN
      checkDecimal("MsRequest.DoneStamp", MsRequest.DoneStamp, 
         get_timestamp(gcReturnStruct, "change_time")).
   ELSE
      checkDecimal("MsRequest.ActStamp", MsRequest.ActStamp, 
         get_timestamp(gcReturnStruct, "change_time")).

   RETURN TRUE.
END.


FUNCTION fCheckBasicLoopProps RETURN LOGICAL:
   DEFINE VARIABLE iReqId AS INTEGER NO-UNDO. 
   iReqId = get_int(gcReturnStruct, "request_id").

   IF MsRequest.ReqType NE 1 THEN
      checkChar("change_type", ENTRY(MsRequest.ReqType + 1,
                               "subscription_type,sms_pack,?,user," +
                                "payer,?,?,?,?,?,owner,?,?,?,?,icc"),
                get_string(gcReturnStruct, "change_type")).

    IF iReqId eq 3605 THEN
       assert(TRUE, "Subrequest was returned as a root request.").
   
    checkInt("MsRequest.MsRequest", MsRequest.MsRequest, iReqId).
    checkInt("MsRequest.ReqStatus", MsRequest.ReqStatus, 
       get_int(gcReturnStruct, "status")).
    checkChar("MsRequest.UserCode", MsRequest.UserCode,
       get_string(gcReturnStruct, "actor")).

   IF MsRequest.DoneStamp NE ? AND MsRequest.DoneStamp NE 0 THEN
      checkDecimal("MsRequest.DoneStamp", MsRequest.DoneStamp, 
         get_timestamp(gcReturnStruct, "change_time")).
   ELSE
      checkDecimal("MsRequest.ActStamp", MsRequest.ActStamp, 
         get_timestamp(gcReturnStruct, "change_time")).
   
   RETURN TRUE.
END.

FUNCTION fCheckSubscriptionTypeChangeHistoryProps RETURN LOGICAL:
   checkChar("MsRequest.ReqCParam1", MsRequest.ReqCParam1, 
       get_string(gcReturnStruct, "old_value")).
   checkChar("MsRequest.ReqCParam2", MsRequest.ReqCParam2, 
       get_string(gcReturnStruct, "new_value")).
   RETURN TRUE.
END.


DEFINE BUFFER xRequest FOR MsRequest.
FUNCTION fCheckOldValueWithReqId RETURN LOGICAL (INPUT piMsRequest AS INTEGER):
   FIND xRequest WHERE xRequest.MsRequest = piMsRequest NO-LOCK NO-ERROR.
   IF AVAIL xRequest THEN
      checkChar("MsRequest.ReqCParam1", xRequest.ReqCParam1, 
         get_string(gcReturnStruct, "old_value")). 
   RETURN TRUE.
END.


FUNCTION fCheckSMSPackHistoryProps RETURN LOGICAL:
   IF LOOKUP(MsRequest.ReqCParam1,"SMS") > 0 THEN
   DO:
      checkChar("change_type", "sms_pack", 
         get_string(gcReturnStruct, "change_type")).
      IF MsRequest.ReqIParam1 EQ 1 THEN
      DO:
          checkChar("MsRequest.ReqCParam1", MsRequest.ReqCParam1, 
             get_string(gcReturnStruct, "new_value")).
          IF MsRequest.ReqCParam4 NE "" THEN
          DO:
             fCheckOldValueWithReqId(INT(ENTRY(1, MsRequest.ReqCParam4))).
          END.
      END.
      ELSE
      DO:
         checkChar("MsRequest.ReqCParam1", MsRequest.ReqCParam1, 
            get_string(gcReturnStruct, "old_value")).
         checkChar("new_value", "off", get_string(gcReturnStruct, 
            "new_value")).
      END.

   END.
   ELSE
   DO:
      checkChar("change_type", "vms", get_string(gcReturnStruct, 
         "change_type")).
      DEFINE VARIABLE cExpNewValue AS CHARACTER NO-UNDO.

      IF MsRequest.ReqIParam1 EQ 1 THEN 
         cExpNewValue = "on".
      ELSE IF MsRequest.ReqIParam1 EQ 0 THEN 
         cExpNewValue = "off".
      ELSE cExpNewValue = STRING(MsRequest.ReqIParam1).
      checkChar("new_value", cExpNewValue, get_string(gcReturnStruct, 
         "new_value")).
   END.

   RETURN TRUE.
END.


FUNCTION fCheckICCChangeHistoryProps RETURN LOGICAL:
  FIND SIM WHERE SIM.MsSeq = piMsSeq NO-LOCK NO-ERROR.

  checkChar("old_value",(IF AVAIL SIM THEN SIM.ICC ELSE ""), 
     get_string(gcReturnStruct, "old_value")).
  checkChar("MsRequest.ReqCParam2", MsRequest.ReqCParam2, 
     get_string(gcReturnStruct, "new_value")).
  RETURN TRUE.
END.


FUNCTION fCheckMsRequestLoop RETURN LOGICAL 
   (INPUT pcMsRequestFixtureList AS CHARACTER, INPUT piBeginIndex AS INTEGER):

   DEFINE VARIABLE iFixtureCount AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iFix AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cFixture AS CHARACTER NO-UNDO. 
   iFixtureCount = NUM-ENTRIES(pcMsRequestFixtureList).
   REPEAT iFix = 1 TO iFixtureCount:
      gcReturnStruct = get_struct(gcReturnArray, 
         STRING(piBeginIndex + iFix - 1)).
      cFixture = ENTRY(iFix, pcMsRequestFixtureList).
      fetch_fixture(cFixture, BUFFER MsRequest:HANDLE).
      fCheckBasicLoopProps().
      CASE MsRequest.ReqType:
         WHEN 0 THEN fCheckSubscriptionTypeChangeHistoryProps().
         WHEN 1 THEN fCheckSMSPackHistoryProps().
         WHEN 15 THEN fCheckICCChangeHistoryProps().
         OTHERWISE
             checkChar("new_value",  
                 ENTRY(2, MsRequest.ReqCParam1, ";") + " " +
                 ENTRY(1, MsRequest.ReqCParam1, ";"),
                 get_string(gcReturnStruct, "new_value")).
      END.
   END.
   RETURN TRUE.
END.


PROCEDURE test_multiple_requests:
   piMsSeq = 40.
   add_int("", "", piMsSeq).
   add_int("", "", 3500).
   call_get_history().
   assert_success().
   gcReturnArray = get_array("", "").
   fCheckAddressRequest("GetHistoryAddressChange").
   fCheckOwnerRequest("GetHistoryOwnerRequest", 1).
   /*fCheckMsRequestLoop(
      "GetHistoryReqType0,GetHistoryReqType1NoSubSMS," + 
      "GetHistoryReqType1SubSMS," + 
      "GetHistoryVms1,GetHistoryVms2,GetHistoryVms3," + 
      "GetHistoryICCChange,GetHistoryOtherReqType", 2).*/
END.

