{test_xmlrpc_includes.i}
{unit/checkutils.i}
{timestamp.i}


gcFixtures = "MsReqStatistic".

DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcStructReqStates AS CHARACTER NO-UNDO. 

FUNCTION call_requestcount RETURN LOGICAL:
    run_rpc_method("newton.requests__counts").
    RETURN TRUE.
END FUNCTION.




PROCEDURE test_getcounts:
   call_requestcount().
   assert_success().
   gcReturnStruct = get_struct("", "").
   FOR EACH MsReqStatistic NO-LOCK USE-INDEX ReqType 
      BREAK BY MsReqStatistic.ReqType BY MsReqStatistic.ReqStatus:
      IF FIRST-OF(MsReqStatisTic.ReqType) THEN
         gcStructReqStates = get_struct(gcReturnStruct, 
            STRING(MsReqStatistic.ReqType)).
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
      iCount = get_int(gcStructReqStates, STRING(MsReqStatistic.ReqStatus)).
      checkInt("MsReqStatistic.ReqStatusCount", 
         MsReqStatistic.ReqStatusCount, get_int(gcStructReqStates, 
            STRING(MsReqStatistic.ReqStatus))).
   END.

END.
