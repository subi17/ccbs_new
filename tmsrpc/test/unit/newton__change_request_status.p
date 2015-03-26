/**
  @input
      piMsRequest = MsRequest id
      piNewStatus = New status for MsRequest
      pcUserCode  = katun

*/

{test_xmlrpc_includes.i}
gcFixtures = "msrequest,MsReqStatFunc,MsReqFuncItem".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
gcParamStruct = "".

FUNCTION add_params RETURN LOGICAL
   (INPUT piMsRequest AS INT,
    INPUT piNewStatus AS INT,
    INPUT pcUserCode  AS CHAR):
   add_int(gcParamArray, "", piMsRequest).
   add_int(gcParamArray, "", piNewStatus).
   add_string(gcParamArray, "", pcUserCode).
  
RETURN TRUE.
END FUNCTION.

PROCEDURE test_change_request_status_ok:
add_params(3236,2,"timok").
run_rpc_method("newton.change_request_status").
assert_success().
assert(get_bool("", "0"), "return value was not TRUE as expected").
END.


PROCEDURE test_change_request_status_failure:
   add_params(3236,3,"timok"). /*  (5) -->  (3) */
   run_rpc_method("newton.change_request_status"). 
   assert_fault(-32500, "Status change from 5 to 3 is not possible with msrequest type 4.").
END.

