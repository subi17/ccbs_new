/**
  @input 
      piReference = Msseq
      pcMemo = katun
      pcReqType = "service" 
      pcServiceName ="Source1"
      plConfirm = YES
*/


{test_xmlrpc_includes.i}
gcFixtures = "msrequest".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
gcParamStruct = "".

FUNCTION call_cancel_request RETURN LOGICAL:
    run_rpc_method("newton.cancel_request").
    assert_success().
    RETURN TRUE.
END FUNCTION.

FUNCTION call_cancel_request_no_service RETURN LOGICAL:
    run_rpc_method("newton.cancel_request").
    assert_fault(0,"").
    RETURN TRUE.
END FUNCTION.


FUNCTION add_params RETURN LOGICAL(
   INPUT piReference AS INT,
   INPUT pcMemo AS CHAR,
   INPUT pcReqType AS CHAR,
   INPUT pcServiceName AS CHAR,
   INPUT plConfirm AS LOG):

   add_int(gcParamArray, ?, piReference).
   add_string(gcParamArray, ?, pcMemo).
   add_string(gcParamArray, ?, pcReqType).
   add_string(gcParamArray, ?, pcServiceName).
   add_boolean(gcParamArray, ?, plConfirm).
 

RETURN TRUE.
END FUNCTION.

/**
   Cancel request ok
*/
PROCEDURE test_cancel_request_ok:
   add_params(1192,"timok","subscription_type","Sms_pack1",YES).
   call_cancel_request().
   DEFINE VARIABLE   bSucceeded AS LOGICAL NO-UNDO.   
   bSucceeded = get_bool("","").
   assert(bSucceeded," Request cancellation OK").
END.
 
/**
   Cannot cancel request

*/
PROCEDURE test_cancel_request_no_service:
   add_params(1199,"timok","subscription_type","service",YES).
   call_cancel_request_no_service(). 
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO.  
   bSucceeded = get_bool("","").
   assert(bSucceeded, "Request cancellation failed").
    
END.
