/**
   @input
      piMsSeq = MsSeq
*/

{test_xmlrpc_includes.i}
gcFixtures = "mobsub".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
gcParamStruct = "".

FUNCTION call_get_mobsub_balance RETURN LOGICAL:
    run_rpc_method("newton.get_mobsub_balance").
    assert_success().
    RETURN TRUE.
END FUNCTION.

FUNCTION add_params RETURN LOGICAL(
   INPUT piMsSeq AS INT):
   add_int(gcParamArray, ?, piMsSeq).

RETURN TRUE.
END FUNCTION.

PROCEDURE test_get_mobsub_balance:
   add_params(1).
   call_get_mobsub_balance(). 
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO.  
   bSucceeded = get_bool("","").
   assert(bSucceeded, "Get mobsub balance failed").
    
END.
