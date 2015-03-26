/**
   @input  
      pcCli = CLI      

*/

{test_xmlrpc_includes.i}
gcFixtures = "mobsub,order".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO. 
gcParamStruct = "".

FUNCTION call_check_mnp_number RETURN LOGICAL:
    run_rpc_method("newton.check_mnp_number").
    assert_success().
    RETURN TRUE.
END FUNCTION.

FUNCTION call_check_mnp_number_exist RETURN LOGICAL:
    run_rpc_method("newton.check_mnp_number").
    assert_fault(-32500,"Subscription exists for this number").
    RETURN TRUE.
END FUNCTION.


FUNCTION add_params RETURN LOGICAL(
   INPUT pcCli AS CHAR):
   add_string(gcParamArray, ?, pcCli).


RETURN TRUE.
END FUNCTION.
/**
   MNP Check OK, cli does not exist in db
*/
PROCEDURE test_check_OK_mnp:
   add_params("123456").
   call_check_mnp_number(). 
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO.  
   bSucceeded = get_bool("","").
   assert(bSucceeded, "MNP Number check OK").
    
END.

/**
  MNP Check will fail, subscription for this cli exists in db
*/
PROCEDURE test_check_FAIL_mnp:
   add_params("04567890123").
   call_check_mnp_number_exist().
   DEFINE VARIABLE   bSucceeded AS LOGICAL NO-UNDO.   
   bSucceeded = get_bool("",""). 
   assert(bSucceeded, "MNP Number check FAILED").

END.
