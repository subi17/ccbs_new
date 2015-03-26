/**
   @input
     key =           (chr)custnum
     type =          (chr)mobsub/customer/invoice/service
     last_in_cache = (int)last memo sequence in cache
*/
{test_xmlrpc_includes.i}

gcFixtures = "memo".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".

FUNCTION call_get_memos RETURN LOGICAL:
    run_rpc_method("newton.get_memos").
    assert_success().
    RETURN TRUE.
END FUNCTION.

FUNCTION add_params RETURN LOGICAL(
   INPUT piKey AS INTEGER,
   INPUT pcType AS CHARACTER,
   INPUT piLastInCache AS INTEGER):
   add_int(gcParamArray, ?, piKey).
   add_string(gcParamArray, ?, pcType).
   add_int(gcParamArray, ?, 0).
   add_int(gcParamArray, ?, piLastInCache).
   RETURN TRUE.
END.
   
/**
   Memo found from Memo
*/

PROCEDURE test_get_memos_found:
   add_params(1234,"service",1).
   call_get_memos().
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO. 
   bSucceeded = get_bool("","").
   assert(bSucceeded, "memo text found").
END.

/**
   No memo found, should return error
*/
PROCEDURE test_get_memos_notfound:
   add_params(3214,"hiphei",33).
   call_get_memos().
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO.  
   bSucceeded = get_bool("","").
   assert(bSucceeded, "memotext not found").

END.
