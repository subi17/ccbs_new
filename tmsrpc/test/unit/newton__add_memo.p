{test_xmlrpc_includes.i}

/* gcFixtures = "memo". */

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".

FUNCTION call_add_memo RETURN LOGICAL:
    run_rpc_method("newton.add_memo").
    RETURN TRUE.
END FUNCTION.

FUNCTION add_params RETURN LOGICAL(
   INPUT pcReference AS CHARACTER,
   INPUT pcType AS CHARACTER,
   INPUT piCustNum AS INTEGER,
   INPUT pcCreator AS CHARACTER,
   INPUT pcTitle AS CHARACTER,
   INPUT pcContent AS CHARACTER,
   INPUT piPriority AS INT):
   
   add_string(gcParamArray, ?, pcReference).
   add_string(gcParamArray, ?, pcType).
   add_int(gcParamArray, ?, piCustNum).
   add_string(gcParamArray, ?, pcCreator).
   add_string(gcParamArray, ?, pcTitle).
   add_string(gcParamArray, ?, pcContent).
   add_int(gcParamArray, ?, piPriority).

   RETURN TRUE.
END.
   

PROCEDURE test_add_service_memo:
   add_params("Ref", "service", 1, "harrim", "New title", "Memo content1", 0).
   call_add_memo().
   assert_success(). 
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO. 
   bSucceeded = get_bool("","").
   assert(bSucceeded, "Create memo failed").
   FIND memo NO-LOCK NO-ERROR.
   assert((AVAILABLE Memo), "Memo record not found").
END.

PROCEDURE test_check_empty_username:
   add_params("Ref", "service", 1, "", "New title", "Memo content1", 0).
   call_add_memo().
   assert_fault({&APPLICATION_ERROR}, "username is empty").
END.
