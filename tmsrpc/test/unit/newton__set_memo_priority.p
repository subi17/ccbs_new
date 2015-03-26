{test_xmlrpc_includes.i}

/**
 * This is the test of corresponding newton__add_termination_request 
 * XML RPC method.
 *
 */

gcFixtures = "memo".

DEFINE VARIABLE piMemoSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE piPriority AS INTEGER NO-UNDO. 

FUNCTION fadd_parameters_and_run RETURNS LOGICAL:
   add_int("", "", piMemoSeq).
   add_int("", "", piPriority).
   run_rpc_method("newton.set_memo_priority").
END.

PROCEDURE test_unknown_memo:
   ASSIGN
      piMemoSeq   = 999 
      piPriority  = 1.
   fadd_parameters_and_run().    
   assert_fault({&APPLICATION_ERROR}, "Memo not found!").
END PROCEDURE.

PROCEDURE test_success:
   ASSIGN
      piMemoSeq   = 1 
      piPriority  = 0.
   fadd_parameters_and_run().    
   assert_success().
   assert(get_bool("","") EQ TRUE, "Memo priority change was not succeeded").
END PROCEDURE.

PROCEDURE test_success2:
   ASSIGN
      piMemoSeq   = 1 
      piPriority  = 1.
   fadd_parameters_and_run().    
   assert_success().
   assert(get_bool("","") EQ TRUE, "Memo priority change was not succeeded").
END PROCEDURE.
