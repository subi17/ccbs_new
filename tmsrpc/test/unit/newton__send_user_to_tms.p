{test_xmlrpc_includes.i}

gcFixtures = "salesman,Reseller".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".

FUNCTION call_send_user_to_tms RETURN LOGICAL:
    run_rpc_method("newton.send_user_to_tms").
    assert_success().
    RETURN TRUE.
END FUNCTION.

FUNCTION add_params RETURN LOGICAL(
  INPUT pcUserName AS CHARACTER,
   INPUT pcFullName AS CHARACTER,
   INPUT pcReseller AS CHARACTER,
   INPUT plActive AS LOGICAL):
   
   add_string(gcParamArray, ?, pcUserName).
   add_string(gcParamArray, ?, pcFullName).
   add_string(gcParamArray, ?, pcReseller).
   add_boolean(gcParamArray, ?, plActive).

   RETURN TRUE.
END.


FUNCTION checkSalesman RETURN LOGICAL(INPUT pcUserName AS CHARACTER, 
                       INPUT pcSmName AS CHARACTER,
                       INPUT pcReseller AS CHARACTER,
                       INPUT plActive AS LOGICAL, INPUT pcEmail AS CHARACTER):
   FIND Salesman WHERE Brand = "1" AND Salesman = pcUserName NO-LOCK NO-ERROR.

   IF NOT AVAILABLE Salesman THEN
      assert(TRUE, 
         "Salesman was not created in the newton__send_user_to_tms RPC method").
   ELSE
   DO:
      assert_equal_char(Salesman.SmName, pcSmName).
      assert_equal_char(Salesman.Reseller, pcReseller).
      assert(plActive eq Salesman.Active, "Active information was not correct").
      assert_equal_char(Salesman.Email, pcEmail).
   END.

   RETURN TRUE.
END.


FUNCTION checkReseller RETURN LOGICAL (INPUT pcReseller AS CHARACTER, 
   INPUT pcEmail AS CHARACTER):
   FIND Reseller WHERE Brand = "1" AND Reseller = pcReseller NO-LOCK NO-ERROR.
   if NOT AVAILABLE Reseller THEN
      assert(TRUE, 
         "Reseller was not created" + 
         " in the newton__send_user_to_tms RPC method").
   ELSE
      assert_equal_char(Reseller.Email , pcEmail).
END.


PROCEDURE test_send_new_salesman_new_reseller:
   add_params("SalesmanNew", "SalesmanName", "NewReseller", TRUE).
   call_send_user_to_tms().
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO. 
   bSucceeded = get_bool("","").
   assert(bSucceeded, "newton__send_user_to_tms RPC method failed").
   checkSalesman("SalesmanNew", "SalesmanName", "NewReseller", TRUE, "").
   checkReseller("Reseller","").
END.


PROCEDURE test_send_new_salesman_existing_reseller:
   add_params("SalesmanNew", "SalesmanName", "ExistReseller", TRUE).
   call_send_user_to_tms().
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO. 
   bSucceeded = get_bool("","").
   assert(bSucceeded, "newton__send_user_to_tms RPC method failed").
   checkSalesman("SalesmanNew", "SalesmanName", "ExistReseller", TRUE, "").
   checkReseller("Reseller","address@google.com").
END.


PROCEDURE test_send_existing_salesman_new_reseller:
   add_params("ExistSalesman", "SalesmanName", "NewReseller", TRUE).
   call_send_user_to_tms().
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO. 
   bSucceeded = get_bool("","").
   assert(bSucceeded, "newton__send_user_to_tms RPC method failed").
   checkSalesman("ExistSalesman", "SalesmanName", "NewReseller", TRUE, 
         "salesman@google.com").
   checkReseller("Reseller","").
END.

PROCEDURE test_send_existing_salesman_existing_reseller:
   add_params("ExistSalesman", "SalesmanName", "ExistReseller", TRUE).
   call_send_user_to_tms().
   DEFINE VARIABLE bSucceeded AS LOGICAL NO-UNDO. 
   bSucceeded = get_bool("","").
   assert(bSucceeded, "newton__send_user_to_tms RPC method failed").
   checkSalesman("ExistSalesman", "SalesmanName", "ExistReseller", TRUE, 
         "salesman@google.com").
   checkReseller("Reseller","address@google.com").
END.
