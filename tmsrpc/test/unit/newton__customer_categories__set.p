{test_xmlrpc_includes.i}
{unit/checkutils.i}

gcFixtures = "CustCat". 

DEFINE VARIABLE gcParamStruct AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSetLimit AS INTEGER NO-UNDO. 


FUNCTION fCallCustomerCategorySet RETURN LOGICAL:
   run_rpc_method("newton.customer_categories__set").
   RETURN TRUE.
END.

FUNCTION fCheckSetValues RETURN LOGICAL 
    (INPUT pcCat AS CHARACTER, INPUT piExpectedLimit AS INTEGER):

    FIND CustCat WHERE CustCat.Brand = "1" AND 
                       CustCat.Category = pcCat NO-LOCK NO-ERROR.
    IF AVAIL CustCat THEN
       assert_equal_int(CustCat.MobSubLimit, piExpectedLimit).
    ELSE
       assert(FALSE, "Expected CustCat did not exist").
    RETURN TRUE.
END.


PROCEDURE test_set_customercategory_correct:
   add_string("", ?, "Cat1").
   gcParamStruct = add_struct("", "").
   add_int(gcParamStruct, "limit", 200).
   add_string(gcParamStruct, "username", "user").
   fCallCustomerCategorySet().
   assert_success().
   assert(get_struct("", "") NE ?, "Expected struct was not returned").
   fCheckSetValues("Cat1", 200).
END.

PROCEDURE test_not_found_category:
   add_string("", ?, "CatNotFound").
   gcParamStruct = add_struct("", "").
   add_int(gcParamStruct, "limit", 200).
   add_string(gcParamStruct, "username", "user").
   fCallCustomerCategorySet().
   assert_fault({&APPLICATION_ERROR}, "Unknown category CatNotFound").
   /* original value still valid */
   fCheckSetValues("Cat1", 200).
END.


PROCEDURE test_too_low_limit:
   add_string("", ?, "Cat1").
   gcParamStruct = add_struct("", "").
   add_int(gcParamStruct, "limit", -1).
   add_string(gcParamStruct, "username", "user").
   fCallCustomerCategorySet().
   assert_fault({&APPLICATION_ERROR}, "Limit -1 is not valid").
   /* original value still valid */
   fCheckSetValues("Cat1", 200).
END.


PROCEDURE test_too_big_limit:
   add_string("", ?, "Cat1").
   gcParamStruct = add_struct("", "").
   add_int(gcParamStruct, "limit", 1000).
   add_string(gcParamStruct, "username", "user").
   fCallCustomerCategorySet().
   assert_fault({&APPLICATION_ERROR}, "Limit 1000 is not valid").
   /* original value still valid */
   fCheckSetValues("Cat1", 200).
END.

