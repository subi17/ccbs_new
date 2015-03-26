/**
 * This is the test set for the corresponding newton__check_referee
 * XML RPC method.
 *
 * The fixture usage of this test set:
 *
 * - fixtures fetched: 
 *
 * - The RPC method is assumed to fetch in addition following records:
 *
 *
 * - restrictions for building other fixtures: 
 */

{test_xmlrpc_includes.i}

gcFixtures = "customer,mobsub,order".

FUNCTION check_referee RETURN LOGICAL:
   run_rpc_method("newton.check_referee").
END.


FUNCTION fAddParams RETURN LOGICAL 
   (INPUT pcCLI        AS CHARACTER,
    INPUT pcCustIdType AS CHARACTER,
    INPUT pcOrgId      AS CHARACTER):

   add_string("", "", pcCLI).
   add_string("", "", pcCustIdType).
   add_string("", "", pcOrgId).
   RETURN TRUE.
END.


PROCEDURE test_total_match:
END.


PROCEDURE test_not_found_referee_subscription:
   fAddParams("999999999", "CustIdReferer", "OrgIdReferer").
   check_referee().
   assert_fault({&APPLICATION_ERROR}, 
      "Subscription 999999999 was not found").
END.


PROCEDURE test_not_found_customer_for_referee:
   fAddParams("622444223", "CustIdReferer", "OrgIdReferer").
   check_referee().
   assert_success().
   assert(get_bool("", "") eq TRUE, 
      "The reference was incorrect, although " + 
      "expected to be correct with unknown " + 
      "customer for referee customer").
END.


PROCEDURE test_referee_customer_with_custidtype_CIF:
   fAddParams("622444222", "", "").
   check_referee().
   assert_success().
   assert(get_bool("", "") eq TRUE, 
      "Corporate customer not allowed").
END.


PROCEDURE test_custidtype_not_same_as_with_referee:
   fAddParams("622444222", "ERROR", "OrgIdReferer").
   check_referee().
   assert_success().
   assert(get_bool("", "") eq TRUE, 
      "The reference was incorrect, although " + 
      "expected to be correct with unknown " + 
      "customer for referee customer").
END.


PROCEDURE test_orgid_not_same_as_with_referee:
   fAddParams("622444222", "CustIdReferer", "ERROR").
   check_referee().
   assert_success().
   assert(get_bool("", "") eq TRUE, 
      "The reference was incorrect, although " + 
      "expected to be correct with unknown " + 
      "customer for referee customer").

END.


PROCEDURE test_found_customer_for_referee_and_indentified_as_same:
   fAddParams("622444222", "CustIdReferer", "OrgIdReferer").
   check_referee().
   assert_success().
   assert(get_bool("", "") eq false, 
      "The reference was correct, although " +
      "expected to be incorrect with " +
      "same custnum in customer and subscription").
END.







