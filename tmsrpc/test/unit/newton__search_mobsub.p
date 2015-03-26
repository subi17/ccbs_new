/**
 * This is the test set for the corresponding newton__search_mobsub
 * XML RPC method.
 *
 * The fixture usage of this test set:
 *
 * - fixtures fetched: 
 *   mobsub:
 *      OrgID_multiple1
 *      OrgID_multiple2
 *      OrgID_multiple3
 *      active_subscription,
 *      search_by_CLI
 *      Search_by_OrgID
 *    
 *   customer:
 *      Customer_by_orgId_multiple_mobsub,
 *      NotPreActivatedMobSubCustomer,
 *      NotPreActivatedMobSubCustomer2,
 *      Customer_by_OrgId,
 *
 * - The RPC method is assumed to fetch in addition following records:
 *
 *
 * - restrictions for building other fixtures: 
 */

{/apps/xfera/tmsrpc/test/includes/dbutils.i} 

{test_xmlrpc_includes.i}

/* Fixture tables used */
gcFixtures = "customer,mobsub".

/* Identifier for the parameter array */
DEF VAR gcParamArray AS CHAR NO-UNDO.

DEF VAR gcArray AS CHAR NO-UNDO.
DEF VAR gcRetStruct AS CHARACTER NO-UNDO. 
DEF VAR gcRetSubscriptions AS CHARACTER NO-UNDO. 
DEF VAR gcRetSubStruct AS CHARACTER NO-UNDO. 

/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_search_mobsub RETURN LOGICAL:
    run_rpc_method("newton.search_mobsub").
    assert_success().
    RETURN TRUE.
END FUNCTION.


FUNCTION fSetLimitAndOffset RETURN LOGICAL
   (INPUT piLimit AS INTEGER, INPUT piOffSet AS INTEGER):

    add_int("", "", piLimit).
    add_int("", "", piOffset).
END.


/**
 * Checks one integer value in the given struct and gives special messages
 * if the expected value or response value is unknown (?).
 * If the expected value is different from the response value or one of them 
 * is ?, then test is asserted.
 *
 * @param pcStructName  the name of the struct from which 
 *                      the value is compared.
 * @param pcFieldName   the name of the integer field inside 
 *                      the struct that is compared
 * @param iExpectedValue  the value to which the struct integer value
 *                        is compared. 
 */
FUNCTION compare_struct_int_value RETURNS LOGICAL 
   (INPUT pcStructName AS CHARACTER, INPUT pcFieldName AS CHARACTER, 
    INPUT iExpectedValue AS INTEGER):

   DEFINE VARIABLE iRetValue AS INTEGER NO-UNDO.
   assert(NOT(iExpectedValue = ?),"Fieldname " + pcFieldName + 
      " expected integer value is ?").

   iRetValue = get_int(pcStructName, pcFieldName).
   assert(NOT(iRetValue = ?), "Fieldname " + pcFieldName + 
      " returned integer value is ?").

   assert_equal_int(iRetValue, iExpectedValue). 
END.
  
/**
 * Checks one character value in the given struct and gives special messages
 * if the expected value or response value is unknown (?).
 * If the expected value is different from the response value or
 * one of them is ?, then test is asserted.
 *
 * @param pcStructName  the name of the struct from which 
 *                      the value is compared.
 * @param pcFieldName   the name of the character field inside 
 *                      the struct that is compared
 * @param cExpectedValue  the value to which the struct character value
 *                        is compared. 
 */
FUNCTION compare_struct_char_value RETURNS LOGICAL 
   (INPUT pcStructName AS CHARACTER, INPUT pcFieldName AS CHARACTER, 
    INPUT cExpectedValue AS CHARACTER):

   DEFINE VARIABLE cRetValue AS CHARACTER NO-UNDO.

   assert(NOT(cExpectedValue = ?) ,"Fieldname " + pcFieldName + 
      " expected character value is ?").
   
   cRetValue = get_string(pcStructName, pcFieldName).
   assert(NOT(cRetValue = ?), "Fieldname " + pcFieldName + 
      " returned character value is ?").

   assert_equal_char(cRetValue, cExpectedValue). 
END.

/**
 * Checks subscription count is correct and gets first subscription into 
 * gcRetSubStruct. Checks the name of the struct is not empty. The subscription
 * array must be got before hand to gcRetSubscriptions.
 *
 * @param piExpectedCount  the number of subscriptions expected.
 * @return TRUE if succeeded.
 */
FUNCTION check_subscription_count_and_get_first_struct RETURN LOGICAL (
   INPUT piExpectedCount AS INTEGER):
   DEFINE VARIABLE iNumSubscriptions AS INTEGER NO-UNDO. 
   iNumSubscriptions = get_paramcount( gcRetSubscriptions ).
   assert( iNumSubscriptions = piExpectedCount, "supbscription count was " +
     STRING(iNumSubscriptions) + " when expected " + STRING(piExpectedCount) + "." ).

   gcRetSubStruct = get_struct(gcRetSubscriptions, "0").

   gcRetSubStruct = get_struct(gcRetSubStruct, "json").

   /*assert_response(gcRetSubStruct,"int,string,string,int").  -------- added ----------- */

   assert(gcRetSubStruct <> "", "First Subscription struct name was empty").
   RETURN TRUE.
END.

/**
 * Gets next subscription struct to gcRetSubStruct. The subcription array 
 * must be got before hand to gcRetSubscriptions.
 * 
 * @param cIndex  the index in the subscription array to be got next.
 * @return TRUE if succeeded.
 */
FUNCTION get_next_subscription_struct RETURN LOGICAL 
   (INPUT cIndex AS CHARACTER):

   gcRetSubStruct = get_struct(gcRetSubscriptions, cIndex).

   gcRetSubStruct = get_struct(gcRetSubStruct, "json").

   assert(gcRetSubStruct <> "", "Subscription struct " + cIndex + 
      " name was empty").
   RETURN TRUE.
END.


/**
 * Fecthes subscription from the given Mobsub fixture name and compares 
 * the values got the gcRetSubscrupt subscription struct got from the response
 * with its values. Correct Customer fixture must be fetched before hand to 
 * Customer buffer. If values don't match, assert is launched.
 *
 * @param pcMobSubFixture  the name of the MobSub fixture to be fetched.
 * @return TRUE if succeeded.
 */
FUNCTION check_single_subscription RETURN LOGICAL 
   (INPUT pcMobSubFixture AS CHARACTER):

   fetch_fixture(pcMobSubFixture, BUFFER MobSub:HANDLE).

   assert_response(gcRetSubStruct, "int,string,string,int"). 

   /*compare_struct_char_value(gcRetStruct, "message",
     "MSIDSN listing is disabled for this customer").*/
   
   compare_struct_int_value(gcRetSubStruct, "seq", MobSub.MsSeq).
   compare_struct_char_value(gcRetSubStruct, "description", MobSub.cli).
   compare_struct_char_value(gcRetSubStruct, "subscription_type_id", 
                             MobSub.clitype).

   compare_struct_int_value(gcRetSubStruct, "status", MobSub.MsStatus).
   /*compare_struct_char_value(gcRetSubStruct, "firstname", Customer.Firstname).
   compare_struct_char_value(gcRetSubStruct, "lastname", Customer.CustName).*/
   RETURN TRUE.
END.


/**
 * Fetches single MobSub fixture ( the first and only one ), compares the count
 * of subscriptions is 1 and compares the values in the subscription struct 
 * to the given subscription fixture. The mobsub is expected to be not 
 * preactivated. Assert is set, if values not match.
 *
 * @param pcMobSubFixture  the mobsub fixture with which the response values
 *                         are compared with
 * @return TRUE if succeeded.
 */
FUNCTION check_single_not_preactivated_subscription_with_count RETURN LOGICAL
   (INPUT pcMobSubFixture AS CHARACTER):

   check_subscription_count_and_get_first_struct(1).
   check_single_subscription(pcMobSubFixture).

   RETURN TRUE.
END.


/**
 * This function...
 * 
 * @return TRUE if succeeded
 */
FUNCTION check_preactivated_subscriptions RETURN LOGICAL:
   DEFINE VARIABLE iNumSubscriptions AS INTEGER NO-UNDO. 
   iNumSubscriptions = get_paramcount( gcRetSubscriptions ).
   assert( iNumSubscriptions = 1, "supbscription count was " +
     STRING(iNumSubscriptions) + " when expected 1." ).

   gcRetSubStruct = get_struct(gcRetSubscriptions, "0").

   gcRetSubStruct = get_struct(gcRetSubStruct, "json").

   assert_response(gcRetSubStruct,"int,string,string,int").

   assert(gcRetSubStruct <> "", "First Subscription struct name was empty").
   compare_struct_char_value(gcRetStruct, "message",
     "MSIDSN listing is disabled for this customer").

   compare_struct_int_value(gcRetSubStruct, "seq", MobSub.MsSeq).
   compare_struct_char_value(gcRetSubStruct, "description", MobSub.CLI).
   compare_struct_char_value(gcRetSubStruct, "subscription_type_id", 
                             MobSub.clitype).
   compare_struct_int_value(gcRetSubStruct, "status", MobSub.msstatus).
   /*compare_struct_char_value(gcRetSubStruct, "firstname", Customer.FirstName).
   compare_struct_char_value(gcRetSubStruct, "lastname", Customer.CustName).*/

   RETURN TRUE.
END.


/** 
 * This function checks the current Customer fixture values to the response 
 * values. The custnum and name values are checked.
 *
 * @param iCustNum  the expected custnum value.
 */
FUNCTION check_custnum_and_name RETURNS LOGICAL (INPUT iCustNum AS INTEGER):
   DEFINE VARIABLE iRetCustNum AS INTEGER NO-UNDO. 
   iRetCustNum = get_int(gcRetStruct, "custnum").
   assert(iCustNum <> ?, "CustNum = ?").

   assert_equal_int( iCustNum, iRetCustNum ).
   assert_equal_char( get_string(gcRetStruct,"name"),
                     SUBST("&1 &2 &3", Customer.FirstName, Customer.CustName, Customer.Surname2) ).

   RETURN TRUE.
END.

/**
 * This function calls the RPC method, fecthes the given customer and mobsub 
 * fixture, checks the customer with given custnum and check the given mobsub 
 * fixture with the one and only expected mobsub record in the response. 
 * 
 * @param pcCustFixture  the name of the Customer fixture with which
                         the response values are checked.
 * @param pcMobSubFixture  the name of the MobSub fixture with which
                           the response values are checked.
 * @param piCustNum      the custnum of the expected Customer.
 * @return TRUE if succeeds.
 */
FUNCTION search_single_mobsub_not_preactivated RETURN LOGICAL
(INPUT pcCustFixture AS CHARACTER, INPUT pcMobSubFixture AS CHARACTER,
 INPUT piCustNum AS INTEGER):

   call_search_mobsub().
   
   fetch_fixture(pcCustFixture, BUFFER Customer:HANDLE).
   
   gcRetStruct = get_struct("", "0").

   check_custnum_and_name(piCustNum).

   assert(gcRetStruct <> "", "Ret struct name empty").
   assert_response(gcRetStruct,"custnum,name,subscriptions,message,associated_cli,sub_count").

   gcRetSubscriptions = get_array( gcRetStruct, "subscriptions" ).
   assert(gcRetSubScriptions <> "", "Ret Subscription name empty").
/*   assert_response(gcRetSubscriptions, ""). */

   check_single_not_preactivated_subscription_with_count(pcMobSubFixture).

   DEFINE VARIABLE iSubCount AS INTEGER NO-UNDO. 
   iSubCount = get_int(gcRetStruct, "sub_count").
   assert_equal_int(iSubCount, 1). /* Single beginning from the first */

   RETURN TRUE.
END.

/**
 * This function calls the RPC method and checks the response array contains 
 * 3 mobsub records that must correspond with mobsub fixture records named
 * OrgId_multiple1, OrgId_multiple2, OrgId_multiple3. The customer must CustNum 
 * and name must correspond to Customer with CustNum 6. The Customer fixture
 * must be fetched before call to this function.
 *
 * Fixtures fetched:
 *     Customer: Customer_by_orgId_multiple_mobsub (  )
 *     MobSub  : OrgId_multiple1
 *               OrgId_multiple2
 *               OrgId_multiple3
 *
 * @return TRUE, if succeeds.
 */
FUNCTION check_multiple_mobsub1 RETURN LOGICAL:
   call_search_mobsub().
   
   fetch_fixture("Customer_by_orgId_multiple_mobsub", BUFFER Customer:HANDLE).
   
   gcRetStruct = get_struct("", "0").
   assert(gcRetStruct <> "", "Ret struct name empty").
   assert_response(gcRetStruct,"custnum,name,subscriptions,message,associated_cli,sub_count"). 

   check_custnum_and_name(6).

   gcRetSubscriptions = get_array( gcRetStruct, "subscriptions" ).
   assert(gcRetSubScriptions <> "", "Ret Subscription name empty").
/*   assert_response(gcRetSubscriptions, "json_struct"). */
   
   /*"custnum,name,description,subscriptions"). */

   check_subscription_count_and_get_first_struct(3).
   check_single_subscription("OrgID_multiple1").
   get_next_subscription_struct("1").
   check_single_subscription("OrgID_multiple2").
   get_next_subscription_struct("2").
   check_single_subscription("OrgID_multiple3").

   DEFINE VARIABLE iSubCount AS INTEGER NO-UNDO. 
   iSubCount = get_int(gcRetStruct, "sub_count").
   assert_equal_int(iSubCount, 3). /* Three beginning from the first */

END.


FUNCTION fCheckSingleIMSIMobsub RETURN LOGICAL:
   call_search_mobsub().
   fetch_fixture("Customer_for_freeairMobSub2", BUFFER Customer:HANDLE).

   gcRetStruct = get_struct("", "0").

   check_custnum_and_name(13).
   assert(gcRetStruct <> "", "Ret struct name empty").
   assert_response(gcRetStruct, "custnum,name,subscriptions,message,associated_cli,sub_count"). 

   gcRetSubscriptions = get_array( gcRetStruct, "subscriptions" ).
   assert(gcRetSubScriptions <> "", "Ret Subscription name empty"). /* -------------done --------------*/

   check_subscription_count_and_get_first_struct(1).
   check_single_subscription("yoigo_subscription").

   DEFINE VARIABLE iSubCount AS INTEGER NO-UNDO. 
   iSubCount = get_int(gcRetStruct, "sub_count").
   assert_equal_int(iSubCount, 1). /* Three beginning from the first */

END.


/**
 * This test searches one customer and mobsub record using
 * non zero integer parameter of the RPC method. The used 
 * Customer record is expected not to be preactivated.
 * The Customer is expected to have one corresponding
 * MobSub record using AgrCust field of mobsub table.
 *
 * Fixtures fetched:
 *    customer: NotPreActivatedMobSubCustomer
 *    mobsub:   active_subscription
 * RPC method fetches no other records.
 * 
 * Restrictions to build additional fixture records:
 *    mobsub: there should not be more customer records
 *            that have agrCust value 1.
 */
PROCEDURE 
  test_search_single_mobsub_customer_by_string_custnum_not_preactivated:
 
   delete_disturbing_records(BUFFER MobSub:HANDLE,
      "MobSub", "MsSeq", "1,2").

   add_int("", ?, 1). /* CustNum = 1 */
   fSetLimitAndOffset(5, 0).
   add_string("", "", "custnum"). /* Search Type = custnum */
   search_single_mobsub_not_preactivated(
     "NotPreActivatedMobSubCustomer", "active_subscription", 1).
END.

/**
 * This test searches one customer and mobsub record using
 * one string parameter that means CLI value of the mobsub
 * records to be found. The customer is then found with
 * the AgrCust field of the mobsub.
 * 
 * Fixtures fetched:
 *     customer: NotPreActivatedMobSubCustomer2
 *     mobsub:   search_by_CLI
 * RPC method fecthes no other records.
 */
PROCEDURE 
   test_search_single_mobsub_with_msisdn_cli_using_string_not_preactivated:

   add_string("", ?, "622323115").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "msisdn"). /* Search Type = msisdn */
   search_single_mobsub_not_preactivated(
      "NotPreActivatedMobSubCustomer2","search_by_CLI", 2).
END.

/**
 * This test does the same as the test  
 * test_search_single_mobsub_customer_by_string_custnum_not_preactivated
 * except the parameter is given as string parameter.
 * Same fixtures and restrictions apply.
 */
PROCEDURE test_search_single_mobsub_with_custnum_by_string:
   add_string("", ?, "1"). /* CustNum */
   fSetLimitAndOffset(5, 0).
   add_string("", "", "custnum"). /* Search Type = custnum */
   search_single_mobsub_not_preactivated(
      "NotPreActivatedMobSubCustomer","active_subscription", 1).
END.

/**
 * This test searches searches one customer with OrgId given
 * and mobsub having agrCust field same the found Customer.
 *
 * The found customer is expected to be the same as the
 * fixture Customer_by_OrgId and it is expected not to be
 * preactivated.
 *
 * Fixtures fetched:
 *    customer: Customer_by_OrgId
 *    mobsub:   Search_by_OrgId
 * RPC method uses no other fixtures.
 *
 * Restrictions to build additional fixture records:
 *    mobsub: there should not be more customer records
 *            that have agrCust value 5.
 */
PROCEDURE test_search_single_mobsub_with_orgid_not_preactivated:
   /* OrgID; is not convertible to integer; 
      does not begin with 6 and length is not 9 */
   add_string("", ?, "1361XX").   
   fSetLimitAndOffset(5, 0).
   add_string("", "", "person_id"). /* Search Type = person_id */
   search_single_mobsub_not_preactivated(
      "Customer_by_OrgId","Search_by_OrgID", 5).
END.

/**
 * This test searches multiple mobsubs with OrgId given. The given
 * string parameter for the test is given first character not 
 * beginning with 6 so that it should be recognized by the RPC method
 * as OrgId and not CLI value. Function check_multiple_mobsub is
 * called to check all the mobsub records 
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_search_multiple_mobsub_with_orgid_not_preactivated:
   /* OrgID; is convertible to integer; 
      does not begin with 6 and length is here 9 */
   add_string("", ?, "A37888888").  
   fSetLimitAndOffset(5, 0).
   add_string("", "", "person_id"). /* Search Type = person_id */

   check_multiple_mobsub1().
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */

PROCEDURE test_search_multiple_mobsub_with_string_custnum_not_preactivated:
   add_string("", ?, "6").  
   fSetLimitAndOffset(5, 0).
   add_string("", "", "custnum"). /* Search Type = custnum */

   check_multiple_mobsub1().
END.  

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_search_multiple_mobsub_with_CLI_not_preactivated:
   add_string("", ?, "632133123").  
   fSetLimitAndOffset(5, 0).
   add_string("", "", "msisdn"). /* Search Type = msisdn */
   check_multiple_mobsub1().
END.   


PROCEDURE test_search_single_mobsub_with_IMSI:
   add_string("", ?, "214040000161161").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "imsi"). /* Search Type = imsi */
   fCheckSingleIMSIMobsub().
END.



/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_search_preactivated_mobsub_by_CLI:
   add_string("", ?, "622200000").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "msisdn"). /* Search Type = msisdn */
   call_search_mobsub().

   fetch_fixture("Customer_PreActivated1", BUFFER Customer:HANDLE).
   fetch_fixture("Preactivated1", BUFFER MobSub:HANDLE).

   gcRetStruct = get_struct("", "0").

   check_custnum_and_name(7).

   assert(gcRetStruct <> "", "Ret struct name empty").
   assert_response(gcRetStruct, "custnum,name,subscriptions,message,associated_cli,sub_count").  

   gcRetSubscriptions = get_array( gcRetStruct, "subscriptions" ).
   assert(gcRetSubScriptions <> "", "Ret Subscription name empty").

   check_preactivated_subscriptions().

   DEFINE VARIABLE iSubCount AS INTEGER NO-UNDO. 
   iSubCount = get_int(gcRetStruct, "sub_count").
   assert_equal_int(iSubCount, 1). /* One beginning from the first */

END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_trying_to_search_preactivated_with_string_custnum:
   add_string("", ?, "7").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "custnum"). /* Search Type = custnum */
   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},"Too many subscriptions to show").
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_trying_to_search_preactivated_with_int_custnum:
   add_int("", ?, 7).
   fSetLimitAndOffset(5, 0).
   add_string("", "", "custnum"). /* Search Type = custnum */

   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},"Too many subscriptions to show").
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_trying_to_search_non_existing_customer_with_string_custnum:
   add_string("", ?, "1223").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "custnum"). /* Search Type = custnum */

   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},"Customer 1223 not found 1").
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_trying_to_search_non_existing_customer_with_int_custnum:
   add_int("", ?, 1223).
   fSetLimitAndOffset(5, 0).
   add_string("", "", "custnum"). /* Search Type = custnum */

   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},"Customer 1223 not found 1").
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_trying_to_search_non_existing_customer_with_orgid:
   add_string("", ?, "X333223333").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "person_id"). /* Search Type = person_id */

   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},"Customer X333223333 not found 2").
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_trying_to_search_non_mobsub_with_CLI:
   add_string("", ?, "692939292").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "msisdn"). /* Search Type = msisdn */
   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},"MobSub entry 692939292 not found").
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE test_trying_to_search_customers_with_orgId_having_two_customers:
   add_string("", ?, "X2").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "person_id"). /* Search Type = person_id */
   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},
                "Please search with customer number").
END.

/**
 * This test searches multiple mobsubs with        The given
 * 
 * Fixtures fecthed:
 *
 * 
 */
PROCEDURE 
   test_trying_to_search_mobsub_with_cli_and_map_it_to_unexisting_customer:
   add_string("", ?, "633000000").
   fSetLimitAndOffset(5, 0).
   add_string("", "", "msisdn"). /* Search Type = msisdn */

   run_rpc_method("newton.search_mobsub").
   assert_fault({&APPLICATION_ERROR},"Customer 123123 not found 3").
END.



