/**
 * This is the test set for the corresponding tms__get_monthly_balance
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
{unit/checkutils.i}

gcFixtures = "mobsub,TMCounter". 

DEFINE VARIABLE gcReturnArray AS CHARACTER NO-UNDO. 


FUNCTION call_tms_get_monthly_balance RETURN LOGICAL:
    run_rpc_method("tms.get_monthly_balance").
    RETURN TRUE.
END FUNCTION.

PROCEDURE test_normal_case:
   add_string("", "", "727272727"). 
   call_tms_get_monthly_balance().
   assert_success().
   fetch_fixture("NormalCase", BUFFER TMCounter:HANDLE).
   fetch_fixture("STCMobsub", BUFFER MobSub:HANDLE).
   gcReturnArray = get_array("", "").
   checkDecimal("TMCounter.Amount", TMCounter.Amount, 
      get_double(gcReturnArray, "0")).
   checkChar("MobSub.CLI", MobSub.CLI, 
      get_string(gcReturnArray, "1")).
END.

PROCEDURE test_unexist_mobsub:
   add_string("", "", "999999999"). 
   call_tms_get_monthly_balance().
   assert_success().
   checkInt("errorno", 1, get_int("", "0")). 
END.

PROCEDURE test_prepaid:
   add_string("", "", "727272728"). 
   call_tms_get_monthly_balance().
   assert_success().
   checkInt("errorno", 2, get_int("", "0")). 
END.


PROCEDURE test_not_found_TMCounter:
   add_string("", "", "727272729"). 
   call_tms_get_monthly_balance().
   assert_success().
   fetch_fixture("MobsubNETMCounter", BUFFER MobSub:HANDLE).
   gcReturnArray = get_array("", "").
   checkDecimal("TMCounter.Amount", 0.0, 
      get_double(gcReturnArray, "0")).
   checkChar("MobSub.CLI", MobSub.CLI, 
      get_string(gcReturnArray, "1")).
END.
