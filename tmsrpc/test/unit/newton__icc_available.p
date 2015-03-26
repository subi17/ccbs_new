/**
 * This is the test set for the corresponding newton__icc_available
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
{timestamp.i}

gcFixtures = "SIM".

PROCEDURE test_icc_avail:
   add_string("", "", "8154213312312312").
   run_rpc_method("newton.icc_available").
   assert_success().
   assert(get_bool("", ""), "SIM was not avail although it should have been.").
END.

PROCEDURE test_icc_not_avail:
   add_string("", "", "8154444333355556").
   run_rpc_method("newton.icc_available").
   assert_success().
   assert(get_bool("", "") = FALSE, "SIM was avail although it should not have been.").
END.

PROCEDURE test_not_found_icc:
   add_string("", "", "81549999999999999").
   run_rpc_method("newton.icc_available").
   assert_success().
   assert(get_bool("", "") = FALSE, "SIM was avail although it should not have existed").
END.



