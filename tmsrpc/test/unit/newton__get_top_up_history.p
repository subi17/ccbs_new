/**
 * This is the test set for the corresponding newton__get_top_up_history
 * XML RPC method.
 *
 * The fixture usage of this test set:
 *
 * - fixtures fetched: 
 *   prepaidrequest: ppr_fixture1, 
 *
 *                   ppr_fixture_multiple_set1_1,
 *                   ppr_fixture_multiple_set1_2,
 *                   ppr_fixture_multiple_set1_3,
 *
 *                   ppr_fixture_multiple_set2_1,
 *                   ppr_fixture_multiple_set2_2,
 *                   ppr_fixture_multiple_set2_3,
 *                   ppr_fixture_multiple_set2_4,
 *                   ppr_fixture_multiple_set2_5,
 *                   ppr_fixture_multiple_set2_6,
 *                   ppr_fixture_multiple_set2_7,
 *                   ppr_fixture_multiple_set2_8,
 *                   ppr_fixture_multiple_set2_9,
 *                   ppr_fixture_multiple_set2_10,
 *                   ppr_fixture_multiple_set2_11,
 *
 * - The RPC method is assumed to fetch the same records.
 * - restrictions for building other fixtures: 
 *         no more PrepaidRequest records may be added 
 *         for MsReq = 1192, 1193 or 1194.
 */

{test_xmlrpc_includes.i}


{/apps/xfera/tmsrpc/test/includes/dbutils.i} 

gcFixtures = "prepaidrequest,TMSCodes".

{Func/timestamp.i}
DEF VAR gcArray AS CHAR NO-UNDO.
DEF VAR gcSubArray AS CHAR NO-UNDO.


/* This function calls the actual RPC method, 
   check the success of the call and gets 
   the response array */
FUNCTION fetch_subscription RETURN LOGICAL ():
    run_rpc_method("newton.get_top_up_history").
    assert_success().
    gcArray = get_Array("", "").
    RETURN TRUE.
END FUNCTION.

/** 
*   This function checks one prepaid request added to 
*   the response. It is assumed that gcSubArray is
*   set to the array of the prepaidrequest record
*   and the corresponding PrepaidRequest record
*   is fetched.
*  
*   @param cTimeStamp   TSRequest changed to the format
*                       the RPC method uses.
*   @param cOK          "OK" if PrepaidRequest RespCode is
*                       assumed to have 0 value;
*                       "NOK" otherwise.
*   @param cTopUpAmt    PrepaidRequest.TopUpAmt handled 
*                       in the way RPC method does it.
*   @param cVatAmt      PrepaidRequest.cVatAmt handled 
*                       in the way RPC method does it.
*/
FUNCTION check_prepaidrequest RETURNS LOGICAL (
   INPUT cTimeStamp AS CHARACTER, 
   INPUT cOK AS CHARACTER,
   INPUT cTopUpAmt AS CHARACTER,
   INPUT cVatAmt AS CHARACTER):

/*   MESSAGE "---------------------------------------" VIEW-AS ALERT-BOX.

   MESSAGE "FixTure, PPRequest: " PrepaidRequest.PPRequest VIEW-AS ALERT-BOX.

   MESSAGE "Fixture, CLI: " PrepaidRequest.CLI ", response, CLI " 
         get_string(gcSubArray, "0") VIEW-AS ALERT-BOX.
   MESSAGE "Expected timestamp: " cTimeStamp ", response timestamp: "
             get_string(gcSubArray, "3") VIEW-AS ALERT-BOX. 
   MESSAGE "Fixture, Request: " PrepaidRequest.Request ", response, Request "  
         get_string(gcSubArray, "1") VIEW-AS ALERT-BOX. 
   MESSAGE "Expected Source: " PrepaidRequest.Source ", response source: "
             get_string(gcSubArray, "2") VIEW-AS ALERT-BOX.  */

   assert_equal_char(PrepaidRequest.CLI, get_string(gcSubArray, "0")).
   assert_equal_char(PrepaidRequest.Request, get_string(gcSubArray, "1")).
   assert_equal_char(PrepaidRequest.Source, get_string(gcSubArray, "2")).
 

   assert_equal_char(cTimeStamp, get_string(gcSubArray, "3")). 
   assert_equal_char(cOK, get_string(gcSubArray, "4")).
/*    MESSAGE "here1" VIEW-AS ALERT-BOX. */
   assert_equal_char(cTopUpAmt, get_string(gcSubArray, "5")).
/*   MESSAGE "here2" VIEW-AS ALERT-BOX. */
   assert_equal_char(cVatAmt, get_string(gcSubArray, "6")). 

   assert_equal_char(PrepaidRequest.Reference, get_string(gcSubArray, "7")).

   IF PrepaidRequest.Entidad = "Ent1" THEN /* Value taken from TMSCodes */
   DO:
      assert_equal_char("Entidad1", get_string(gcSubArray, "8")).
   END.
   ELSE
      assert_equal_char(PrepaidRequest.Entidad, get_string(gcSubArray, "8")).

/*   MESSAGE "---------------------------------------" VIEW-AS ALERT-BOX. */
END.


/**
 * Gets PrepaidRequest records with mobsub MsSeq for which there exists 
 * exactly one PrepaidRequest and checks the data of the records is received
 * correctly. The data of individual PrepaidRequest record is checked 
 * with function check_prepaidrequest.
 *
 * Fixtures used:
 *    prepaidrequest: ppr_fixture1 (MsSeq: 1192)
 */
PROCEDURE test_get_one_prepaidrequest:
    add_int("", ?, 1192).
    add_int("", ?, 10).

   delete_disturbing_records(BUFFER PrepaidRequest:HANDLE,
      "PrepaidRequest", "PPRequest", 
      "34,35,36,37,38,39,40,41,42,43,44,45,46,47,48" ).   

    fetch_subscription().

    fetch_fixture("ppr_fixture1", BUFFER PrepaidRequest:HANDLE).
    gcSubArray = get_Array(gcArray,"0").
    assert_response(gcSubArray,
        "string,string,string,string,string,string,string,string,string").
    check_prepaidrequest("25-10-2007 12:04:44", "OK", "1.22", "1.35").
END PROCEDURE.


/**
 * Gets PrepaidRequest records with mobsub MsSeq for which there exists 
 * exactly three PrepaidRequest and checks the data of the records 
 * is received correctly. The count of the records is checked.
 * The data of individual PrepaidRequest record is checked 
 * with function check_prepaidrequest.
 */
PROCEDURE test_get_multiple_prepaidrequest:
    add_int("", ?, 1193).
    add_int("", ?, 10).

/*    MESSAGE "-------------- test_get_multiple_prepaidrequest begin --------"
       VIEW-AS ALERT-BOX. */


    delete_disturbing_records(BUFFER PrepaidRequest:HANDLE,
      "PrepaidRequest", "PPRequest", 
      "34,35,36,37,38,39,40,41,42,43,44,45,46,47,48" ).   

    fetch_subscription().

    /* test count is 3 */
    assert_equal_int(get_paramcount(gcArray), 3).

    /* 1 st entry */
    gcSubArray = get_Array(gcArray,"0").
    assert_response(gcSubArray,
        "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set1_1", BUFFER PrepaidRequest:HANDLE).
    check_prepaidrequest("26-10-2007 11:57:20", "OK", "1.22", "1.33").

    /* 2 nd entry */
    gcSubArray = get_Array(gcArray,"1").
    assert_response(gcSubArray,
        "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set1_2", BUFFER PrepaidRequest:HANDLE).
    check_prepaidrequest("26-10-2007 11:57:00", "OK", "2.22", "2.23").

    /* 3 rd entry */
    gcSubArray = get_Array(gcArray,"2").
    assert_response(gcSubArray,
        "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set1_3",BUFFER PrepaidRequest:HANDLE).
    check_prepaidrequest("26-10-2007 11:56:40", "NOK", "2.25", "2.72").

/*    MESSAGE "-------  test_get_multiple_prepaid_request-----------------" VIEW-AS ALERT-BOX. */
END PROCEDURE.

/**
 * Gets PrepaidRequest records with mobsub MsSeq for which there exists 
 * exactly 11 PrepaidRequest records and checks the data of the records 
 * is received correctly. 
 * The RPC method is assumed to return the maximum count of records: 10.
 * The count of the records is checked.
 * The data of individual PrepaidRequest record is checked 
 * with function check_prepaidrequest.
 */
PROCEDURE test_get_max_overmax_prepaidrequest:
    add_int("", ?, 1194).
    add_int("", ?, 10).


/*    MESSAGE "-------------- test_get_overmax_prepaidrequest begin --------"
       VIEW-AS ALERT-BOX. */

   delete_disturbing_records(BUFFER PrepaidRequest:HANDLE,
      "PrepaidRequest", "PPRequest", 
      "34,35,36,37,38,39,40,41,42,43,44,45,46,47,48" ).   

    fetch_subscription().

    /* test count is 10 */
    assert_equal_int(get_paramcount(gcArray), 10).

    /* 1 st entry */
    gcSubArray = get_Array(gcArray,"0").
    assert_response(gcSubArray,
        "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_1",BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43010 */
    check_prepaidrequest("15-01-2008 12:16:50", "OK", "2.26", "35.20").

    /* 2 nd entry */
    gcSubArray = get_Array(gcArray,"1").
    assert_response(gcSubArray,
        "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_2",BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43020 */
    check_prepaidrequest("15-01-2008 12:16:40", "OK", "23.89", "13.32").


    /* 3 rd entry */
    gcSubArray = get_Array(gcArray,"2").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_3", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43030 */
    check_prepaidrequest("15-01-2008 12:13:20", "OK", "22.77", "13.33").


    /* 4 th entry */
    gcSubArray = get_Array(gcArray,"3").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_4", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43040 */
    check_prepaidrequest("15-01-2008 11:56:40", "NOK", "2.26", "35.20").

     /* 5 th entry */
    gcSubArray = get_Array(gcArray,"4").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_5", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43050 */
    check_prepaidrequest("15-01-2008 11:55:50", "OK", "23.89", "13.32").
 
    /* 6 th entry */
    gcSubArray = get_Array(gcArray,"5").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_6", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43060 */
    check_prepaidrequest("15-01-2008 11:48:20", "NOK", "22.77", "13.33").

    /* 7 th entry */
    gcSubArray = get_Array(gcArray,"6").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_7", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43070 */
    check_prepaidrequest("15-01-2008 11:23:20", "OK", "2.26", "35.20").

    /* 8 th entry */
    gcSubArray = get_Array(gcArray,"7").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_8", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43080 */
    check_prepaidrequest("13-01-2008 11:23:20", "NOK", "23.89", "13.32").

    /* 9 th entry */
    gcSubArray = get_Array(gcArray,"8").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_9", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43090 */
    check_prepaidrequest("12-01-2008 18:03:20", "OK", "22.77", "13.33").

    /* 10 th entry */
    gcSubArray = get_Array(gcArray,"9").
    assert_response(gcSubArray,
       "string,string,string,string,string,string,string,string,string").

    fetch_fixture("ppr_fixture_multiple_set2_10", BUFFER PrepaidRequest:HANDLE).
    /* TimeStamp value: 20071026.43100 */
    check_prepaidrequest("12-01-2008 12:13:20", "OK", "34.00", "13.33").

/*    MESSAGE "-------------- test_get_overmax_prepaidrequest end --------"
       VIEW-AS ALERT-BOX. */

 END PROCEDURE.


 /**
  * Gets PrepaidRequests with MsSeq with which there does not 
  * exist any records. Checks that RPC method succeeds and 
  * the count of the records is 0.
  */
 PROCEDURE test_zero_prepaid_request:
    add_int("", ?, 1195).
    add_int("", ?, 10).

    delete_disturbing_records(BUFFER PrepaidRequest:HANDLE,
      "PrepaidRequest", "PPRequest", 
      "34,35,36,37,38,39,40,41,42,43,44,45,46,47,48" ).   

    fetch_subscription().

    /* test count is 0 */
    assert_equal_int(get_paramcount(gcArray), 0).
 END.
