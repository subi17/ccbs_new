/**
 * This is the test set for the corresponding newton__get_order_memos
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

gcFixtures = "memo".

DEFINE VARIABLE gcParamArray AS CHARACTER NO-UNDO. 
gcParamArray = "".
DEFINE VARIABLE gcReturnStruct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 
DEFINE VARIABLE iNumberOfMemo AS INTEGER NO-UNDO. 

DEFINE VARIABLE lLogOn AS LOGICAL NO-UNDO.
lLogOn = FALSE.

/* Memo in variables */
DEFINE VARIABLE liMemoSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeCreStamp AS DECIMAL NO-UNDO. 
DEFINE VARIABLE liPriority AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCreator AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTitle AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcText AS CHARACTER NO-UNDO. 

/* more left -return value */
DEFINE VARIABLE liMoreLeft AS INTEGER NO-UNDO. 



FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF lLogOn THEN MESSAGE pcMsg.
   RETURN TRUE.
END.


/** 
 * This function calls the RPC method and checks the call succeeds.
 */
FUNCTION call_get_order_memos RETURN LOGICAL:
    fLog("before call...").
    run_rpc_method("newton.get_order_memos").
    fLog("end call... ").
    RETURN TRUE.
END FUNCTION.



FUNCTION fGetMemoToVars RETURN LOGICAL
  (INPUT pcMemoStruct AS CHARACTER):

 liMemoSeq = get_int(pcMemoStruct, "memo_id").
 fLog("liMemoSeq = " + STRING(liMemoSeq)).

 fLog(".....................................................").
 FOR EACH tt_param WHERE tt_param.parent = pcMemoStruct:
    fLog("Name = " + tt_param.name).
    fLog("Type = " + tt_param.type).
    fLog("cvalue = " + tt_param.cvalue).
    /*message tt_param.name skip tt_param.type skip tt_param.cvalue view-as alert-box.*/
 END.
 fLog(".....................................................").

gc_xmlrpc_error = "".
 /*ldeCreStamp = get_timestamp(pcMemoStruct, "creation_time").*/
message gc_xmlrpc_error.
 liPriority  = get_int(pcMemoStruct, "priority").
 liCustNum   = get_int(pcMemoStruct, "custnum").
 lcCreator   = get_string(pcMemoStruct, "creator").
 lcTitle     = get_string(pcMemoStruct, "title").
 lcText      = get_string(pcMemoStruct, "content").

 /*message liMemoSeq skip ldeCreStamp skip liPriority skip liCustNum skip lcCreator skip lcTitle skip lcText view-as alert-box.*/
END.

FUNCTION fCheckMemo RETURN LOGICAL
   (INPUT pcMemoStruct AS CHARACTER,
    INPUT piMemoNo AS INTEGER):

   fLog("MemoStruct: " + pcMemoStruct).
   fLog("piMemoNo: " + STRING(piMemoNo)).

   DEFINE VARIABLE lcFixtureName AS CHARACTER NO-UNDO. 
   /* Determine fixture */
   IF iOrderId eq 10 THEN
      lcFixtureName = "MemoForGetOrders1".
   ELSE IF iOrderId eq 11 THEN
      /* MemoSeq is descending in Memo.HostTable index, 1 -> 5, 2 -> 4 */
      lcFixtureName = "MemoForGetOrders" + STRING(6 - piMemoNo).

   fLog("lcFixtureName: " + lcFixtureName).

   fGetMemoToVars(pcMemoStruct).

   fetch_fixture(lcFixtureName, BUFFER Memo:HANDLE).


  /* fLog("ldeCreStamp = " + STRING(ldeCreStamp)).
   fLog("Memo.CreStamp = " + STRING(Memo.CreStamp)).*/

   checkInt("Memo.MemoSeq", Memo.MemoSeq, liMemoSeq).
  /* checkDecimal("Memo.CreStamp", Memo.CreStamp, ldeCreStamp). */
   checkInt("Memo.Priority", Memo.Priority, liPriority).
   checkInt("Memo.CustNum", Memo.CustNum, liCustNum).
   checkChar("Memo.CreUser", Memo.CreUser, lcCreator).
   checkChar("Memo.MemoTitle", Memo.MemoTitle, lcTitle).
   checkChar("Memo.MemoText", Memo.MemoText, lcText).
END.


FUNCTION fCheckMemos RETURN LOGICAL 
   (INPUT piBeginNo AS INTEGER, 
    INPUT piEndNo AS INTEGER):

   fLog("fCheckMemos, piBeginNo: " + STRING(piBeginNo) +
        ", piEndNo: " + STRING(piEndNo)).

   DEFINE VARIABLE liMemoNo AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcMemoArray    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMemoStruct AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE lcParamId AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE liMemoCount AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liExpectedMemoCount AS INTEGER NO-UNDO. 

   lcMemoArray = get_array(gcReturnStruct, "memos").

   fLog("lcMemoArray: " + lcMemoArray).

   liMemoCount = get_paramcount(lcMemoArray).
   liExpectedMemoCount = piEndNo - piBeginNo + 1.

   fLog("MemoCount = " + STRING(liMemoCount)).
   fLog("Expected MemoCount = " + STRING(liExpectedMemoCount)).
    
   checkInt("Memo array size", liMemoCount, liExpectedMemoCount).


   IF piEndNo >= piBeginNo THEN
      REPEAT liMemoNo = piBeginNo TO piEndNo:
         lcParamId = STRING(liMemoNo - piBeginNo).
         fLog("checking memo struct with lcParamId: " + lcParamId).
         lcMemoStruct = get_struct(lcMemoArray, lcParamId).
         fCheckMemo(lcMemoStruct, liMemoNo).
      END.
END.


PROCEDURE test_get_memos_for_unexisting_order:
   iOrderId = 20.
   fLog("---------------------------------------------------------").
   fLog("test_get_memos_for_unexisting_order").

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 5). /* max_count */
   add_int(gcParamArray, "", 0). /* from begin */

   call_get_order_memos().
   assert_success().

   gcReturnStruct = get_struct("", "").

   fCheckMemos(1,0). /* zero length array */

   limoreleft = get_int(gcreturnstruct, "more_memos").
   checkInt("morememos", 0, limoreleft).
END.


PROCEDURE test_get_order_with_no_memos:

   fLog("---------------------------------------------------------").
   fLog("test_get_order_with_no_memos").
   iOrderId = 8.

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 5). /* max_count */
   add_int(gcParamArray, "", 0). /* from begin */

   call_get_order_memos().
   assert_success().

   gcReturnStruct = get_struct("", "").

   fCheckMemos(1,0). /* zero length array */

   limoreleft = get_int(gcreturnstruct, "more_memos").
   checkInt("morememos", 0, limoreleft).
END.


PROCEDURE test_get_order_with_one_memo:
   fLog("--------------------------------------------------------").
   fLog("test_get_order_with_one_memo").

   iOrderId = 10.

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 5). /* max_count */
   add_int(gcParamArray, "", 0). /* from begin */

   call_get_order_memos().
   assert_success().

   gcReturnStruct = get_struct("", "").
   
   fCheckMemos(1,1).

   liMoreLeft = get_int(gcReturnStruct, "more_memos").
   checkInt("MoreMemos", 0, liMoreLeft).
END.


PROCEDURE test_get_order_with_four_memos_from_begin_unlimited:
   fLog("---------------------------------------------------------").
   fLog("test_get_order_with_four_memos_from_begin_unlimited").

   iOrderId = 11.

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 5). /* max_count */
   add_int(gcParamArray, "", 0). /* from begin */

   call_get_order_memos().

   gcReturnStruct = get_struct("", "").

   fCheckMemos(1,4).

   limoreleft = get_int(gcreturnstruct, "more_memos").
   checkInt("morememos", 0, limoreleft).
END.


PROCEDURE test_get_order_with_four_memos_with_start_point_unlimited:
   fLog("---------------------------------------------------------").
   fLog("test_get_order_with_four_memos_with_start_point_unlimited").

   iOrderId = 11.

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 5). /* max_count */
   add_int(gcParamArray, "", 0). /* from second memo */

   call_get_order_memos().

   gcReturnStruct = get_struct("", "").

   fCheckMemos(1,4).

   limoreleft = get_int(gcreturnstruct, "more_memos").
   checkInt("morememos", 0, limoreleft).
END.

PROCEDURE test_get_order_with_four_memos_from_begin_limited:
   fLog("---------------------------------------------------------").
   fLog("test_get_order_with_four_memos_from_begin_limited").

   iOrderId = 11.

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 5). /* max_count */
   add_int(gcParamArray, "", 0). /* from begin */

   call_get_order_memos().

   gcReturnStruct = get_struct("", "").

   fCheckMemos(1,4).

   limoreleft = get_int(gcreturnstruct, "more_memos").
   checkInt("morememos", 0, limoreleft).
END.


PROCEDURE test_get_order_with_four_memos_with_start_point_limited:
   fLog("--------------------------------------------------------").
   fLog("test_get_order_with_four_memos_with_start_point_limited").

   iOrderId = 11.

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 1). /* max_count */
   add_int(gcParamArray, "", 2). /* from third memo */

   call_get_order_memos().

   gcReturnStruct = get_struct("", "").

   fCheckMemos(3,3).

   limoreleft = get_int(gcreturnstruct, "more_memos").
   checkInt("morememos", 1, limoreleft).
END.


PROCEDURE test_get_order_with_four_memos_start_point_too_big:
   fLog("-----------------------------------------------------").
   fLog("test_get_order_with_four_memos_start_point_too_big").

   iOrderId = 11.

   add_int(gcParamArray, "", iOrderId).
   add_string(gcParamArray, "", "order"). /* type */
   add_int(gcParamArray, "", 3). /* max_count */
   add_int(gcParamArray, "", 4). /* from fifth memo */

   call_get_order_memos().

   gcReturnStruct = get_struct("", "").

   fCheckMemos(1,0). /* zero length array */

   limoreleft = get_int(gcreturnstruct, "more_memos").
   checkInt("morememos", 0, limoreleft).
END.


