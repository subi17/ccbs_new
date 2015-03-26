{test_xmlrpc_includes.i}

gcFixtures = "Fatime,Invoice,mobsub".


DEFINE VARIABLE gcRetArray AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gcRetStruct AS CHARACTER NO-UNDO. 

FUNCTION call_get_freeairtime RETURN LOGICAL ():
    run_rpc_method("newton.get_freeairtime").
    assert_success().
END.


FUNCTION check_record RETURN LOGICAL (INPUT lInvoiceExists AS LOGICAL):

/*   MESSAGE "before id" VIEW-AS ALERT-BOX. */

   assert_equal_int(Fatime.FatNum, get_int(gcRetStruct, "fat_id") ).

/*   MESSAGE "after id" VIEW-AS ALERT-BOX. */
   assert_equal_char(Fatime.FTGrp, get_string(gcRetStruct, "fat_group") ).

   DEFINE VARIABLE lFeeType AS LOGICAL NO-UNDO. 
   lFeeType = get_bool(gcRetStruct, "fee_type").
   assert( Fatime.fatclass eq lFeeType, "feetype differs").

/*
   FIND tt_param
        WHERE tt_param.inout EQ 1
          AND tt_param.parent = gcRetStruct
          AND tt_param.name = "fee_type" NO-ERROR.

   IF AVAIL tt_param THEN
     MESSAGE tt_param.cvalue VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "tt_param not found" VIEW-AS ALERT-BOX. */

   DEFINE VARIABLE lcc AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMonth AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcYear AS CHARACTER NO-UNDO. 
   lcc  = STRING(fatime.period, "999999").
   lcMonth = SUBSTRING(lcc, 5, 2).
   lcYear = SUBSTRING(lcc, 1, 4).

/*   MESSAGE lcc VIEW-AS ALERT-BOX.
   MESSAGE lcMonth VIEW-AS ALERT-BOX. 
   MESSAGE lcYear VIEW-AS ALERT-BOX. */

   DEFINE VARIABLE decTimeStamp AS DECIMAL NO-UNDO.

   DEFINE VARIABLE daTimeStamp AS DATE NO-UNDO.

/*   daTimeStamp = get_date(gcRetStruct, "period").
   MESSAGE daTimeStamp VIEW-AS ALERT-BOX.

   decTimeStamp = get_timestamp(gcRetStruct, "period"). 
   MESSAGE decTimeStamp VIEW-AS ALERT-BOX.

   DEFINE VARIABLE cTimeStamp AS CHARACTER NO-UNDO. 
   cTimeStamp = STRING(decTimeStamp).

   FIND tt_param
        WHERE tt_param.inout EQ 1
          AND tt_param.parent = gcRetStruct
          AND tt_param.name = "period" NO-ERROR.

   IF AVAIL tt_param THEN
     MESSAGE tt_param.cvalue VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "tt_param not found" VIEW-AS ALERT-BOX.


   assert_equal_char( lcYear + lcMonth + '01', 
      cTimeStamp ). */

   assert_equal_char( STRING( Fatime.amt ), 
      STRING( get_double(gcRetStruct, "amount" ))).

   



   assert_equal_char( STRING( Fatime.fatperc ),
      STRING( get_double(gcRetStruct, "percent" ))).

   assert_equal_char( STRING( Fatime.used ),
      STRING( get_double(gcRetStruct, "used" ))).

   DEFINE VARIABLE lTransfer AS LOGICAL NO-UNDO. 
   lTransfer = get_bool(gcRetStruct, "transferrable").
   assert( Fatime.transfer eq lTransfer, "feetype differs").


   assert_equal_char( Fatime.CLI, get_string(gcRetStruct, "cli" )).

   IF lInvoiceExists THEN
      assert_equal_char( Invoice.ExtInvId, get_string(gcRetStruct, "ext_invnum") ).
   ELSE
      assert_equal_char( STRING(Fatime.Invnum), get_string(gcRetStruct, "ext_invnum") ).

   RETURN TRUE.
END.



FUNCTION check_records RETURNS LOGICAL ():
   gcRetArray = get_array("", "").
/*   MESSAGE "here" VIEW-AS ALERT-BOX. */
   assert_equal_int(get_paramcount(gcRetArray), 3).
/*   MESSAGE "here2" VIEW-AS ALERT-BOX. */

   gcRetStruct = get_struct(gcRetArray, "0").

   fetch_fixture("FirstFaTime", BUFFER Fatime:HANDLE).
   fetch_fixture("FatimeInvoice1", BUFFER Invoice:HANDLE).
   check_record(TRUE).


   gcRetStruct = get_struct(gcRetArray, "1").

   fetch_fixture("ThirdFaTime", BUFFER Fatime:HANDLE).
   check_record(FALSE).
   
   gcRetStruct = get_struct(gcRetArray, "2").

   fetch_fixture("SecondFaTime", BUFFER Fatime:HANDLE).
   fetch_fixture("FatimeInvoice2", BUFFER Invoice:HANDLE).

   check_record(TRUE).

   RETURN TRUE.
END.


PROCEDURE test_get_fatimes:
   add_int("", ?, 13).
   call_get_freeairtime().

   check_records().
END.

PROCEDURE test_non_existent_mobsub:
   add_int("", ?, 21313).
   run_rpc_method("newton.get_freeairtime").
   assert_fault( {&APPLICATION_ERROR}  , "Mobsub entry for 21313 not found").
END.
