{test_xmlrpc_includes.i}

/**
 * This is the test of corresponding newton__add_termination_request 
 * XML RPC method.
 *
 * The fixture usage of this test:
 *
 *   active_subscription 
 *   active_subscription_imsi,
 *   active_subscription_mssdn
 *
 */

gcFixtures = "mobsub,msisdn,imsi".

DEF VAR gcParamStruct AS CHAR NO-UNDO.

DEF VAR pcSalesMan AS CHAR NO-UNDO. 
DEF VAR piMsSeq    AS INT  NO-UNDO. 
DEF VAR piOrderer  AS INT  NO-UNDO. 
DEF VAR pdeKillTS  AS DEC  NO-UNDO. 
DEF VAR piMsisdnStat   AS INT  NO-UNDO. 
DEF VAR piSimStat      AS INT  NO-UNDO. 
DEF VAR piQuarTime AS INT  NO-UNDO. 
DEF VAR pcOpCode AS CHAR NO-UNDO. 

FUNCTION fResetMin RETURNS LOG:
   ASSIGN 
      pcSalesMan = "XMLRPC_TEST" 
      piMsSeq    = 1 
      piOrderer  = 1 
      pdeKillTS  = 30000101.10000
      piMsisdnStat = 0
      piSimStat  = 0
      piQuartime = 0
      pcOpCode   = "777777". 
END FUNCTION. 

FUNCTION fResetMax RETURNS LOG:
   fResetMin().
   ASSIGN
   piMsisdnStat = 4
   piSimStat   = 7.

END FUNCTION. 

FUNCTION fadd_parameters_and_run RETURNS LOGICAL:

   gcParamStruct = add_struct("", ?).
   IF pcSalesman NE "" THEN
      add_string(gcParamStruct, "salesman", pcSalesman).
   IF piMsSeq NE 0 THEN 
      add_int(gcParamStruct, "msseq", piMsSeq).
   IF piOrderer NE 0 THEN
      add_int(gcParamStruct, "orderer", piOrderer).
   IF pdeKillTS NE 0 THEN
      add_timestamp(gcParamStruct, "killts", pdeKillTS).
   IF piMsisdnStat NE 0 THEN
      add_int(gcParamStruct, "msisdnstat", piMsisdnStat).
   IF piSimStat NE 0 THEN
      add_int(gcParamStruct, "simstat", piSimStat).
   IF piQuarTime NE 0 THEN
      add_int(gcParamStruct, "quartime", piQuarTime).
   IF pcOpcode NE "" THEN
      add_int(gcParamStruct, "opcode", INT(pcOpCode)).
   run_rpc_method("newton.add_termination_request").
END.

FUNCTION fadd_termination_request RETURNS LOGICAL:
    run_rpc_method("newton.add_termination_request").
    assert_success().
    assert(get_bool("",""), "Create termination request failed").
END.

PROCEDURE test_min_params:
    fResetMin().
    fadd_parameters_and_run().    
    assert_success().
    assert(get_bool("",""), "Create termination request failed").
    
    fetch_fixture("active_subscription",BUFFER MobSub:HANDLE).
    FIND FIRST msrequest where
      msrequest.brand = "1" and
      msrequest.reqtype = 18 and
      msrequest.msseq = mobsub.msseq NO-LOCK NO-ERROR.
    assert_equal_char(msrequest.cli,mobsub.cli).
    assert_equal_char(msrequest.reqcparam1,"DELETE").
    assert_equal_char(msrequest.reqcparam3,STRING(piOrderer)).
    assert_equal_int(msrequest.custnum,mobsub.custnum).
    assert_equal_int(msrequest.reqiparam1,11).
    assert_equal_int(msrequest.reqiparam2,7).
    assert_equal_char(msrequest.usercode, "VISTA_" + pcSalesman).
    assert_equal_int(msrequest.reqiparam3,-1).
END PROCEDURE.

PROCEDURE test_max_params:
    fResetMax().
  /*  piMsisdnStat = 5. */
  /*  piSimStat = 9. */
    fadd_parameters_and_run().    
    assert_success().
    assert(get_bool("",""), "Create termination request failed").
    fetch_fixture("active_subscription",BUFFER MobSub:HANDLE).
    FIND FIRST msrequest where
      msrequest.brand = "1" and
      msrequest.reqtype = 18 and
      msrequest.msseq = mobsub.msseq NO-LOCK NO-ERROR.
    assert_equal_char(msrequest.cli,mobsub.cli).
    assert_equal_char(msrequest.reqcparam1,"DELETE").
    assert_equal_char(msrequest.reqcparam3,STRING(piOrderer)).
    assert_equal_int(msrequest.custnum,mobsub.custnum).
    assert_equal_int(msrequest.reqiparam1,11). /* default */
    assert_equal_int(msrequest.reqiparam2,7). /* default */
    assert_equal_char(msrequest.usercode, "VISTA_" + pcSalesman).
    assert_equal_int(msrequest.reqiparam3,-1). 

 END PROCEDURE.

PROCEDURE test_wrong_orderer:
    fResetMin().
    piOrderer = 20.
    pdeKillTS = 30000101.00000.
    fadd_parameters_and_run().    
    assert_fault({&APPLICATION_ERROR},"Unknown Orderer").
END PROCEDURE.

PROCEDURE test_order_cancellation_with_mnp_number:
    fResetMin().
    piOrderer = 3.
    pdeKillTS = 30000101.00000.
    fadd_parameters_and_run().    
    assert_fault({&APPLICATION_ERROR},"Cannot choose Order cancellation with MNP numbers!").
END PROCEDURE.

PROCEDURE test_killtime_past:
    fResetMin().
    pdeKillTS = 20000101.00000.
    fadd_parameters_and_run().    
    assert_fault({&APPLICATION_ERROR},"Kill time cannot be in the past!").
END PROCEDURE.

PROCEDURE test_wrong_killtime_window:
    fResetMin().
    piOrderer = 2.
    pdeKillTS = 30000101.07199.
    fadd_parameters_and_run().    
    assert_fault({&APPLICATION_ERROR},"Time is outside time window!").
END PROCEDURE.

PROCEDURE test_wrong_opcode:
    fResetMin().
    piOrderer = 2.
    pdeKillTS = 30000101.07200.
    pcOpCode = "42".
    fadd_parameters_and_run().    
    assert_fault({&APPLICATION_ERROR},"Operator code must be 6 digits!").
END PROCEDURE.

PROCEDURE test_wrong_opcode2:
    fResetMin().
    piOrderer = 2.
    pdeKillTS = 30000101.07200.
    pcOpCode = "666666".
    fadd_parameters_and_run().    
    assert_fault({&APPLICATION_ERROR},"Operator code must begin with digit 7 or 9!").
END PROCEDURE.

PROCEDURE test_wrong_opcode3:
    fResetMin().
    piOrderer = 2.
    pdeKillTS = 30000101.07200.
    pcOpCode = "744444".
    fadd_parameters_and_run().    
    assert_fault({&APPLICATION_ERROR},"Operator code must be outside range 740000 - 744999!").
END PROCEDURE.

PROCEDURE test_wrong_msisdnstat:
   fResetMax().
   piOrderer = 5.
   piMsisdnStat = 666.
   fadd_parameters_and_run().    
   assert_fault({&APPLICATION_ERROR},"Incorrect or missing MSISDN status value").
END PROCEDURE.

PROCEDURE test_wrong_simstat:
   fResetMax().
   piOrderer = 5.
   piSimStat = 666.
   fadd_parameters_and_run().    
   assert_fault({&APPLICATION_ERROR},"Incorrect or missing ICC status value").
END PROCEDURE.

PROCEDURE test_wrong_quartime:
   fResetMax().
   piMsSeq = 14. /* yoigo cli */
   piOrderer = 5.
   piMsisdnStat = 4.
   piQuarTime = 100.
   fadd_parameters_and_run().    
   assert_fault({&APPLICATION_ERROR},"Value must be between 1 and 90!").
END PROCEDURE.

PROCEDURE test_ongoing_request:
   fResetMin().
   fetch_fixture("active_subscription",BUFFER MobSub:HANDLE).
   create msrequest.
   assign
      msrequest.brand = "1"
      msrequest.reqtype = 18 
      msrequest.reqstatus = 0 
      msrequest.msseq    = mobsub.msseq .
   fadd_parameters_and_run().    
   assert_fault({&APPLICATION_ERROR},"Ongoing termination requests").
  
END PROCEDURE.
