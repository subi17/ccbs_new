/*
  Depends = Mm/fbundle.i,Syst/tmsconst.i,Func/date.i,Func/cparam2.i,Func/fcreatereq.i,Func/matrix.i,Func/transname.i,Func/ftaxdata.i,Func/xmlfunction.i,Func/fdss.i,Func/fmakemsreq.i,Func/fparse.i,Func/fixedfee.i,Syst/eventval.i,Func/create_eventlog.i,Func/dss_search.i,Func/msreqfunc.i,Func/fctserval.i,Func/fcustdata.i,Func/coinv.i,Func/fmakesms.i,Func/fgettxt.i,triggers/rw-daycampaign.p,triggers/hpdwrite_generic.i,HPD/HPDConst.i,triggers/rw-customer.p,triggers/rw-mobsub.p,Gwy/pp_platform.p,Gwy/airnodes.i,Gwy/tcpgwy.p,Mm/msbalance.i
  Mocks   = Syst/commali.i
*/
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS unit.Mm.AdjustBalTest INHERITS gearbox.unit.ProceduralTest:

   METHOD OVERRIDE PUBLIC CHAR getFixtureFiles():
      RETURN "fixtures/basic/customer_mobsub.yaml,fixtures/gwy/adjustbal.yaml".
   END METHOD.


   METHOD OVERRIDE PUBLIC CHAR getTestedFile ():
      RETURN "Mm/fbundle.i".
   END METHOD.


   METHOD OVERRIDE PUBLIC VOID setUp ():
      fixtures:allForTable("TMSParam", NO).
      fixtures:allForTable("DayCampaign", NO).
      fixtures:f("NormalCustomer"):generate(SUBSTITUTE("CustNum=&1", 1)).
      fixtures:f("NormalSubsription"):generate(SUBSTITUTE("CustNum=&1,MsSeq=&2", 1, 1)).
   END METHOD.


   METHOD OVERRIDE PUBLIC VOID setupHandle ( INPUT ihTested AS HANDLE ):
   END METHOD.


   METHOD PUBLIC VOID testfIsBundle ( INPUT ihTested AS HANDLE ):
      DEF VAR llReturnValue AS LOGICAL NO-UNDO.
      llReturnValue = DYNAMIC-FUNCTION("fIsBundle" IN ihTested, "DUB").
      assertEqual(llReturnValue, FALSE).
      llReturnValue = DYNAMIC-FUNCTION("fIsBundle" IN ihTested, "TERMPAYM12DATA").
      assertEqual(llReturnValue, TRUE).
      llReturnValue = DYNAMIC-FUNCTION("fIsBundle" IN ihTested, "DATADEVRC24").
      assertEqual(llReturnValue, FALSE).
   END METHOD.


   METHOD PUBLIC VOID testpAdjustBal ( INPUT ihTested AS HANDLE ):

      DEFINE VARIABLE lcError      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ldeAdjAmount AS DECIMAL   NO-UNDO.

      ldeAdjAmount = 11.1.

      RUN pAdjustBal IN ihTested ({&PMDUB}, 1, ldeAdjAmount, OUTPUT lcError).

      assertEqual(lcError, "").      

      FIND FIRST Memo NO-LOCK NO-ERROR.

      assert(AVAILABLE Memo, "Memo is not created").

      FIND FIRST TopUpQueue NO-LOCK NO-ERROR.

      assert(AVAILABLE TopUpQueue, "TopUpQueue is not created").

      assertEqual(TopUpQueue.TopUpAmt * -1, ldeAdjAmount).

   END METHOD.

END CLASS.
