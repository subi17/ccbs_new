// Depends = Mm/fbundle.i,Syst/commali.i,Syst/tmsconst.i,Func/date.i,Func/timestamp.i,Func/cparam2.i,Func/fcreatereq.i,Func/matrix.i,Func/transname.i,Func/ftaxdata.i,Func/xmlfunction.i,Func/fdss.i,Func/fmakemsreq.i,Func/fparse.i,Func/fixedfee.i,Syst/eventval.i,Func/create_eventlog.i,Func/msreqfunc.i,Func/fctserval.i,Func/fcustdata.i,Func/coinv.i,Func/fmakesms.i,Func/fgettxt.i,Func/freplacesms.i
// Mocks   = .

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


   METHOD OVERRIDE PUBLIC VOID setupHandle ( INPUT lhTested AS HANDLE ):
      // Do something to lhTested
   END METHOD.


   METHOD PUBLIC VOID testfIsBundle ( INPUT lhTested AS HANDLE ):
      DEF VAR llReturnValue AS LOGICAL NO-UNDO.
      llReturnValue = DYNAMIC-FUNCTION("fIsBundle" IN lhTested, "DUB").
      assertEqual(llReturnValue, FALSE).
      llReturnValue = DYNAMIC-FUNCTION("fIsBundle" IN lhTested, "TERMPAYM12DATA").
      assertEqual(llReturnValue, FALSE).
      llReturnValue = DYNAMIC-FUNCTION("fIsBundle" IN lhTested, "DATADEVRC24").
      assertEqual(llReturnValue, TRUE).
   END METHOD.


   METHOD PUBLIC VOID testpAdjustBal ( INPUT lhTested AS HANDLE ):

      DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.

      RUN pAdjustBal IN lhTested ({&PMDUB}, 1, 11.1, OUTPUT lcError).
      assertEqual(lcError, "").      

   END METHOD.

END CLASS.
