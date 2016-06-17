/* 
   Comma separated list of files that should be copied into the sandbox:

   Depends = HPD/RepLogHandler.cls, HPD/CustomerDump.cls, HPD/GenericDump.cls, HPD/HPDAckReaderSingle.cls, HPD/DumpHandler.cls, HPD/HPDEnvironment.i, HPD/HPDConst.i, Syst/tmsconst.i, Syst/Parameters.cls, triggers/rw-customer.p, triggers/hpdwrite_generic.i, HPD/DumpHandlerWithQuery.cls, HPD/DumpBase.cls, HPD/DumpFile.cls, HPD/HPDCommon.cls, HPD/HPDTime.cls, HPD/HPDAckHandler.cls

   Comma separated list of dependencies of which mock versions should be generated:
   
   Mocks = Gwy/MqPublisher.cls, HPD/HPDAckReader.cls
*/
CLASS unit.HPD.ReplogAckTest INHERITS gearbox.unit.UnitTest:

   DEFINE VARIABLE objRepLogHandler AS CLASS HPD.RepLogHandler       NO-UNDO.
   DEFINE VARIABLE objHPDAckReader  AS CLASS HPD.HPDAckReader        NO-UNDO.
   DEFINE VARIABLE objStompMessage  AS CLASS dotr.Stomp.StompMessage NO-UNDO.
   DEFINE VARIABLE lcTempDir        AS CHARACTER                     NO-UNDO.
  
   METHOD OVERRIDE PUBLIC CHAR getFixtureFiles():
      RETURN "fixtures/basic/customer_mobsub.yaml,fixtures/hpd/reploghandler.yaml,fixtures/hpd/hpdackreader.yaml,fixtures/hpd/dumpfile.yaml".
   END METHOD.

   METHOD OVERRIDE PUBLIC VOID setUp ():

      lcTempDir = makeTempdir().

      fixtures:allForTable("TMSParam", NO).
      fixtures:allForTable("DumpFile", NO).
      fixtures:allForTable("DFField", NO).

      FOR FIRST TMSParam EXCLUSIVE-LOCK WHERE TMSParam.Paramcode = "HPD.Ack.ExportDir":

         TMSParam.CharVal = SUBSTRING(lcTempDir,1,LENGTH(lcTempDir) - 1).

      END.

      objRepLogHandler = NEW HPD.RepLogHandler("Common").
      objHPDAckReader = NEW HPD.HPDAckReaderSingle("Common").

   END METHOD.

   METHOD OVERRIDE PUBLIC VOID tearDown ():
      IF VALID-OBJECT(objRepLogHandler) THEN DELETE OBJECT objRepLogHandler.
      IF VALID-OBJECT(objHPDAckReader) THEN DELETE OBJECT objHPDAckReader.
   END METHOD.

  
   METHOD PRIVATE VOID mUpdateReplogSendTime
      (icTableName AS CHARACTER,
       iiMinutesFromNow AS INTEGER):
    
      DO TRANSACTION:
         FIND CURRENT common.replog EXCLUSIVE-LOCK.
         common.replog.sendtime = ADD-INTERVAL(NOW, iiMinutesFromNow, "minutes").
         FIND CURRENT common.replog NO-LOCK.
      END.

   END METHOD.

   METHOD PRIVATE VOID mCheckReplogAfterHandlerProcess
      ( icTableName AS CHARACTER,
        iiSendCount AS INTEGER ):

      FIND FIRST common.replog NO-LOCK WHERE replog.tablename = "Customer" NO-ERROR.
      
      assert(AVAILABLE common.replog, "No replog record available after replog handler").
      
      assertEqual("Replog sendcount", common.replog.SendCount, iiSendCount).
      
      assert(common.replog.sendtime NE ?, "Replog send time is not set").

   END METHOD.


   METHOD PRIVATE VOID mCheckReplogAfterAck
      ( icTableName AS CHARACTER,
        ilTimeLimitReached AS LOGICAL ):

      FIND FIRST common.replog NO-LOCK WHERE replog.tablename = "Customer" NO-ERROR.
      
      assert(AVAILABLE common.replog, "No replog record available after replog ack").
      
      IF ilTimeLimitReached
      THEN assert(common.replog.sendtime = ?, "Replog send time is already set after ack").
      ELSE assert(common.replog.sendtime <> ?, "Replog send time should be available after ack as the time limit is not reached").

   END METHOD.


   METHOD PRIVATE VOID mProcessAckMQ():
   
      DEFINE VARIABLE lcMessage AS LONGCHAR NO-UNDO.
      
      lcMessage = SUBSTITUTE('~{"dbid":"&1","keyid":"&2","time":"&3"~}', "common", STRING(ROWID(common.replog)), ISO-DATE(common.replog.eventtime)).

      objStompMessage = NEW dotr.Stomp.StompMessage("", "", "MESSAGE", lcMessage).
    
      objHPDAckReader:mProcess(objStompMessage).
      
      FINALLY:
         IF VALID-OBJECT(objStompMessage) THEN DELETE OBJECT objStompMessage.
      END FINALLY.            
     
   END METHOD.      


   METHOD PUBLIC VOID testRepLogCreated():
     
      fixtures:f("NormalCustomer"):generate().

      FIND FIRST common.replog NO-LOCK WHERE replog.tablename = "Customer" NO-ERROR.
      
      assert(AVAILABLE common.replog, "No replog record available").

      assert(objRepLogHandler:mIsEmpty() = FALSE, "Replog handler cannot find a replog record").
      
   END METHOD.
   
   
   METHOD PUBLIC VOID testReplogProcessed():
     
      testRepLogCreated().
      
      objRepLogHandler:mProcess().
      
      mCheckReplogAfterHandlerProcess("Customer", 1).
     
   END METHOD.


   METHOD PUBLIC VOID testAckSuccessful():
      
      testReplogProcessed().
      
      mProcessAckMQ().
      
      FIND FIRST common.replog NO-LOCK WHERE replog.tablename = "Customer" NO-ERROR.
      
      assert(NOT AVAILABLE common.replog, "Replog record still available after processing ACK to replog").
      
   END.


   METHOD PUBLIC VOID testNoAck():
      
      DEFINE VARIABLE lii AS INTEGER NO-UNDO.
      
      testReplogProcessed().
      
      objHPDAckReader:mProcess().
      mCheckReplogAfterAck("Customer", NO).
      
      mUpdateReplogSendTime("Customer", -30).
      objHPDAckReader:mProcess().
      mCheckReplogAfterAck("Customer", YES).
      
      DO lii = 2 TO 4:
         objRepLogHandler:mProcess().
         mCheckReplogAfterHandlerProcess("Customer", lii).
         
         mUpdateReplogSendTime("Customer", -30).
         objHPDAckReader:mProcess().
         mCheckReplogAfterAck("Customer", YES).
      END.
            
      objRepLogHandler:mProcess().
      mCheckReplogAfterHandlerProcess("Customer", 5).
      
      mUpdateReplogSendTime("Customer", -30).
      objHPDAckReader:mProcess().

      assert(NOT CAN-FIND(FIRST common.replog NO-LOCK WHERE replog.tablename = "Customer"), "Replog should be deleted after 5th failed attempt to send it").

   END METHOD.
    
END CLASS.
