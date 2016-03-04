USING Progress.Lang.*.
USING dotr.Stomp.*.
USING dotr.Stomp.Interface.*.

{HPD/HPDEnvironment.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDAckReader ABSTRACT
   IMPLEMENTS dotr.Stomp.Interface.IStompHandler:

   DEFINE PRIVATE VARIABLE lcHostName             AS CHARACTER      NO-UNDO.
   DEFINE PRIVATE VARIABLE liPortNumber           AS INTEGER        NO-UNDO.
   DEFINE PRIVATE VARIABLE lcQueueName            AS CHARACTER      NO-UNDO.

   DEFINE PRIVATE VARIABLE loStompConfig          AS StompConfig    NO-UNDO.
   DEFINE PRIVATE VARIABLE loStompClient          AS StompClient    NO-UNDO.

   DEFINE PRIVATE VARIABLE lhWaitForProcedure     AS HANDLE         NO-UNDO.
   DEFINE PRIVATE VARIABLE llDisconnected AS LOGICAL INITIAL FALSE  NO-UNDO.
   DEFINE PRIVATE VARIABLE liAckBatch             AS INTEGER        NO-UNDO.
   DEFINE PRIVATE VARIABLE lcExportDir            AS CHARACTER NO-UNDO.

   DEFINE PROTECTED VARIABLE liAckCount           AS INTEGER        NO-UNDO.
   DEFINE PROTECTED VARIABLE lcJSON AS LONGCHAR INITIAL '~{"":&1~}' NO-UNDO.
   DEFINE PROTECTED VARIABLE ldtzNextCheckTime    AS DATETIME-TZ    NO-UNDO.
   DEFINE PROTECTED VARIABLE liCheckWaitMinutes   AS INTEGER        NO-UNDO.
   DEFINE PROTECTED VARIABLE liMaxLockedLoopCount AS INTEGER        NO-UNDO.
   
   DEFINE PROTECTED TEMP-TABLE ttResponse NO-UNDO SERIALIZE-NAME ""
      FIELD DBId      AS CHARACTER   SERIALIZE-NAME "dbid"
      FIELD KeyValue  AS CHARACTER   SERIALIZE-NAME "keyid"
      &IF DEFINED(OLD_HEADER_VERSION) &THEN
      FIELD EventTime AS DATETIME    SERIALIZE-NAME "time"
      &ELSE
      FIELD EventTime AS DATETIME-TZ SERIALIZE-NAME "time"
      &ENDIF
      INDEX DBid IS PRIMARY DBId
      .

   DEFINE PROTECTED DATASET ldsAckDS SERIALIZE-HIDDEN FOR ttResponse.

   DEFINE PRIVATE STREAM outstr.


   CONSTRUCTOR PROTECTED HPDAckReader (icDB AS CHARACTER):

      ASSIGN
         lcHostName           = Syst.Parameters:getc("HPDAck", "HPDAckHostName")
         liPortNumber         = Syst.Parameters:geti("HPDAck", "HPDAckPortNumber")
         lcQueueName          = Syst.Parameters:getc("HPDAck", "HPDAckQueue" + icDB)
         liAckBatch           = Syst.Parameters:geti("HPDAck", "HPDAckBatchSize")
         liCheckWaitMinutes   = Syst.Parameters:geti("HPDAck", "HPDAckCheckWaitMin")
         liMaxLockedLoopCount = Syst.Parameters:geti("HPDAck", "HPDAckMaxLockedLoop")
         lcExportDir          = Syst.Parameters:getc("HPDAck", "HPDAckExportDir")
         ldtzNextCheckTime    = NOW
         .

      mlogMsg(SUBSTITUTE("SETTINGS: hostname=&1, port=&2, queueName=&1",
                         lcHostName,
                         STRING(liPortNumber),
                         lcQueueName), "INIT").

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC HPDAckReader():

      DATASET ldsAckDS:EMPTY-DATASET().
      
      mCloseSession().

   END DESTRUCTOR.   

   
   METHOD PROTECTED VOID mLogMsg
      ( icMsg     AS CHARACTER,
        icMsgType AS CHARACTER ):

      LOG-MANAGER:WRITE-MESSAGE(icMsg, icMsgType).

   END METHOD.


   METHOD PRIVATE VOID mCloseSession():
      
      loStompClient:UnSubscribeFromQueue(lcQueueName) NO-ERROR.
      
      IF VALID-OBJECT(loStompClient)
      THEN DELETE OBJECT loStompClient.
      
      IF VALID-OBJECT(loStompConfig)
      THEN DELETE OBJECT loStompConfig.
      
      IF VALID-OBJECT(dotr.Stomp.StompConfig:DEFAULT)
      THEN DELETE OBJECT dotr.Stomp.StompConfig:DEFAULT.

   END METHOD.


   METHOD PRIVATE VOID mConfig ():

      loStompConfig = dotr.Stomp.StompConfig:DEFAULT.

      ASSIGN
         loStompConfig:StompServer         = lcHostName
         loStompConfig:StompPort           = STRING(liPortNumber)
         loStompConfig:LargeMessageSupport = NO
         .

   END METHOD.


   METHOD PRIVATE VOID mConnect():
      
      loStompClient = NEW dotr.Stomp.StompClient(loStompConfig).      
      loStompClient:SUBSCRIBE(THIS-OBJECT).
      loStompClient:subscribeToQueueACK(lcQueueName,"client-individual").

   END METHOD.


   METHOD PUBLIC VOID mProcess():
        
      IF liAckCount > liAckBatch
      THEN mAckReplog().
      
      IF NOW > ldtzNextCheckTime
      THEN mCheckReplog().
      
      IF NOT VALID-OBJECT(loStompConfig)
      THEN mConfig().
      
      IF NOT VALID-OBJECT(loStompClient)
      THEN mConnect().
      
      /* IMPORTANT: This will allow events to be triggered, otherwise 
         READ-RESPONSE wont be triggered and no processing. */
      IF NOT VALID-HANDLE(lhWaitForProcedure)
      THEN RUN Gwy/waitfor.p PERSISTENT SET lhWaitForProcedure.
      
      RUN runWaitForStatement IN lhWaitForProcedure.  
      
      IF llDisconnected
      THEN DO:
         mLogMsg("Socket disconnected, retry after 10 seconds","ERROR").
         PAUSE 10 NO-MESSAGE.
         llDisconnected = FALSE.
      END.
      
      CATCH errorobj AS Progress.Lang.Error:

         DEFINE VARIABLE lii AS INTEGER NO-UNDO.
         
         DO lii = 1 TO errorobj:NumMessages:
            mLogMsg(errorobj:GetMessage(lii), "ERROR").    
         END.
         
         RETURN ERROR NEW Progress.Lang.AppError ("An error encountered", 550).

      END CATCH.
            
   END METHOD.


   METHOD PUBLIC VOID newStompMessage
      ( INPUT ioStompMessage AS StompMessage ):
         
      DEFINE VARIABLE lcHeader AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liCnt    AS INTEGER  NO-UNDO.
      DEFINE VARIABLE llACK    AS LOGICAL   INITIAL TRUE NO-UNDO.

      IF ioStompMessage:Body = ""
      THEN DO:
         mLogMsg("Empty Body", "INFO").
         RETURN.
      END. 

      IF ioStompMessage:Body BEGINS "SocketDisconnected"
      THEN DO:
         llDisconnected = TRUE.
         RETURN.
      END.
   
      lcHeader = REPLACE(ioStompMessage:HEADER,"~n"," ").
      
      IF ENTRY(1,lcHeader," ") = "MESSAGE"
      THEN DO:
         /* Lets store the act to temp-table */
         DATASET ldsAckDS:READ-JSON("LONGCHAR", SUBSTITUTE(lcJSON,ioStompMessage:Body), "APPEND").
         liAckCount = liAckCount + 1.
      END.
      ELSE mLogMsg(lcHeader, "INFO").

      FINALLY:
         /*ack only in case of properly dequeued message*/
         IF NOT llDisconnected THEN loStompClient:ACK(ioStompMessage).
         IF VALID-OBJECT(ioStompMessage) THEN DELETE OBJECT ioStompMessage.                             
      END FINALLY.
         
   END METHOD.


   METHOD PRIVATE VOID mLogLocked():

      mlogMsg("There were locked replog records", "INFO").

      FOR
         EACH ttResponse
         BREAK
            BY ttResponse.DBId:
            
         IF FIRST-OF(ttResponse.DBId)
         THEN OUTPUT STREAM outstr TO
               VALUE(lcExportDir + "/locked_replog_" + ttResponse.DBId + "_" +
               STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") +
               STRING(YEAR(TODAY),"9999") + ".log") APPEND.
      
         PUT STREAM outstr UNFORMATTED
            ttResponse.DBId " " ttResponse.KeyValue " " ttResponse.EventTime SKIP.
      
         IF LAST-OF(ttResponse.DBId)
         THEN OUTPUT STREAM outstr CLOSE.
      
      END.
      
      FINALLY:
      
         OUTPUT STREAM outstr CLOSE.
         
      END FINALLY.
      
   END METHOD.

   /* This is overrided in subclasses, but the logic in here is also used */
   METHOD PROTECTED VOID mAckReplog():

      /* We assume that subclass overriden method is doing its job
         before executing following lines. */

      /* If there are still locked replogs we will resend
         replogs for them again eventually */
      IF liAckCount > 0 OR
         CAN-FIND(FIRST ttResponse)
      THEN mLogLocked().
      
      FINALLY:
         liAckCount = 0.
         DATASET ldsAckDS:EMPTY-DATASET().   
      END FINALLY.
      
   END METHOD.
   
   /* This is overrided in subclasses, but the logic in here is also used */
   METHOD PROTECTED VOID mCheckReplog().
   
      /* We assume that subclass overriden method is doing its job
         before executing following line. */

      ldtzNextCheckTime = ADD-INTERVAL(ldtzNextCheckTime,liCheckWaitMinutes,"minutes").
   
   END METHOD.
    
END CLASS.