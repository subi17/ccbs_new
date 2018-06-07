USING Progress.Lang.*.
USING dotr.Stomp.*.
USING dotr.Stomp.Interface.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDAckReader ABSTRACT
   IMPLEMENTS dotr.Stomp.Interface.IStompHandler:

   DEFINE PRIVATE VARIABLE lcHostName             AS CHARACTER      NO-UNDO.
   DEFINE PRIVATE VARIABLE liPortNumber           AS INTEGER        NO-UNDO.
   DEFINE PRIVATE VARIABLE lcQueueName            AS CHARACTER      NO-UNDO.
   DEFINE PRIVATE VARIABLE lcUserName             AS CHARACTER      NO-UNDO.
   DEFINE PRIVATE VARIABLE lcPassword             AS CHARACTER      NO-UNDO.

   DEFINE PRIVATE VARIABLE loStompConfig          AS StompConfig    NO-UNDO.
   DEFINE PRIVATE VARIABLE loStompClient          AS StompClient    NO-UNDO.

   DEFINE PRIVATE VARIABLE lhWaitForProcedure     AS HANDLE         NO-UNDO.
   DEFINE PRIVATE VARIABLE llDisconnected AS LOGICAL INITIAL FALSE  NO-UNDO.
   DEFINE PRIVATE VARIABLE liAckBatch             AS INTEGER        NO-UNDO.
   DEFINE PRIVATE VARIABLE liAckTimeLimit         AS INTEGER        NO-UNDO.
   DEFINE PRIVATE VARIABLE liLastAck              AS INTEGER        NO-UNDO.
   DEFINE PRIVATE VARIABLE lcExportDir            AS CHARACTER      NO-UNDO.
   DEFINE PRIVATE VARIABLE lhSocketHandle         AS HANDLE         NO-UNDO.

   DEFINE PROTECTED VARIABLE liAckCount           AS INTEGER        NO-UNDO.
   DEFINE PROTECTED VARIABLE lcJSON AS LONGCHAR INITIAL '~{"":[&1]~}' NO-UNDO.
   DEFINE PROTECTED VARIABLE ldtzNextCheckTime    AS DATETIME-TZ    NO-UNDO.
   DEFINE PROTECTED VARIABLE liCheckWaitMinutes   AS INTEGER        NO-UNDO.
   DEFINE PROTECTED VARIABLE liMaxLockedLoopCount AS INTEGER        NO-UNDO.
   
   DEFINE PROTECTED TEMP-TABLE ttResponse NO-UNDO SERIALIZE-NAME ""
      FIELD DBId      AS CHARACTER   SERIALIZE-NAME "dbid"
      FIELD KeyValue  AS CHARACTER   SERIALIZE-NAME "keyid"
      FIELD EventTime AS DATETIME-TZ SERIALIZE-NAME "time"
      FIELD ErrorCode AS INTEGER     SERIALIZE-NAME "errorcode"
      FIELD ErrorText AS CHARACTER   SERIALIZE-NAME "errormessage"
      INDEX DBid IS PRIMARY DBId
      .

   DEFINE PROTECTED DATASET ldsAckDS SERIALIZE-HIDDEN FOR ttResponse.

   DEFINE PRIVATE STREAM outstr.


   CONSTRUCTOR PROTECTED HPDAckReader (icDB AS CHARACTER):

      DO ON ERROR UNDO, THROW:
         lcUserName = Syst.Parameters:getc("HPD.Ack.UserName", "HPD.Interface").
         CATCH anyErrorObject AS Progress.Lang.Error:
            lcUserName = ?.
         END CATCH.
      END.

      DO ON ERROR UNDO, THROW:
         lcPassword = Syst.Parameters:getc("HPD.Ack.Password", "HPD.Interface").
         CATCH anyErrorObject AS Progress.Lang.Error:
            lcPassword = ?.
         END CATCH.
      END.

      DO ON ERROR UNDO, THROW:
         liAckTimeLimit = Syst.Parameters:geti("HPD.Ack.AckTimeLimit", "HPD.Interface").
         CATCH anyErrorObject AS Progress.Lang.Error:
            liAckTimeLimit = 10.
         END CATCH.
      END.

      ASSIGN
         lcHostName           = Syst.Parameters:getc("HPD.Ack.HostName", "HPD.Interface")
         liPortNumber         = Syst.Parameters:geti("HPD.Ack.PortNumber", "HPD.Interface")
         lcQueueName          = Syst.Parameters:getc("HPD.Ack.Queue" + icDB, "HPD.Interface")
         liAckBatch           = Syst.Parameters:geti("HPD.Ack.BatchSize", "HPD.Interface")
         liCheckWaitMinutes   = Syst.Parameters:geti("HPD.Ack.CheckWaitMin", "HPD.Interface")
         liMaxLockedLoopCount = Syst.Parameters:geti("HPD.Ack.MaxLockedLoop", "HPD.Interface")
         lcExportDir          = Syst.Parameters:getc("HPD.Ack.ExportDir", "HPD.Interface")
         ldtzNextCheckTime    = NOW
         .

      mlogMsg(SUBSTITUTE("SETTINGS: hostname=&1, port=&2, queueName=&3",
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
      
      IF VALID-HANDLE(lhWaitForProcedure)
      THEN DELETE OBJECT lhWaitForProcedure.
      
      mDisConnect().
      
      IF VALID-OBJECT(loStompConfig)
      THEN DELETE OBJECT loStompConfig.
      
      IF VALID-OBJECT(dotr.Stomp.StompConfig:DEFAULT)
      THEN DELETE OBJECT dotr.Stomp.StompConfig:DEFAULT.

   END METHOD.


   METHOD PRIVATE VOID mCreateLog
      ( ilLogLevel AS INTEGER,
        iMessage   AS CHARACTER,
        iLogType   AS CHARACTER ):

      IF LOG-MANAGER:LOGFILE-NAME <> ? AND
         LOG-MANAGER:LOGGING-LEVEL GE ilLogLevel
      THEN LOG-MANAGER:WRITE-MESSAGE(iMessage, iLogType).

   END METHOD.


   METHOD PRIVATE VOID mConfig ():

      loStompConfig = dotr.Stomp.StompConfig:DEFAULT.

      ASSIGN
         loStompConfig:StompServer         = lcHostName
         loStompConfig:StompPort           = STRING(liPortNumber)
         loStompConfig:login               = lcUserName
         loStompConfig:passcode            = lcPassword
         loStompConfig:LargeMessageSupport = NO
         .

   END METHOD.


   METHOD PRIVATE VOID mDisConnect():

      lhSocketHandle = ?.

      IF VALID-OBJECT(loStompClient)
      THEN DO:
         loStompClient:UnSubscribeFromQueue(lcQueueName) NO-ERROR.
         DELETE OBJECT loStompClient.
      END.

   END METHOD.


   METHOD PRIVATE VOID mConnect():
      
      loStompClient = NEW dotr.Stomp.StompClient(loStompConfig).      
      loStompClient:SUBSCRIBE(THIS-OBJECT).
      loStompClient:subscribeToQueueACK(lcQueueName,"client-individual").
      lhSocketHandle = loStompClient:SocketHandle.

   END METHOD.


   METHOD PRIVATE LOGICAL mAckTimePassed():

      DEFINE VARIABLE liNow AS INTEGER NO-UNDO.

      liNow = TIME.

      IF liNow < liLastAck
      THEN RETURN (liNow + 86399 - liLastAck) >= liAckTimeLimit.

      RETURN (liNow - liLastAck ) >= liAckTimeLimit.

   END METHOD.


   METHOD PUBLIC VOID mProcess():
        
      IF liAckCount >= liAckBatch OR mAckTimePassed()
      THEN mAckReplog().
      
      IF NOW > ldtzNextCheckTime
      THEN mCheckReplog().
      
      IF NOT VALID-OBJECT(loStompConfig)
      THEN mConfig().
      
      IF NOT VALID-OBJECT(loStompClient)
      THEN mConnect().

      WAIT-FOR READ-RESPONSE OF lhSocketHandle PAUSE 10.

      IF llDisconnected
      THEN DO:
         mDisConnect().
         mLogMsg("Socket disconnected, retry after 1 second","ERROR").
         PAUSE 1 NO-MESSAGE.
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
         IF ioStompMessage:HEADER BEGINS "CONNECTED"
         THEN mLogMsg(ioStompMessage:HEADER, "INFO").
         ELSE mLogMsg("Empty Body", "INFO").
         RETURN.
      END. 

      IF ioStompMessage:Body BEGINS "SocketDisconnected"
      THEN DO:
         llDisconnected = TRUE.
         RETURN.
      END.

      mCreateLog(4, SUBSTITUTE("Got body: &1", ioStompMessage:Body), "DEBUG").
   
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
         ASSIGN
            liLastAck  = TIME
            liAckCount = 0
            .
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
