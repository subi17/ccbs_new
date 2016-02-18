/* Sends messages to the ESB using the STOMProtocol */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Syst.Parameters.
USING javax.jms.Connection.
USING nl.flusso.stomp.Session.
USING javax.jms.MessageProducer.
USING nl.flusso.stomp.ConnectionFactory.
USING nl.flusso.stomp.Queue.
USING nl.flusso.stomp.TextMessage.
USING nl.flusso.stomp.BytesMessage.
USING nl.flusso.util.TimeOutException.
{javax/jms/Session.i}

CLASS Gwy.MqPublisher:

   DEFINE PRIVATE VARIABLE objSession           AS CLASS Session           NO-UNDO.
   DEFINE PRIVATE VARIABLE objConnection        AS CLASS Connection        NO-UNDO.
   DEFINE PRIVATE VARIABLE objMessageProducer   AS CLASS MessageProducer   NO-UNDO.
   DEFINE PRIVATE VARIABLE objConnectionFactory AS CLASS ConnectionFactory NO-UNDO.
   DEFINE PRIVATE VARIABLE objQueue             AS CLASS Queue             NO-UNDO.

   DEFINE PRIVATE VARIABLE gcBrand          AS CHARACTER   NO-UNDO INITIAL "1".
   DEFINE PRIVATE VARIABLE lcHostName       AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE liPort           AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE liTimeOut        AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE liSessionTime    AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE lcQueueName      AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcUserName       AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcPassword       AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE ldttzLastConnect AS DATETIME-TZ NO-UNDO.
   
   CONSTRUCTOR PUBLIC MqPublisher
      ( icHost AS CHARACTER,
        iiPort AS INTEGER,
        iiTimeOut AS INTEGER,
        icQueue AS CHARACTER,
        icUserName AS CHARACTER,
        icPassword AS CHARACTER ):
      
      DO ON ERROR UNDO, THROW:
         liSessionTime = Syst.Parameters:geti("MqPublisherSessionTime").
         CATCH anyErrorObject AS Progress.Lang.Error:
            liSessionTime = 30.
         END CATCH.
      END.

      ASSIGN
         lcHostName  = icHost
         liPort      = iiPort
         liTimeOut   = iiTimeOut
         lcQueueName = icQueue
         lcUserName  = icUserName
         lcPassword  = icPassword.
         
      mInitialize().

   END CONSTRUCTOR.
   

   CONSTRUCTOR PUBLIC MqPublisher ( icConfigurationPrefix AS CHARACTER ):
      
      ASSIGN
         lcHostName  = Parameters:getc(icConfigurationPrefix + ".ip")
         liPort      = Parameters:geti(icConfigurationPrefix + ".liPort")
         liTimeOut   = Parameters:geti(icConfigurationPrefix + ".liTimeOut")
         lcQueueName = Parameters:getc(icConfigurationPrefix + ".Name")
         .
      
      DO ON ERROR UNDO, THROW:
         liSessionTime = Syst.Parameters:geti("MqPublisherSessionTime").
         CATCH anyErrorObject AS Progress.Lang.Error:
            liSessionTime = 30.
         END CATCH.
      END.

      DO ON ERROR UNDO, THROW:
         lcUserName = Syst.Parameters:getc(icConfigurationPrefix + ".UserName").
         CATCH anyErrorObject AS Progress.Lang.Error:
            lcUserName = ?.
         END CATCH.
      END.   
         
      DO ON ERROR UNDO, THROW:
         lcPassword = Syst.Parameters:getc(icConfigurationPrefix + ".Password").
         CATCH anyErrorObject AS Progress.Lang.Error:
            lcPassword = ?.
         END CATCH.
      END.   

      mCreateLog(1, SUBSTITUTE("Prefix=&1",icConfigurationPrefix), "INIT").
      mCreateLog(1, SUBSTITUTE("IP=&1",lcHostName), "INIT").
      mCreateLog(1, SUBSTITUTE("Port=&1",liPort), "INIT").
      mCreateLog(1, SUBSTITUTE("Name=&1",lcQueueName), "INIT").
      mCreateLog(1, SUBSTITUTE("TimeOut=&1",liTimeOut), "INIT").

      mInitialize().
      
   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC MqPublisher ():       

      cleanup().

      IF VALID-OBJECT(objConnection)
      THEN DELETE OBJECT objConnection NO-ERROR.

      IF VALID-OBJECT(objConnectionFactory)
      THEN DELETE OBJECT objConnectionFactory NO-ERROR.

   END DESTRUCTOR.


   METHOD PRIVATE VOID mInitialize():
      
      /* These objects needs to be initialized only once */
      ASSIGN
         objConnectionFactory = NEW ConnectionFactory(lcHostName, liPort)
         objConnection        = objConnectionFactory:createConnection(lcUserName, lcPassword).
      
   END METHOD.
   

   METHOD PRIVATE VOID cleanup ():

      IF VALID-OBJECT(objMessageProducer)
      THEN DELETE OBJECT objMessageProducer NO-ERROR.

      IF VALID-OBJECT(objQueue)
      THEN DELETE OBJECT objQueue NO-ERROR.

      IF VALID-OBJECT(objSession)
      THEN DELETE OBJECT objSession NO-ERROR.

   END METHOD.   


   METHOD PUBLIC LOGICAL mReConnect():

     DEFINE VARIABLE lii AS INTEGER NO-UNDO.

     DO lii = 1 TO 6:

        IF mConnectAndValidate()
        THEN RETURN TRUE.

        PAUSE 10 NO-MESSAGE.

     END.

     RETURN FALSE.

   END METHOD.


   METHOD PRIVATE LOGICAL mConnectAndValidate():
      
      IF VALID-OBJECT(objSession) AND
         objSession:writable      AND
         ( IF liSessionTime = 0 OR liSessionTime = ?
           THEN ldttzLastConnect > ADD-INTERVAL(NOW, liSessionTime * (-1), "minutes")
           ELSE TRUE )
      THEN RETURN TRUE.

      cleanup().

      mCreateLog(3, SUBSTITUTE("Connecting to &1:&2/queue/&3",
                               lcHostName, liPort, lcQueueName), "DEBUG").

      ASSIGN
         /* connection:createSession will throw an error if an error happened */
         objSession         = CAST(objConnection:createSession(?,
                              {&CLIENT_ACKNOWLEDGE}, liTimeOut), Session)
         objQueue           = CAST(objSession:createQueue(lcQueueName), Queue)
         objMessageProducer = objSession:createProducer(objQueue).

      IF NOT VALID-OBJECT(objSession) OR NOT objSession:writable
      THEN DO:
         mCreateLog(3, "Failed to connect", "ERROR").
         RETURN FALSE.
      END.

      ldttzLastConnect = NOW.

      mCreateLog(3, "Connected", "DEBUG").

      RETURN TRUE.

      CATCH errorobj AS Progress.Lang.Error:

         DEFINE VARIABLE lii AS INTEGER NO-UNDO.

         DO lii = 1 TO errorobj:NumMessages:
            mCreateLog(3, errorobj:GetMessage(lii), "ERROR").
         END.

         RETURN FALSE.

      END CATCH.

   END METHOD.


   METHOD PRIVATE VOID mCreateLog
      ( ilLogLevel AS INTEGER,
        iMessage   AS CHARACTER,
        iLogType   AS CHARACTER ):

      IF LOG-MANAGER:LOGFILE-NAME <> ? AND
         LOG-MANAGER:LOGGING-LEVEL GE ilLogLevel
      THEN LOG-MANAGER:WRITE-MESSAGE(iMessage, iLogType).

   END METHOD.


   METHOD PUBLIC LOGICAL send_message
      ( iclMessage AS LONGCHAR ):

      DEFINE VARIABLE objTextMessage AS CLASS TextMessage NO-UNDO.

      IF SESSION:CHARSET NE "utf-8"
      THEN iclMessage = CODEPAGE-CONVERT(iclMessage, "utf-8", SESSION:CHARSET).

      IF NOT mConnectAndValidate()
      THEN RETURN FALSE.

      mCreateLog(4, SUBSTITUTE("Sending message: &1...", iclMessage), "DEBUG").
      
      objTextMessage = CAST(objSession:createTextMessage(iclMessage), TextMessage).
      objTextMessage:JMSDeliveryMode = "true".
      objMessageProducer:sendMessage(objTextMessage).
      
      IF NOT VALID-OBJECT(objSession) OR NOT objSession:writable
      THEN RETURN FALSE.
      
      RETURN TRUE.
        
      FINALLY:
         IF VALID-OBJECT(objTextMessage) THEN DELETE OBJECT objTextMessage NO-ERROR. 
      END FINALLY.
                
   END METHOD.
   
   
   METHOD PUBLIC LOGICAL send_message
      ( iclMessage    AS LONGCHAR,
        icStompHeader AS CHARACTER ):

      DEFINE VARIABLE objTextMessage AS CLASS     TextMessage NO-UNDO.
      DEFINE VARIABLE lcKey          AS CHARACTER             NO-UNDO.
      DEFINE VARIABLE lcValue        AS CHARACTER             NO-UNDO.
      DEFINE VARIABLE liHeaderCount  AS INTEGER               NO-UNDO.

      IF SESSION:CHARSET NE "utf-8"
      THEN iclMessage = CODEPAGE-CONVERT(iclMessage, "utf-8", SESSION:CHARSET).

      IF NOT mConnectAndValidate()
      THEN RETURN FALSE.
      
      mCreateLog(4, SUBSTITUTE("Sending message: &1...", iclMessage), "DEBUG").

      objTextMessage = CAST(objSession:createTextMessage(iclMessage), TextMessage).
      
      DO liHeaderCount = 1 TO NUM-ENTRIES(icStompHeader).
         ASSIGN
            lcKey   = ENTRY(1, ENTRY(liHeaderCount, icStompHeader), "|")
            lcValue = ENTRY(2, ENTRY(liHeaderCount, icStompHeader), "|").
         objTextMessage:setStringProperty(lcKey, lcValue).
      END.
      objTextMessage:JMSDeliveryMode = "true".
      objMessageProducer:sendMessage(objTextMessage).

      IF NOT VALID-OBJECT(objSession) OR NOT objSession:writable
      THEN RETURN FALSE.

      RETURN TRUE. 
        
      FINALLY:
         IF VALID-OBJECT(objTextMessage) THEN DELETE OBJECT objTextMessage NO-ERROR. 
      END FINALLY.

   END METHOD.

   
   METHOD PUBLIC LOG send_message
      ( impMessage AS MEMPTR ):
      
      DEFINE VARIABLE objMessage AS CLASS BytesMessage NO-UNDO.
      
      IF NOT mConnectAndValidate()
      THEN RETURN FALSE.
      
      mCreateLog(4, SUBSTITUTE("Sending &1 bytes", STRING(GET-SIZE(impMessage))), "DEBUG").

      objMessage = CAST(objSession:createBytesMessage(impMessage), BytesMessage).
      objMessageProducer:sendMessage(objMessage).

      IF NOT VALID-OBJECT(objSession) OR NOT objSession:writable
      THEN RETURN FALSE.
              
      RETURN TRUE.

      FINALLY:
         IF VALID-OBJECT(objMessage) THEN DELETE OBJECT objMessage NO-ERROR.  
      END FINALLY.
   
   END METHOD.

END CLASS.
