/* Sends messages to the ESB using the STOMProtocol */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Syst.Parameters.

CLASS Gwy.MqPublisher:

   DEFINE PUBLIC VARIABLE gcBrand          AS CHARACTER   NO-UNDO INITIAL "1".
   DEFINE PUBLIC VARIABLE lcHostName       AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE liPort           AS INTEGER     NO-UNDO.
   DEFINE PUBLIC VARIABLE liTimeOut        AS INTEGER     NO-UNDO.
   DEFINE PUBLIC VARIABLE liSessionTime    AS INTEGER     NO-UNDO.
   DEFINE PUBLIC VARIABLE lcQueueName      AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE lcUserName       AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE lcPassword       AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE lcHeader         AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE lcMessage        AS LONGCHAR    NO-UNDO.
   DEFINE PUBLIC VARIABLE lmpMessage       AS MEMPTR      NO-UNDO.

   
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

   END CONSTRUCTOR.
   

   CONSTRUCTOR PUBLIC MqPublisher ( icConfigurationPrefix AS CHARACTER ):
      
      ASSIGN
         lcHostName  = Parameters:getc(icConfigurationPrefix + ".ip")
         liPort      = Parameters:geti(icConfigurationPrefix + ".Port")
         liTimeOut   = Parameters:geti(icConfigurationPrefix + ".TimeOut")
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
      
   END CONSTRUCTOR.


   METHOD PUBLIC LOGICAL mReConnect():

      RETURN TRUE.

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

      lcMessage = iclMessage.

   END METHOD.
   
   
   METHOD PUBLIC LOGICAL send_message
      ( iclMessage    AS LONGCHAR,
        icStompHeader AS CHARACTER ):

      ASSIGN
         lcMessage = iclMessage
         lcHeader  = icStompHeader
         .

   END METHOD.

   
   METHOD PUBLIC LOG send_message
      ( impMessage AS MEMPTR ):

      lmpMessage = impMessage.
   
   END METHOD.

END CLASS.
