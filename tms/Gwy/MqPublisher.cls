/* Sends messages to the ESB using the STOMProtocol */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

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

   DEF VAR gcBrand AS CHAR NO-UNDO INITIAL "1".

   DEF VAR stompSession AS CLASS Session NO-UNDO.
   DEF VAR connection   AS CLASS Connection NO-UNDO.
   DEF VAR producer     AS CLASS MessageProducer NO-UNDO.
   DEF VAR hostname     AS CHAR  NO-UNDO.
   DEF VAR port         AS INT   NO-UNDO.
   DEF VAR timeout      AS INT   NO-UNDO.
   DEF VAR queueName    AS CHAR  NO-UNDO.
   DEF VAR username     AS CHAR  NO-UNDO.
   DEF VAR password     AS CHAR  NO-UNDO.

   CONSTRUCTOR PUBLIC MqPublisher (icHost AS CHAR, iiPort AS INT,
                                   iiTimeOut AS INT, icQueue AS CHAR,
                                   icUserName AS CHAR, icPassword AS CHAR):
      ASSIGN
         hostname  = icHost
         port      = iiPort
         timeout   = iiTimeOut
         queueName = icQueue
         username  = icUserName
         password  = icPassword.

   END CONSTRUCTOR.

   METHOD PRIVATE VOID connect ():
      DEF VAR factory     AS CLASS ConnectionFactory NO-UNDO.
      DEF VAR destination AS CLASS Queue NO-UNDO.

      IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN
         LOG-MANAGER:WRITE-MESSAGE(SUBST("Connecting to &1:&2/queue/&3",
                                          hostname, port, queueName), "DEBUG").
      ASSIGN
         factory = NEW ConnectionFactory(hostname, port)
         connection = factory:createConnection(username, password)
         stompSession = CAST(connection:createSession(?,
                                    {&CLIENT_ACKNOWLEDGE}, timeout), Session)
         destination = CAST(stompSession:createQueue(queueName), Queue)
         producer = stompSession:createProducer(destination).

      IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN
         LOG-MANAGER:WRITE-MESSAGE("Connected", "DEBUG").
   END METHOD.

   METHOD PUBLIC LOG send_message ( iMessage AS CHAR ):
      DEF VAR lMessage     AS CLASS TextMessage NO-UNDO.
      DEF VAR lcUTFMessage AS CHAR NO-UNDO.

      lcUTFMessage = CODEPAGE-CONVERT(iMessage, "utf-8", SESSION:CHARSET).

      IF NOT VALID-OBJECT(stompSession) OR NOT stompSession:writable THEN
         connect() NO-ERROR.

      IF NOT VALID-OBJECT(stompSession) OR NOT stompSession:writable THEN DO:
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
             LOG-MANAGER:WRITE-MESSAGE("ActiveMQ connection failed","ERROR").
      END.
      ELSE DO:
         IF LOG-MANAGER:LOGGING-LEVEL GE 4 THEN
            LOG-MANAGER:WRITE-MESSAGE(SUBST("Sending message: &1...",
                                      lcUTFMessage), "DEBUG").
         lMessage = CAST(stompSession:createTextMessage(lcUTFMessage),TextMessage).
         lMessage:JMSDeliveryMode = "true".
         producer:sendMessage(lMessage).
         RETURN TRUE.
      END.
      RETURN FALSE.
   END METHOD.

   METHOD PUBLIC LOG send_message (iMessage AS LONGCHAR, iStompHeader AS CHAR):
      DEF VAR lMessage AS CLASS TextMessage NO-UNDO.
      DEF VAR lUTFMessage AS CHAR NO-UNDO.
      DEF VAR lKey AS CHAR NO-UNDO.
      DEF VAR lValue AS CHAR NO-UNDO.
      DEF VAR lHeaderCount AS INT NO-UNDO.
      DEF VAR lCharVarForConverting AS CHAR NO-UNDO.
      DEF VAR lcHeaderKeys AS CHAR NO-UNDO INIT "Table|EventType|RecordId|Identifier|TimeStamp".

      lCharVarForConverting = iMessage.
      lUTFMessage = CODEPAGE-CONVERT(lCharVarForConverting, "utf-8", SESSION:CHARSET).
      IF NOT VALID-OBJECT(stompSession) OR NOT stompSession:writable THEN
         connect().

      IF NOT VALID-OBJECT(stompSession) OR NOT stompSession:writable THEN DO:
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
             LOG-MANAGER:WRITE-MESSAGE("ActiveMQ connection failed","ERROR").
      END.

      IF LOG-MANAGER:LOGGING-LEVEL GE 4 THEN
         LOG-MANAGER:WRITE-MESSAGE(SUBST("Sending message: &1, header: &2",
                                   iMessage, iStompHeader), "DEBUG").
      lMessage = CAST(stompSession:createTextMessage(lUTFMessage), TextMessage).

      REPEAT lHeaderCount = 1 TO NUM-ENTRIES(iStompHeader,"|").
         ASSIGN
            lKey = ENTRY(lHeaderCount, lcHeaderKeys, "|")
            lValue = ENTRY(lHeaderCount, iStompHeader, "|").
         lMessage:setStringProperty(lKey, lValue).
      END.

      lMessage:JMSDeliveryMode = "true".
      producer:sendMessage(lMessage).
   END METHOD.
   
   METHOD PUBLIC LOG send_message ( iMessage AS MEMPTR ):
      DEF VAR lMessage AS CLASS BytesMessage NO-UNDO.
      IF NOT VALID-OBJECT(stompSession) OR NOT stompSession:writable THEN
         connect().
      IF LOG-MANAGER:LOGGING-LEVEL GE 4 THEN
         LOG-MANAGER:WRITE-MESSAGE(SUBST("Sending &1 bytes",
                                    GET-SIZE(iMessage)), "DEBUG").
      lMessage = CAST(stompSession:createBytesMessage(iMessage), BytesMessage).
      producer:sendMessage(lMessage).
   END METHOD.

   DESTRUCTOR PUBLIC MqPublisher ():
      IF VALID-OBJECT(stompSession) THEN
         stompSession:CLOSE().
      IF LOG-MANAGER:LOGGING-LEVEL GE 4 THEN
          LOG-MANAGER:WRITE-MESSAGE("ActiveMQ object destructed","DEBUG").
   END DESTRUCTOR.

END CLASS.
