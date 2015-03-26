/* Copyright (c) 2009 Flusso B.V.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE. 
*/
USING nl.flusso.stomp.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{javax/jms/Session.i}

&SCOPED-DEFINE BUFFER_SIZE 8192

CLASS nl.flusso.stomp.Session IMPLEMENTS javax.jms.Session:
  DEFINE PRIVATE VARIABLE socketHandle  AS HANDLE        NO-UNDO.
  DEFINE PRIVATE VARIABLE receiveBuffer AS ReceiveBuffer NO-UNDO.  
  DEFINE PRIVATE VARIABLE bufferSize    AS INTEGER       NO-UNDO INITIAL {&BUFFER_SIZE}.
  DEFINE PRIVATE VARIABLE bytesBuffer   AS MEMPTR        NO-UNDO.


  DEFINE PRIVATE TEMP-TABLE stompTable NO-UNDO
    FIELD consumerName AS CHARACTER            INITIAL ?
    FIELD timeStamp    AS DATETIME-TZ
    FIELD stompObject  AS Progress.Lang.Object
    INDEX sortNameTime
      consumerName
      timeStamp.
      

  DEFINE PUBLIC PROPERTY acknowledgeMode AS INTEGER NO-UNDO
    GET.
    PRIVATE SET.

  DEFINE PRIVATE VARIABLE liTimeOutInSeconds AS INTEGER NO-UNDO INIT 0.

  DEFINE PUBLIC PROPERTY clientAcknowledge AS LOGICAL
    GET:
      RETURN acknowledgeMode <> {&AUTO_ACKNOWLEDGE}.
    END GET.


  DEFINE PUBLIC PROPERTY writable AS LOGICAL NO-UNDO 
    GET:
      RETURN VALID-HANDLE(socketHandle) AND socketHandle:CONNECTED().
    END GET.
         
  
  CONSTRUCTOR PUBLIC Session(hostName    AS CHARACTER, 
                             port        AS INTEGER, 
                             login       AS CHARACTER, 
                             passcode    AS CHARACTER, 
                             acknowledge AS INTEGER):
                                
    mCreateStompFramesAndLogin(INPUT hostName ,
                               INPUT port     ,
                               INPUT login    ,
                               INPUT passcode , 
                               INPUT acknowledge).
    
    CATCH anyError AS Progress.Lang.Error:
      IF VALID-HANDLE(socketHandle) THEN
        DELETE OBJECT socketHandle.
      UNDO, THROW anyError.
    END CATCH.
  END CONSTRUCTOR.


  CONSTRUCTOR PUBLIC Session(hostName    AS CHARACTER, 
                             port        AS INTEGER, 
                             login       AS CHARACTER, 
                             passcode    AS CHARACTER, 
                             acknowledge AS INTEGER,
                             iiTimeOut   AS INTEGER):
    liTimeOutInSeconds =   iiTimeOut.
    mCreateStompFramesAndLogin(INPUT hostName ,
                               INPUT port     ,
                               INPUT login    ,
                               INPUT passcode , 
                               INPUT acknowledge).
    CATCH anyError AS Progress.Lang.Error:
      IF VALID-HANDLE(socketHandle) THEN
        DELETE OBJECT socketHandle.
      UNDO, THROW anyError.
    END CATCH.
  END CONSTRUCTOR.


  DESTRUCTOR PUBLIC Session():
    close().
    SET-SIZE(bytesBuffer) = 0.
  END DESTRUCTOR.


  METHOD PUBLIC VOID acknowledge(jmsMessage AS nl.flusso.stomp.Message):
    DEFINE VARIABLE acknowledgeFrame AS StompFrame NO-UNDO.
    IF clientAcknowledge THEN
    DO:
      acknowledgeFrame = NEW stompFrame('ACK':u).
      acknowledgeFrame:assignHeaderValue('message-id':U, jmsMessage:JMSMessageID).
      sendFrame(acknowledgeFrame).
    END.
  END METHOD.
  
  METHOD PRIVATE VOID mLogin(INPUT icHostName AS CHARACTER ,
                             INPUT icPort     AS CHARACTER ):
     
     CREATE SOCKET socketHandle.
     socketHandle:CONNECT(SUBSTITUTE('-H &1 -S &2':U, icHostName , icPort )).
     IF liTimeOutInSeconds NE 0 THEN
        socketHandle:SET-SOCKET-OPTION("SO-RCVTIMEO", 
                                       STRING(liTimeOutInSeconds)).
  
  END METHOD.
  
  METHOD PRIVATE VOID mCreateStompFramesAndLogin
      (hostName    AS CHARACTER, 
       port        AS INTEGER, 
       login       AS CHARACTER, 
       passcode    AS CHARACTER, 
       acknowledge AS INTEGER) :
    
    DEFINE VARIABLE frameToSend    AS StompFrame NO-UNDO.
    DEFINE VARIABLE frameToReceive AS StompFrame NO-UNDO.
    
    acknowledgeMode = acknowledge.
    SET-SIZE(bytesBuffer) = bufferSize.
    receiveBuffer = NEW ReceiveBuffer().
    
    frameToSend = NEW StompFrame('CONNECT':u).
    IF login <> ? THEN
      frameToSend:assignHeaderValue('login':U, login).
    IF passcode <> ? THEN
      frameToSend:assignHeaderValue('passcode':U, passcode).
    mLogin(hostName , STRING(port) ).
    sendFrame(frameToSend).
    frameToReceive = receiveFrame(?).
     
  END METHOD.

  METHOD PUBLIC VOID close():
    IF writable THEN
    DO:
      sendFrame(NEW StompFrame('DISCONNECT':U)).
      socketHandle:DISCONNECT().
    END.

    FINALLY:
      IF VALID-HANDLE(socketHandle) THEN 
         DELETE OBJECT socketHandle NO-ERROR.
    END FINALLY.
  END METHOD.
  
  
  METHOD PUBLIC javax.jms.MessageConsumer createConsumer(destination AS javax.jms.Destination):
    DEFINE VARIABLE jmsQueue AS nl.flusso.stomp.Queue           NO-UNDO.
    DEFINE VARIABLE jmsTopic AS nl.flusso.stomp.Topic           NO-UNDO.
    DEFINE VARIABLE consumer AS javax.jms.MessageConsumer NO-UNDO.
    
    jmsQueue = CAST(destination, nl.flusso.stomp.Queue) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      consumer = NEW QueueReceiver(THIS-OBJECT, jmsQueue).
    ELSE
    DO:
      jmsTopic = CAST(destination, nl.flusso.stomp.Topic).
      consumer = NEW TopicSubscriber(THIS-OBJECT, jmsTopic).
    END.  
    RETURN consumer. 
  END METHOD.
  
  
  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC javax.jms.MessageConsumer createConsumer(queue AS nl.flusso.stomp.Queue):
    RETURN NEW QueueReceiver(THIS-OBJECT, CAST(queue, nl.flusso.stomp.Queue)).
  END METHOD.
  
  
  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC javax.jms.MessageConsumer createConsumer(topic AS nl.flusso.stomp.Topic):
    RETURN NEW TopicSubscriber(THIS-OBJECT, CAST(topic, nl.flusso.stomp.Topic) ).
  END METHOD.
  
  
  METHOD PUBLIC javax.jms.MessageProducer createProducer(destination AS javax.jms.Destination):
    DEFINE VARIABLE jmsQueue AS nl.flusso.stomp.Queue           NO-UNDO.
    DEFINE VARIABLE jmsTopic AS nl.flusso.stomp.Topic           NO-UNDO.
    DEFINE VARIABLE producer AS javax.jms.MessageProducer NO-UNDO.
    
    jmsQueue = CAST(destination, nl.flusso.stomp.Queue) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      producer = NEW QueueSender(THIS-OBJECT, jmsQueue).
    ELSE
    DO:
      jmsTopic = CAST(destination, nl.flusso.stomp.Topic).
      producer = NEW TopicPublisher(THIS-OBJECT, jmsTopic).
    END.  
    RETURN producer. 
  END METHOD.
  
  
  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC javax.jms.MessageProducer createProducer(jmsQueue AS nl.flusso.stomp.Queue):
    RETURN NEW QueueSender(THIS-OBJECT, cast(jmsQueue,nl.flusso.stomp.Queue) ).
  END METHOD.
  
  
  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC javax.jms.MessageProducer createProducer(jmsTopic AS nl.flusso.stomp.Topic):
    RETURN NEW TopicPublisher(THIS-OBJECT, cast(jmsTopic,nl.flusso.stomp.Topic) ).
  END METHOD.


  METHOD PUBLIC javax.jms.Queue createQueue(queueName AS CHARACTER):
    RETURN NEW Queue(queueName).
  END METHOD.


  METHOD PUBLIC javax.jms.TextMessage createTextMessage():
    RETURN NEW TextMessage().
  END METHOD.

  METHOD PUBLIC javax.jms.BytesMessage createBytesMessage():
    RETURN NEW BytesMessage().
  END METHOD.

  METHOD PUBLIC javax.jms.Topic createTopic(topicName AS CHARACTER):
    RETURN NEW Topic(topicName).
  END METHOD.


  METHOD PUBLIC javax.jms.TextMessage createTextMessage(textString AS LONGCHAR):
    DEFINE VARIABLE textMessage AS nl.flusso.stomp.TextMessage NO-UNDO.
    textMessage = NEW TextMessage().
    textMessage:text1 = textString.
    RETURN textMessage.
  END METHOD.

  METHOD PUBLIC javax.jms.BytesMessage createBytesMessage(bodyBytes AS MEMPTR):
    DEFINE VARIABLE BytesMessage AS nl.flusso.stomp.BytesMessage NO-UNDO.
    BytesMessage = NEW BytesMessage().
    BytesMessage:bytes = bodyBytes.
    RETURN BytesMessage.
  END METHOD.

  METHOD PUBLIC nl.flusso.stomp.StompFrame receiveFrame(destinationName AS CHARACTER):
    
    DEFINE BUFFER stompRow FOR stompTable.
    DEFINE VARIABLE stompFrame1 AS nl.flusso.stomp.StompFrame NO-UNDO.
    
    FOR EACH stompRow WHERE stompRow.consumerName = destinationName:
      stompFrame1 = CAST(stompRow.stompObject, nl.flusso.stomp.StompFrame).
      DELETE stompRow.
      LEAVE.
    END.
    
/*    IF stompFrame1 = ? THEN                                                           */
/*      stompFrame1 = CAST(nextStompFrame(destinationName), nl.flusso.stomp.StompFrame).*/
    
    IF stompFrame1 = ? THEN
       StompFrame1 = nextStompFrame(destinationName ) .
    
    RETURN stompFrame1.
  END.
  

  METHOD PUBLIC VOID sendFrame(stompFrame AS StompFrame):
    DEFINE VARIABLE sendBuffer     AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE bytesToWrite   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE bufferPosition AS INTEGER   NO-UNDO.
    DEFINE VARIABLE utf8LogString  AS CHARACTER NO-UNDO.

    sendBuffer = stompFrame:composeFrameBytes().
    bytesToWrite = GET-SIZE(sendBuffer).
    IF bytesToWrite < 8000 THEN
    DO:
      utf8LogString = GET-STRING(sendBuffer, 1).
      Logger:dbg(utf8LogString).
    END.
    bufferPosition = 1.
    DO WHILE bytesToWrite > 0:
      socketHandle:WRITE(sendBuffer, bufferPosition, bytesToWrite).
      bytesToWrite = bytesToWrite - socketHandle:BYTES-WRITTEN.
      bufferPosition = bufferPosition + socketHandle:BYTES-WRITTEN.
    END.
    FINALLY:
      SET-SIZE(sendBuffer) = 0.
    END FINALLY.    
  END METHOD.

  METHOD PUBLIC StompFrame receiveHeader():
    
    DEFINE VARIABLE stompFrame1 AS StompFrame NO-UNDO.
    DEFINE VARIABLE headerText  AS LONGCHAR   NO-UNDO.
    
    headerText = receiveBuffer:extractHeader().
    DO WHILE headerText = ?:
      receivePacket().
      headerText = receiveBuffer:extractHeader().
    END.
    stompFrame1 = NEW StompFrame(headerText).
    
    RETURN stompFrame1.
  END METHOD.


  METHOD PUBLIC StompFrame nextStompFrame(requestedName AS CHARACTER ) :
    DEFINE BUFFER stompRow FOR stompTable.
    DEFINE VARIABLE stompFrameFound AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE stompFrame1     AS StompFrame  .
    DEFINE VARIABLE destinationName AS CHARACTER  NO-UNDO.
    
    DO WHILE stompFrameFound = FALSE:
      
      stompFrame1 = receiveHeader( )  .
      
      IF stompFrame1:binary THEN
        stompFrame1:bytes = receiveBodyBytes(stompFrame1).
      ELSE
        stompFrame1:text1 = receiveBodyText().

      destinationName = stompFrame1:currentHeaderValue('destination':U).

      /* Assume a STOMP frame with a destination is a MESSAGE and skip further tests for frame type. */
      IF destinationName = requestedName THEN
        stompFrameFound = TRUE.
      ELSE
      DO:
        CREATE stompRow.
        ASSIGN
          stompRow.consumerName = destinationName
          stompRow.timeStamp    = NOW
          stompRow.stompObject  = stompFrame1.
      END.
    END.

    RETURN stompFrame1.
  END METHOD.
  
  
  METHOD PRIVATE MEMPTR receiveBodyBytes (FrameToReceive AS stompframe):
    DEFINE VARIABLE bodyBytes   AS MEMPTR     NO-UNDO.
    DEFINE VARIABLE bytesCount  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ReadBuffer  AS MEMPTR     NO-UNDO.
    DEFINE VARIABLE bytesToRead AS INTEGER    NO-UNDO.
    DEFINE VARIABLE AlreadyRead AS INTEGER    NO-UNDO.

    ASSIGN bytesCount  = INTEGER(FrameToReceive:currentHeaderValue('content-length':U))
           bytesToRead = bytesCount
           AlreadyRead = socketHandle:BYTES-READ 
           SET-SIZE(bytesbuffer) = bytesToRead + AlreadyRead  + 1
           SET-SIZE(ReadBuffer)  = bytesToRead + AlreadyRead + 1 
           .
    
    IF AlreadyRead >= bytesToRead THEN
      receiveBuffer:addBytes(ReadBuffer, socketHandle:BYTES-READ).
    ELSE
    DO WHILE bytesToRead > 0 :
      
      socketHandle:READ(ReadBuffer, 1, bytesToRead, READ-AVAILABLE).
      ASSIGN bytesToRead = bytesToRead - socketHandle:BYTES-READ
             AlreadyRead = AlreadyRead + socketHandle:BYTES-READ
             .
      receiveBuffer:addBytes(ReadBuffer, socketHandle:BYTES-READ).
    END.
    
    ASSIGN bodyBytes = receiveBuffer:extractBodyBytes()
           FrameToReceive:bytes = bodyBytes
           .

    RETURN bodyBytes.

    FINALLY:
      SET-SIZE(ReadBuffer) = 0.
    END FINALLY.
        
  END METHOD.

  METHOD PRIVATE LONGCHAR receiveBodyText():
    DEFINE VARIABLE bodyText AS LONGCHAR NO-UNDO.
    bodyText = receiveBuffer:extractBodyText().
    DO WHILE bodyText = ?:
      receivePacket().
      bodyText = receiveBuffer:extractBodyText().
    END.
    RETURN bodyText.
  END METHOD.
  
  METHOD PRIVATE VOID receivePacket():
    DEFINE VARIABLE defa AS CHARACTER NO-UNDO.
    DEFINE VARIABLE IsOk AS LOGICAL   NO-UNDO.
    
    IsOk = socketHandle:READ(bytesBuffer, 1, bufferSize, READ-AVAILABLE).
    IF IsOk AND socketHandle:BYTES-READ = 0 THEN 
       UNDO , THROW NEW nl.flusso.util.TimeOutException("Time out reached!").
    defa = get-string(bytesBuffer,1).
    receiveBuffer:addBytes(bytesBuffer, socketHandle:BYTES-READ).
  END METHOD.

END CLASS.
