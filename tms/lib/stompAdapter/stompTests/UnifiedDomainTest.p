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
USING javax.jms.*.
USING nl.flusso.unit.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

&SCOPED-DEFINE topicName 'testtopic':U
&SCOPED-DEFINE queueName 'SampleQ1':U
 
{javax/jms/Session.i}

DEFINE VARIABLE factory        AS ConnectionFactory NO-UNDO.
DEFINE VARIABLE connection     AS Connection        NO-UNDO.
DEFINE VARIABLE sendSession    AS Session           NO-UNDO.
DEFINE VARIABLE receiveSession AS Session           NO-UNDO.
DEFINE VARIABLE sendQueue      AS Queue             NO-UNDO.
DEFINE VARIABLE receiveQueue   AS Queue             NO-UNDO.
DEFINE VARIABLE receiveTopic   AS Topic             NO-UNDO.
DEFINE VARIABLE subscriber     AS MessageConsumer   NO-UNDO.
DEFINE VARIABLE sender         AS MessageProducer   NO-UNDO.
DEFINE VARIABLE receiver       AS MessageConsumer   NO-UNDO.


PROCEDURE testPtPASCII:
  DEFINE VARIABLE data           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE actual         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE sendMessage    AS TextMessage NO-UNDO.
  DEFINE VARIABLE receiveMessage AS TextMessage NO-UNDO.
  
  data = 'Hello, FUSE':U.
  sendMessage = sendSession:createTextMessage(data).
  sender:send(sendMessage).
  DELETE OBJECT sendMessage.

  receiveMessage = CAST(receiver:receive(), javax.jms.TextMessage).
  receiveMessage:acknowledge().
  actual = STRING(receiveMessage:text).
  DELETE OBJECT receiveMessage.
  Assert:assertEquals(data, actual).
END PROCEDURE.


PROCEDURE testPubSubASCII:
  DEFINE VARIABLE data           AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE actual         AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE sendTopic      AS Topic           NO-UNDO.
  DEFINE VARIABLE publisher      AS MessageProducer NO-UNDO.
  DEFINE VARIABLE sendMessage    AS TextMessage     NO-UNDO.
  DEFINE VARIABLE receiveMessage AS TextMessage     NO-UNDO.
  
  sendTopic = sendSession:createTopic({&topicName}).
  publisher = sendSession:createProducer(sendTopic).
  data = 'Hello, FUSE':U.
  sendMessage = sendSession:createTextMessage(data).
  publisher:send(sendMessage).
  DELETE OBJECT sendMessage.

  receiveMessage = CAST(subscriber:receive(), javax.jms.TextMessage).
  receiveMessage:acknowledge().
  actual = STRING(receiveMessage:text).
  DELETE OBJECT receiveMessage.
  Assert:assertEquals(data, actual).
  DELETE OBJECT publisher.
  DELETE OBJECT sendTopic.
END PROCEDURE.


PROCEDURE testUnspecifiedTopic:
  DEFINE VARIABLE data           AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE actual         AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE sendMessage    AS TextMessage     NO-UNDO.
  DEFINE VARIABLE publisher      AS MessageProducer NO-UNDO.
  DEFINE VARIABLE destination    AS Destination     NO-UNDO.
  DEFINE VARIABLE sendTopic      AS Topic           NO-UNDO.
  DEFINE VARIABLE receiveMessage AS TextMessage     NO-UNDO.

  sendTopic = ?.  
  publisher = sendSession:createProducer(sendTopic).
  destination = publisher:destination.
  Assert:assertNotValidObject(destination).
  data = 'Hello, FUSE':U.
  sendMessage = sendSession:createTextMessage(data).
  sendTopic = sendSession:createTopic({&topicName}).
  publisher:send(sendTopic, sendMessage).

  receiveMessage = CAST(subscriber:receive(), javax.jms.TextMessage).
  actual = STRING(receiveMessage:text).
  Assert:assertEquals(data, actual).
  DELETE OBJECT publisher.
  DELETE OBJECT sendTopic.
END PROCEDURE.


PROCEDURE setUp:
  receiveSession = connection:createSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  receiveQueue = receiveSession:createQueue({&queueName}).
  receiveTopic = receiveSession:createTopic({&topicName}).
  receiver = receiveSession:createConsumer(receiveQueue).
  subscriber = receiveSession:createConsumer(receiveTopic). 
  sendSession = connection:createSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  sendQueue = sendSession:createQueue({&queueName}).
  sender = sendSession:createProducer(sendQueue).
END PROCEDURE.


PROCEDURE tearDown:
  IF VALID-OBJECT (sendSession) THEN 
    sendSession:close().
  IF VALID-OBJECT (receiveSession) THEN 
    receiveSession:close().
  DELETE OBJECT sender NO-ERROR.
  DELETE OBJECT receiver NO-ERROR.
  DELETE OBJECT sendSession NO-ERROR.
  DELETE OBJECT subscriber NO-ERROR.
  DELETE OBJECT receiveTopic NO-ERROR.
  DELETE OBJECT receiveSession NO-ERROR.
END PROCEDURE.


Logger:debugLogManager().
factory = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613).
connection = factory:createConnection(?,?).
RUN nl/flusso/unit/procedureSuite.p (INPUT THIS-PROCEDURE).
RETURN.


CATCH e AS Progress.Lang.Error:  
  MESSAGE e:GetMessage(1) VIEW-AS ALERT-BOX ERROR TITLE 'Uncaught exception':U.		
END CATCH.
FINALLY:
  RUN tearDown.
  DELETE OBJECT factory    NO-ERROR.
  DELETE OBJECT connection NO-ERROR.
END FINALLY.
