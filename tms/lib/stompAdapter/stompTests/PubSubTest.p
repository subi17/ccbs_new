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
{javax/jms/Session.i}

DEFINE VARIABLE factory        AS TopicConnectionFactory NO-UNDO.
DEFINE VARIABLE connection     AS TopicConnection        NO-UNDO.
DEFINE VARIABLE sendSession    AS TopicSession           NO-UNDO.
DEFINE VARIABLE receiveSession AS TopicSession           NO-UNDO.
DEFINE VARIABLE receiveTopic   AS Topic                  NO-UNDO.
DEFINE VARIABLE subscriber     AS TopicSubscriber        NO-UNDO.


PROCEDURE testASCII:
  DEFINE VARIABLE data           AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE actual         AS CHARACTER      NO-UNDO.
  DEFINE VARIABLE sendMessage    AS TextMessage    NO-UNDO.
  DEFINE VARIABLE publisher      AS TopicPublisher NO-UNDO.
  DEFINE VARIABLE sendTopic      AS Topic          NO-UNDO.
  DEFINE VARIABLE receiveMessage AS TextMessage    NO-UNDO.
  
  sendTopic = sendSession:createTopic({&topicName}).
  publisher = sendSession:createPublisher(sendTopic).
  data = 'Hello, FUSE':U.
  sendMessage = sendSession:createTextMessage(data).
  publisher:publish(sendMessage).
  receiveMessage = CAST(subscriber:receive(), javax.jms.TextMessage).
  actual = STRING(receiveMessage:text).
  Assert:assertEquals(data, actual).
  DELETE OBJECT publisher.
  DELETE OBJECT sendTopic.
END PROCEDURE.


PROCEDURE testUnspecifiedTopic:
  DEFINE VARIABLE data           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE actual         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE sendMessage    AS TextMessage NO-UNDO.
  DEFINE VARIABLE publisher      AS TopicPublisher NO-UNDO.
  DEFINE VARIABLE publisherTopic AS Topic          NO-UNDO.
  DEFINE VARIABLE sendTopic      AS Topic          NO-UNDO.
  DEFINE VARIABLE receiveMessage AS TextMessage NO-UNDO.
  
  sendTopic = sendSession:createTopic({&topicName}).
  publisher = sendSession:createPublisher(?).
  publisherTopic = publisher:topic.
  Assert:assertNotValidObject(publisherTopic).
  data = 'Hello, FUSE':U.
  sendMessage = sendSession:createTextMessage(data).
  publisher:publish(sendTopic, sendMessage).
  receiveMessage = CAST(subscriber:receive(), javax.jms.TextMessage).
  actual = STRING(receiveMessage:text).
  Assert:assertEquals(data, actual).
  DELETE OBJECT publisher.
  DELETE OBJECT sendTopic.
END PROCEDURE.


PROCEDURE setUp:
  receiveSession = connection:createTopicSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  receiveTopic = receiveSession:createTopic({&topicName}).
  subscriber = receiveSession:createSubscriber(receiveTopic). 
  sendSession = connection:createTopicSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
END PROCEDURE.


PROCEDURE tearDown:
  DELETE OBJECT sendSession NO-ERROR.
  DELETE OBJECT subscriber NO-ERROR.
  DELETE OBJECT receiveTopic NO-ERROR.
  DELETE OBJECT receiveSession NO-ERROR.
END PROCEDURE.


Logger:debugLogManager().
factory = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613).
connection = factory:createTopicConnection(?,?).

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
