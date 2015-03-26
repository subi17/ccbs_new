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

&SCOPED-DEFINE queue1Name 'SampleQ1':U
&SCOPED-DEFINE queue2Name 'SampleQ2':U
&SCOPED-DEFINE topicName 'testtopic':U
 
{javax/jms/Session.i}

DEFINE VARIABLE factory        AS ConnectionFactory NO-UNDO.
DEFINE VARIABLE connection     AS Connection        NO-UNDO.
DEFINE VARIABLE sendSession    AS Session           NO-UNDO.
DEFINE VARIABLE receiveSession AS Session           NO-UNDO.
DEFINE VARIABLE sendQueue1     AS Queue             NO-UNDO.
DEFINE VARIABLE sendQueue2     AS Queue             NO-UNDO.
DEFINE VARIABLE receiveQueue1  AS Queue             NO-UNDO.
DEFINE VARIABLE receiveQueue2  AS Queue             NO-UNDO.
DEFINE VARIABLE sendTopic      AS Topic             NO-UNDO.
DEFINE VARIABLE receiveTopic   AS Topic             NO-UNDO.
DEFINE VARIABLE publisher      AS MessageProducer   NO-UNDO.
DEFINE VARIABLE subscriber     AS MessageConsumer   NO-UNDO.
DEFINE VARIABLE sender         AS MessageProducer   NO-UNDO.
DEFINE VARIABLE receiver1      AS MessageConsumer   NO-UNDO.
DEFINE VARIABLE receiver2      AS MessageConsumer   NO-UNDO.


PROCEDURE testQueues:
  DEFINE VARIABLE data1          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE data2          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE actual         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE sendMessage    AS TextMessage NO-UNDO.
  DEFINE VARIABLE receiveMessage AS TextMessage NO-UNDO.
  
  data1 = 'First message Q1'.
  sendMessage = sendSession:createTextMessage(data1).
  sender:send(sendQueue1, sendMessage).
  DELETE OBJECT sendMessage.

  data2 = 'Second message Q2'.
  sendMessage = sendSession:createTextMessage(data2).
  sender:send(sendQueue2, sendMessage).
  DELETE OBJECT sendMessage.

  receiveMessage = CAST(receiver2:receive(), javax.jms.TextMessage).
  receiveMessage:acknowledge().
  actual = STRING(receiveMessage:text).
  DELETE OBJECT receiveMessage.
  Assert:assertEquals(data2, actual).
  
  receiveMessage = CAST(receiver1:receive(), javax.jms.TextMessage).
  receiveMessage:acknowledge().
  actual = STRING(receiveMessage:text).
  DELETE OBJECT receiveMessage.
  Assert:assertEquals(data1, actual).
END PROCEDURE.


PROCEDURE testMixed:
  DEFINE VARIABLE data1          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE data2          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE actual         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE sendMessage    AS TextMessage NO-UNDO.
  DEFINE VARIABLE receiveMessage AS TextMessage NO-UNDO.
  
  data1 = 'First message Q1'.
  sendMessage = sendSession:createTextMessage(data1).
  sender:send(sendQueue1, sendMessage).
  DELETE OBJECT sendMessage.

  data2 = 'Second message T'.
  sendMessage = sendSession:createTextMessage(data2).
  publisher:send(sendMessage).
  DELETE OBJECT sendMessage.

  receiveMessage = CAST(subscriber:receive(), javax.jms.TextMessage).
  receiveMessage:acknowledge().
  actual = STRING(receiveMessage:text).
  DELETE OBJECT receiveMessage.
  Assert:assertEquals(data2, actual).
  
  receiveMessage = CAST(receiver1:receive(), javax.jms.TextMessage).
  receiveMessage:acknowledge().
  actual = STRING(receiveMessage:text).
  DELETE OBJECT receiveMessage.
  Assert:assertEquals(data1, actual).
END PROCEDURE.


PROCEDURE setUp:
  receiveSession = connection:createSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  receiveQueue1 = receiveSession:createQueue({&queue1Name}).
  receiveQueue2 = receiveSession:createQueue({&queue2Name}).
  receiveTopic = receiveSession:createTopic({&topicName}).
  receiver1 = receiveSession:createConsumer(receiveQueue1).
  receiver2 = receiveSession:createConsumer(receiveQueue2).
  subscriber = receiveSession:createConsumer(receiveTopic). 
  sendSession = connection:createSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  sendQueue1 = ?.
  sender = sendSession:createProducer(sendQueue1).
  sendQueue1 = sendSession:createQueue({&queue1Name}).
  sendQueue2 = sendSession:createQueue({&queue2Name}).
  sendTopic = sendSession:createTopic({&topicName}).
  publisher = sendSession:createProducer(sendTopic).
END PROCEDURE.


PROCEDURE tearDown:
  IF VALID-OBJECT (sendSession) THEN 
    sendSession:close().
  IF VALID-OBJECT (receiveSession) THEN 
    receiveSession:close().
  DELETE OBJECT sender NO-ERROR.
  DELETE OBJECT receiver1 NO-ERROR.
  DELETE OBJECT receiver2 NO-ERROR.
  DELETE OBJECT publisher NO-ERROR.
  DELETE OBJECT sendQueue1 NO-ERROR.
  DELETE OBJECT sendQueue2 NO-ERROR.
  DELETE OBJECT sendTopic NO-ERROR.
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
