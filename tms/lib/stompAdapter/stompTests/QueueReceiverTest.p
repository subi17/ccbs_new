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

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{javax/jms/Session.i}

Logger:debugLogManager().
RUN testReceiveTextMessage (TRUE).

PROCEDURE testReceiveTextMessage:
  DEFINE INPUT PARAMETER clientAcknowledge AS LOGICAL NO-UNDO.

  DEFINE VARIABLE factory         AS QueueConnectionFactory NO-UNDO.
  DEFINE VARIABLE connection      AS QueueConnection        NO-UNDO.
  DEFINE VARIABLE queueSession    AS QueueSession           NO-UNDO.
  DEFINE VARIABLE queue           AS Queue                  NO-UNDO.
  DEFINE VARIABLE queueReceiver   AS QueueReceiver          NO-UNDO.
  DEFINE VARIABLE receivedMessage AS Message                NO-UNDO.
  DEFINE VARIABLE textMessage     AS TextMessage            NO-UNDO.
  DEFINE VARIABLE textLength      AS INTEGER                NO-UNDO.
  
  factory    = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613).
  connection = factory:createQueueConnection(?, ?).
  queueSession = connection:createQueueSession(FALSE, IF clientAcknowledge THEN {&CLIENT_ACKNOWLEDGE} ELSE {&AUTO_ACKNOWLEDGE}).
  queue = queueSession:createQueue('SampleQ1':U).
  queueReceiver = queueSession:createReceiver(queue).
  
  receivedMessage = queueReceiver:receive().
  textMessage = CAST(receivedMessage, javax.jms.TextMessage).
  textLength = LENGTH(textMessage:text).
  MESSAGE 'JMSMessageId:' textMessage:JMSMessageId SKIP
          'body contains' textLength 'characters' SKIP
          'text:' (IF textLength < 100 THEN STRING(textMessage:text) ELSE '(too long to display)') SKIP
          'persistent:' textmessage:jmsdeliveryMode SKIP
          'Correlation-id: ' textmessage:JMSCorrelationID
          VIEW-AS ALERT-BOX.  
  IF clientAcknowledge THEN
    textMessage:acknowledge().
  
  queueReceiver:close().
  queueSession:close().    
  RETURN.

  FINALLY:
    DELETE OBJECT receivedMessage NO-ERROR.
    DELETE OBJECT queueReceiver   NO-ERROR.
    DELETE OBJECT queue           NO-ERROR.
    DELETE OBJECT queueSession    NO-ERROR.
    DELETE OBJECT connection      NO-ERROR.
    DELETE OBJECT factory         NO-ERROR.
  END FINALLY.
END PROCEDURE.
