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

PROCEDURE testQueueSender:
  DEFINE VARIABLE factory      AS QueueConnectionFactory NO-UNDO.
  DEFINE VARIABLE connection   AS QueueConnection        NO-UNDO.
  DEFINE VARIABLE queueSession AS QueueSession           NO-UNDO.
  DEFINE VARIABLE queue        AS Queue                  NO-UNDO.
  DEFINE VARIABLE queueSender  AS QueueSender            NO-UNDO.
  DEFINE VARIABLE myMessage    AS TextMessage            NO-UNDO.
  DEFINE VARIABLE myBytesMessage    AS BytesMessage      NO-UNDO.
  DEFINE VARIABLE bytesMessage AS MEMPTR                 NO-UNDO.
  DEFINE VARIABLE cFile        AS CHARACTER              NO-UNDO.

  factory      = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613).
  connection   = factory:createQueueConnection(?, ?).
  queueSession = connection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  queue        = queueSession:createQueue('SampleQ2':U).
  queueSender  = queueSession:createSender(queue).
  
  cfile =  "C:\progress\oejms\receivedText.txt". 
  COPY-LOB FILE cFile TO OBJECT bytesMessage.
  myBytesMessage = queueSession:createBytesMessage(bytesMessage). 
  
  myBytesMessage:JMSDeliveryMode = "true".
  
  myBytesMessage:setStringProperty("FileName",cFile).
  myBytesMessage:setLongProperty("FileSize",GET-SIZE(bytesMessage)).
  queueSender:send(myBytesMessage).

  FINALLY:
    SET-SIZE(bytesMessage) = 0.
    IF VALID-OBJECT(myBytesMessage) THEN
      DELETE OBJECT myBytesMessage.
    IF VALID-OBJECT(queueSender) THEN
      DELETE OBJECT queueSender.
    IF VALID-OBJECT(queue) THEN
      DELETE OBJECT queue.
    IF VALID-OBJECT(queueSession) THEN
      DELETE OBJECT queueSession.
    IF VALID-OBJECT(connection) THEN
      DELETE OBJECT connection.
    IF VALID-OBJECT(factory) THEN
      DELETE OBJECT factory.
  END FINALLY.
END PROCEDURE.

Logger:debugLogManager().
RUN testQueueSender.
