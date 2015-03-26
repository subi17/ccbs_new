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

DEFINE VARIABLE mp           AS MEMPTR                 NO-UNDO.
DEFINE VARIABLE l            AS LONGCHAR               NO-UNDO.
DEFINE VARIABLE factory      AS QueueConnectionFactory NO-UNDO.
DEFINE VARIABLE connection   AS QueueConnection        NO-UNDO.
DEFINE VARIABLE queueSession AS QueueSession           NO-UNDO.
DEFINE VARIABLE queueSender  AS QueueSender            NO-UNDO.
DEFINE VARIABLE queue        AS Queue                  NO-UNDO.
DEFINE VARIABLE greekMessage AS TextMessage            NO-UNDO.

{javax/jms/Session.i}

Logger:debugLogManager().

SET-SIZE(mp) = 12.
PUT-BYTE(mp, 1) = 206.
PUT-BYTE(mp, 2) = 166.
PUT-BYTE(mp, 3) = 206.
PUT-BYTE(mp, 4) = 187.
PUT-BYTE(mp, 5) = 207.
PUT-BYTE(mp, 6) = 133.
PUT-BYTE(mp, 7) = 207.
PUT-BYTE(mp, 8) = 131.
PUT-BYTE(mp, 9) = 207.
PUT-BYTE(mp, 10) = 131.
PUT-BYTE(mp, 11) = 207.
PUT-BYTE(mp, 12) = 137.

FIX-CODEPAGE(l) = 'UTF-8':U.
COPY-LOB FROM mp TO l NO-CONVERT.

factory      = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613).
connection   = factory:createQueueConnection(?, ?).
queueSession = connection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
queue        = queueSession:createQueue('SampleQ1':U).
queueSender  = queueSession:createSender(queue).
greekMessage = queueSession:createTextMessage(l).

queueSender:send(greekMessage).

FINALLY:
  IF VALID-OBJECT(greekMessage) THEN
    DELETE OBJECT greekMessage.
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

