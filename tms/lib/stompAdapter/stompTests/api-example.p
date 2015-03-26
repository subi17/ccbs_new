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
{javax/jms/Session.i}
USING javax.jms.*.
ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE factory         AS QueueConnectionFactory NO-UNDO.
DEFINE VARIABLE queueConnection AS QueueConnection        NO-UNDO.
DEFINE VARIABLE queueSession    AS QueueSession           NO-UNDO.
DEFINE VARIABLE receiveQueue    AS Queue                  NO-UNDO.
DEFINE VARIABLE queueReceiver   AS QueueReceiver          NO-UNDO.
DEFINE VARIABLE firstMessage    AS Message                NO-UNDO.

factory         = NEW nl.flusso.stomp.ConnectionFactory('localhost':U, 61613).
queueConnection = factory:createQueueConnection('scott':U, 'tiger':U).
queueSession    = queueConnection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
receiveQueue    = queueSession:createQueue('SampleQ1':U).
queueReceiver   = queueSession:createReceiver(receiveQueue).

firstMessage    = queueReceiver:receive().
/* use message ... */
firstMessage:acknowledge().

queueReceiver:close().
queueSession:close().

FINALLY:
  DELETE OBJECT firstMessage    NO-ERROR.
  DELETE OBJECT queueReceiver   NO-ERROR.
  DELETE OBJECT receiveQueue    NO-ERROR.
  DELETE OBJECT queueSession    NO-ERROR.
  DELETE OBJECT queueConnection NO-ERROR.
  DELETE OBJECT factory         NO-ERROR.
END FINALLY.


