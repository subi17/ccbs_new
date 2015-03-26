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

CLASS nl.flusso.stomp.QueueSession INHERITS Session IMPLEMENTS javax.jms.QueueSession:
  CONSTRUCTOR PUBLIC QueueSession(hostName AS CHARACTER, port AS INTEGER, login AS CHARACTER, passcode AS CHARACTER, acknowledge AS INTEGER):
    SUPER(hostName, port, login, passcode, acknowledge).
  END CONSTRUCTOR.
  
  
  METHOD PUBLIC javax.jms.QueueSender createSender(queue AS javax.jms.Queue):
    RETURN NEW QueueSender(THIS-OBJECT, cast(queue,nl.flusso.stomp.Queue)).
  END METHOD.


  METHOD PUBLIC javax.jms.QueueReceiver createReceiver(queue AS javax.jms.Queue):
    RETURN NEW QueueReceiver(THIS-OBJECT, cast(queue,nl.flusso.stomp.Queue)).
  END METHOD.
END CLASS.
