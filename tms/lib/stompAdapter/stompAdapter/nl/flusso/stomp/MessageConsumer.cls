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

CLASS nl.flusso.stomp.MessageConsumer IMPLEMENTS javax.jms.MessageConsumer:
  DEFINE PROTECTED VARIABLE thisSession AS Session   NO-UNDO.  
  DEFINE PRIVATE   VARIABLE isOpen      AS LOGICAL   NO-UNDO.


  DEFINE PUBLIC PROPERTY destinationName AS CHARACTER NO-UNDO INITIAL ?
    GET.
    PRIVATE SET.
    

  CONSTRUCTOR PROTECTED MessageConsumer(jmsSession AS Session, destinationType AS CHARACTER, queueOrTopicName AS CHARACTER):
    DEFINE VARIABLE subscribeFrame AS StompFrame NO-UNDO.

    destinationName = SUBSTITUTE('/&1/&2':U, destinationType, queueOrTopicName).
    thisSession = jmsSession.
    
    subscribeFrame = NEW StompFrame('SUBSCRIBE':U).
    subscribeFrame:assignHeaderValue('destination':U, destinationName).
    IF thisSession:clientAcknowledge THEN
      subscribeFrame:assignHeaderValue('ack':U, 'client':U).
    thisSession:sendFrame(subscribeFrame).
    isOpen = TRUE.
  END CONSTRUCTOR.
  

  DESTRUCTOR PUBLIC MessageConsumer():
    close().
  END DESTRUCTOR.  


  METHOD PUBLIC VOID close():
    DEFINE VARIABLE unsubscribeFrame AS StompFrame NO-UNDO.
    
    IF isOpen AND VALID-OBJECT(thisSession) AND thisSession:writable THEN
    DO:
      unsubscribeFrame = NEW stompFrame('UNSUBSCRIBE':U).
      unsubscribeFrame:assignHeaderValue('destination':U, destinationName).
      thisSession:sendFrame(unsubscribeFrame).
      isOpen = FALSE.
    END.
  END METHOD.


  METHOD PUBLIC javax.jms.Message receive():
    DEFINE VARIABLE stompFrame      AS nl.flusso.stomp.StompFrame NO-UNDO.
    DEFINE VARIABLE receivedMessage AS javax.jms.Message          NO-UNDO.
    stompFrame = thisSession:receiveFrame(destinationName).
    IF stompFrame:binary THEN
      receivedMessage = NEW nl.flusso.stomp.BytesMessage(thisSession, stompFrame).
    ELSE
      receivedMessage = NEW nl.flusso.stomp.TextMessage(thisSession, stompFrame).
    RETURN receivedMessage.
  END METHOD.
END CLASS.

