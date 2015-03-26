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

CLASS nl.flusso.stomp.MessageProducer IMPLEMENTS javax.jms.MessageProducer:
  DEFINE PROTECTED VARIABLE thisSession AS Session NO-UNDO.


  DEFINE PUBLIC PROPERTY destination AS javax.jms.Destination NO-UNDO
    GET.
    PRIVATE SET.

    
  CONSTRUCTOR PROTECTED MessageProducer(jmsSession AS Session, messageDestination AS javax.jms.Destination):
    thisSession = jmsSession.
    destination = messageDestination.
  END CONSTRUCTOR.


  METHOD PUBLIC VOID sendMessage(jmsMessage AS nl.flusso.stomp.Message):
    sendMessage(destination, jmsMessage).
  END METHOD.
  

  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(jmsMessage AS nl.flusso.stomp.TextMessage):
    sendMessage(CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  METHOD PUBLIC VOID sendMessage(jmsMessage AS nl.flusso.stomp.BytesMessage):
    sendMessage(CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  METHOD PUBLIC VOID sendMessage(messageDestination AS javax.jms.Destination, jmsMessage AS nl.flusso.stomp.Message):
    DEFINE VARIABLE jmsQueue AS nl.flusso.stomp.Queue NO-UNDO.
    DEFINE VARIABLE jmsTopic AS nl.flusso.stomp.Topic NO-UNDO.
    
    jmsQueue = CAST(messageDestination, nl.flusso.stomp.Queue) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      postMessage('queue':U, jmsQueue:queueName, jmsMessage).
    ELSE
    DO:
      jmsTopic = CAST(messageDestination, nl.flusso.stomp.Topic).
      postMessage('topic':U, jmsTopic:topicName, jmsMessage).
    END.
  END METHOD.


  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS javax.jms.Destination, jmsMessage AS nl.flusso.stomp.TextMessage).
    sendMessage(messageDestination, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS javax.jms.Destination, jmsMessage AS nl.flusso.stomp.BytesMessage).
    sendMessage(messageDestination, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Queue, jmsMessage AS nl.flusso.stomp.Message):
    postMessage('queue':U, messageDestination:queueName, jmsMessage).
  END METHOD.


  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Queue, jmsMessage AS nl.flusso.stomp.TextMessage):
    sendMessage(messageDestination, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Queue, jmsMessage AS nl.flusso.stomp.BytesMessage):
    sendMessage(messageDestination, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.Message):
    postMessage('topic':U, messageDestination:topicName, jmsMessage).
  END METHOD.
  
  
  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.TextMessage).
    sendMessage(messageDestination, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.
    
  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID sendMessage(messageDestination AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.BytesMessage).
    sendMessage(messageDestination, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  METHOD PUBLIC VOID postMessage(destinationType AS CHARACTER, destinationName AS CHARACTER, jmsMessage AS nl.flusso.stomp.Message):
    DEFINE VARIABLE aMessage    AS Message    NO-UNDO.
    DEFINE VARIABLE frameToSend AS StompFrame NO-UNDO.

    aMessage = CAST(jmsMessage, Message) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
    DO:
      frameToSend = aMessage:stompFrame.
      frameToSend:frameType = 'SEND':U.
    END.
    ELSE
      frameToSend = NEW StompFrame(jmsMessage).
    frameToSend:assignHeaderValue('destination':U, SUBSTITUTE('/&1/&2':U, destinationType, destinationName)).
    thisSession:sendFrame(frameToSend).
  END METHOD.
END CLASS.
