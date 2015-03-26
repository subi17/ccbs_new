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

CLASS nl.flusso.stomp.TopicPublisher INHERITS MessageProducer IMPLEMENTS javax.jms.TopicPublisher:
  DEFINE PUBLIC PROPERTY topic AS nl.flusso.stomp.Topic NO-UNDO
    GET.
    PRIVATE SET.
    
  
  CONSTRUCTOR PUBLIC TopicPublisher(jmsSession AS Session, jmsTopic AS nl.flusso.stomp.Topic):
    SUPER(jmsSession, CAST(jmsTopic, javax.jms.Destination)).
    topic = jmsTopic.
  END CONSTRUCTOR.


  METHOD PUBLIC VOID publish(jmsMessage AS javax.jms.Message):
    THIS-OBJECT:PUBLISH(topic, jmsMessage).
  END METHOD.


/* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID publish(jmsMessage AS nl.flusso.stomp.TextMessage):
    THIS-OBJECT:PUBLISH(topic, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

/* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID publish(jmsMessage AS nl.flusso.stomp.BytesMessage):
    THIS-OBJECT:PUBLISH(topic, CAST(jmsMessage, nl.flusso.stomp.Message)).
  END METHOD.

  
  METHOD PUBLIC VOID publish(jmsTopic AS nl.flusso.stomp.Topic, jmsMessage AS javax.jms.Message):
    postMessage('topic':U, jmsTopic:topicName, CAST(jmsMessage, nl.flusso.stomp.Message)  ).
  END METHOD.
  

  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID publish(jmsTopic AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.TextMessage):
    postMessage('topic':U, jmsTopic:topicName, CAST(jmsMessage, nl.flusso.stomp.Message)  ).
  END METHOD.
  

  /* Workaround for OpenEdge limitation: no interface inheritance. */
  METHOD PUBLIC VOID publish(jmsTopic AS nl.flusso.stomp.Topic, jmsMessage AS nl.flusso.stomp.BytesMessage):
    postMessage('topic':U, jmsTopic:topicName, CAST(jmsMessage, nl.flusso.stomp.Message)  ).
  END METHOD.
END CLASS.
