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
/*
DEFINE PUBLIC PROPERTY acknowledgeMode AS INTEGER NO-UNDO
  GET.

*/


METHOD PUBLIC VOID close().

METHOD PUBLIC javax.jms.MessageConsumer createConsumer(destination AS javax.jms.Destination).


/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC javax.jms.MessageConsumer createConsumer(queue AS nl.flusso.stomp.Queue).

/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC javax.jms.MessageConsumer createConsumer(topic AS nl.flusso.stomp.Topic).

METHOD PUBLIC javax.jms.MessageProducer createProducer(destination AS javax.jms.Destination).

/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC javax.jms.MessageProducer createProducer(queue AS nl.flusso.stomp.Queue).

/* Not a part of real JMS API; workaround for OpenEdge limitation. */
METHOD PUBLIC javax.jms.MessageProducer createProducer(topic AS nl.flusso.stomp.Topic).

METHOD PUBLIC javax.jms.Queue createQueue(queueName AS CHARACTER).

METHOD PUBLIC javax.jms.TextMessage createTextMessage().

METHOD PUBLIC javax.jms.TextMessage createTextMessage(textString AS LONGCHAR).

METHOD PUBLIC javax.jms.Topic createTopic(topicName AS CHARACTER).

METHOD PUBLIC javax.jms.BytesMessage createBytesMessage(BytesString AS MEMPTR).
