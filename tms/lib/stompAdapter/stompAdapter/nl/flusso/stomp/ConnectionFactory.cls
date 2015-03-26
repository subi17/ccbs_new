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

CLASS nl.flusso.stomp.ConnectionFactory IMPLEMENTS javax.jms.QueueConnectionFactory, javax.jms.TopicConnectionFactory, javax.jms.ConnectionFactory:
  DEFINE PRIVATE VARIABLE connectionHost AS CHARACTER NO-UNDO.
  DEFINE PRIVATE VARIABLE connectionPort AS INTEGER   NO-UNDO.


  CONSTRUCTOR PUBLIC ConnectionFactory():
    THIS-OBJECT('localhost':U, 61613).
  END CONSTRUCTOR.


  CONSTRUCTOR PUBLIC ConnectionFactory(host AS CHARACTER, port AS INTEGER):
    connectionHost = host.
    connectionPort = port.
  END CONSTRUCTOR.


  METHOD PUBLIC javax.jms.Connection createConnection():
    RETURN NEW Connection(connectionHost, connectionPort, ?, ?).
  END METHOD.


  METHOD PUBLIC javax.jms.Connection createConnection(userName AS CHARACTER, password AS CHARACTER).
    RETURN NEW Connection(connectionHost, connectionPort, userName, password).
  END METHOD.


  METHOD PUBLIC javax.jms.QueueConnection createQueueConnection().
    RETURN NEW Connection(connectionHost, connectionPort, ?, ?).
  END METHOD.


  METHOD PUBLIC javax.jms.QueueConnection createQueueConnection(userName AS CHARACTER, password AS CHARACTER).
    RETURN NEW Connection(connectionHost, connectionPort, userName, password).
  END METHOD.  


  METHOD PUBLIC javax.jms.TopicConnection createTopicConnection().
    RETURN NEW Connection(connectionHost, connectionPort, ?, ?).
  END METHOD.


  METHOD PUBLIC javax.jms.TopicConnection createTopicConnection(userName AS CHARACTER, password AS CHARACTER).
    RETURN NEW Connection(connectionHost, connectionPort, userName, password).
  END METHOD.  
END CLASS.