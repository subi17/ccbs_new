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

CLASS nl.flusso.stomp.Connection IMPLEMENTS javax.jms.QueueConnection, javax.jms.TopicConnection, javax.jms.Connection:
  DEFINE PRIVATE VARIABLE serverHost     AS CHARACTER NO-UNDO.
  DEFINE PRIVATE VARIABLE serverPort     AS INTEGER   NO-UNDO.
  DEFINE PRIVATE VARIABLE serverLogin    AS CHARACTER NO-UNDO.
  DEFINE PRIVATE VARIABLE serverPasscode AS CHARACTER NO-UNDO.  
  
  
  CONSTRUCTOR PUBLIC Connection(host AS CHARACTER, port AS INTEGER, login AS CHARACTER, passcode AS CHARACTER):
    serverHost = host.
    serverPort = port.
    serverLogin = login.
    serverPasscode = passcode.
  END CONSTRUCTOR.
  

  METHOD PUBLIC VOID close():
    /* For API compatibility only. */
  END METHOD.
  
  METHOD PUBLIC javax.jms.Session createSession(transacted AS LOGICAL, acknowledgeMode AS INTEGER):
    RETURN NEW Session(serverHost, serverPort, serverLogin, serverPasscode, acknowledgeMode).
  END METHOD.
  
  METHOD PUBLIC javax.jms.Session createSession(transacted      AS LOGICAL, 
                                                acknowledgeMode AS INTEGER,
                                                iiTimeOut       AS INTEGER ):
    RETURN NEW Session(serverHost, 
                       serverPort, 
                       serverLogin, 
                       serverPasscode, 
                       acknowledgeMode,
                       iiTimeOut ).
  END METHOD.
  
  METHOD PUBLIC javax.jms.QueueSession createQueueSession(transacted AS LOGICAL, acknowledgeMode AS INTEGER):
    RETURN NEW QueueSession(serverHost, serverPort, serverLogin, serverPasscode, acknowledgeMode).
  END METHOD.
  
  
  METHOD PUBLIC javax.jms.TopicSession createTopicSession(transacted AS LOGICAL, acknowledgeMode AS INTEGER):
    RETURN NEW TopicSession(serverHost, serverPort, serverLogin, serverPasscode, acknowledgeMode).
  END METHOD.
  

  METHOD PUBLIC VOID start():
    /* For API compatibility only. */
  END METHOD.

  
  METHOD PUBLIC VOID stop():
    /* For API compatibility only. */
  END METHOD.  
END CLASS.
