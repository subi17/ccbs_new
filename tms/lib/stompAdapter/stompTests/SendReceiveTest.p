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

&SCOPED-DEFINE queueName 'Bestand':U

DEFINE VARIABLE factory         AS QueueConnectionFactory NO-UNDO.
DEFINE VARIABLE queueConnection AS QueueConnection        NO-UNDO.

DEFINE VARIABLE gStringProperty AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gLongProperty   AS INT64      NO-UNDO.
DEFINE VARIABLE gDoubleProperty AS DECIMAL  NO-UNDO.
DEFINE VARIABLE mMemptr AS MEMPTR      NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER   NO-UNDO.

/* Verstuur TextMessage's (True) / BytesMessage (false)*/
DEFINE VARIABLE lText AS LOGICAL     NO-UNDO INIT TRUE.


PROCEDURE sendMessage:
  DEFINE INPUT PARAMETER textMessage AS CHARACTER NO-UNDO.

  DEFINE VARIABLE mp            AS MEMPTR       NO-UNDO.
  DEFINE VARIABLE l             AS LONGCHAR     NO-UNDO.
  DEFINE VARIABLE queueSession  AS QueueSession NO-UNDO.
  DEFINE VARIABLE sendQueue     AS Queue        NO-UNDO.
  DEFINE VARIABLE queueSender   AS QueueSender  NO-UNDO.
  DEFINE VARIABLE messageToSend AS TextMessage  NO-UNDO.

  queueSession    = queueConnection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  sendQueue       = queueSession:createQueue({&queueName}).
  queueSender     = queueSession:createSender(sendQueue).
  
  FIX-CODEPAGE(l) = 'UTF-8':U.
  IF textMessage = 'flusso':U THEN
  DO:
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
  
    COPY-LOB FROM mp TO l NO-CONVERT.
  END.
  ELSE 
    l = textMessage.
  
  messageToSend = queueSession:createTextMessage(l).
  messageToSend:JMSDeliveryMode = "true".
  messageToSend:setStringProperty("StringProperty","Waarde").
  messagetosend:setstringproperty("test","2").
  messageToSend:setDoubleProperty("DoubleProperty",12.345).
  messageToSend:setLongProperty("LongProperty",12345).
  queueSender:send(messageToSend).

  FINALLY:
    DELETE OBJECT messageToSend   NO-ERROR.
    DELETE OBJECT queueSender     NO-ERROR.
    DELETE OBJECT sendQueue       NO-ERROR.
    DELETE OBJECT queueSession    NO-ERROR.
  END FINALLY.
END PROCEDURE.


PROCEDURE sendBytesMessage:
  DEFINE INPUT PARAMETER ipcFile      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER bytesMessage AS MEMPTR      NO-UNDO.

  DEFINE VARIABLE queueSession  AS QueueSession NO-UNDO.
  DEFINE VARIABLE sendQueue     AS Queue        NO-UNDO.
  DEFINE VARIABLE queueSender   AS QueueSender  NO-UNDO.
  DEFINE VARIABLE messageToSend AS BytesMessage  NO-UNDO.

  queueSession    = queueConnection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  sendQueue       = queueSession:createQueue({&queueName}).
  queueSender     = queueSession:createSender(sendQueue).
  messageToSend   = queueSession:createBytesMessage(bytesMessage).
  
  messageToSend:JMSDeliveryMode = "true".
  messageToSend:setStringProperty("FileName",ipcFile).
  messageToSend:setLongProperty("FileSize",GET-SIZE(bytesMessage)).
  
  queueSender:send(messageToSend).

  FINALLY:
    DELETE OBJECT messageToSend   NO-ERROR.
    DELETE OBJECT queueSender     NO-ERROR.
    DELETE OBJECT sendQueue       NO-ERROR.
    DELETE OBJECT queueSession    NO-ERROR.
  END FINALLY.
END PROCEDURE.


PROCEDURE receiveMessage:
  DEFINE VARIABLE queueSession    AS QueueSession  NO-UNDO.
  DEFINE VARIABLE receiveQueue    AS Queue         NO-UNDO.
  DEFINE VARIABLE queueReceiver   AS QueueReceiver NO-UNDO.
  DEFINE VARIABLE receivedMessage AS Message       NO-UNDO.
  DEFINE VARIABLE textMessage     AS textMessage   NO-UNDO.
  DEFINE VARIABLE bytesMessage    AS BytesMessage  NO-UNDO.
  DEFINE VARIABLE longcharText    AS LONGCHAR      NO-UNDO.
  DEFINE VARIABLE messageType     AS CLASS         Progress.Lang.Class NO-UNDO.
  
  queueSession    = queueConnection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  receiveQueue    = queueSession:createQueue({&queueName}).
  queueReceiver   = queueSession:createReceiver(receiveQueue).
  receivedMessage = queueReceiver:receive().
  messageType     = receivedMessage:getClass().
  
  gStringProperty = receivedMessage:getstringProperty(?).
  gLongProperty = receivedMessage:getLongProperty("LongProperty").
  gDoubleProperty = receivedMessage:getDoubleProperty("DoubleProperty").

  textMessage  = CAST(receivedMessage, TextMessage) NO-ERROR.
  bytesMessage = CAST(receivedMessage,BytesMessage) NO-ERROR.
  
  longcharText = textMessage:text.
  receivedMessage:acknowledge().
  
MESSAGE    "String Property: " gStringProperty SKIP
    "Long Property: " gLongProperty     SKIP
    "Double Property: " gDoubleProperty 
    VIEW-AS ALERT-BOX.
    FINALLY:
    DELETE OBJECT receivedMessage NO-ERROR.
    DELETE OBJECT queueReceiver   NO-ERROR.
    DELETE OBJECT receiveQueue    NO-ERROR.
    DELETE OBJECT queueSession    NO-ERROR.
  END FINALLY.
END PROCEDURE.

PROCEDURE receiveBytesMessage:
  DEFINE VARIABLE queueSession    AS QueueSession  NO-UNDO.
  DEFINE VARIABLE receiveQueue    AS Queue         NO-UNDO.
  DEFINE VARIABLE queueReceiver   AS QueueReceiver NO-UNDO.
  DEFINE VARIABLE receivedMessage AS Message       NO-UNDO.
  DEFINE VARIABLE bytesMessage    AS BytesMessage  NO-UNDO.
  DEFINE VARIABLE longcharText    AS LONGCHAR      NO-UNDO.
  DEFINE VARIABLE messageType     AS CLASS         Progress.Lang.Class NO-UNDO.
  DEFINE VARIABLE lcFile AS CHARACTER   NO-UNDO.
  
  queueSession    = queueConnection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).
  receiveQueue    = queueSession:createQueue({&queueName}).
  queueReceiver   = queueSession:createReceiver(receiveQueue).
  
  
  receivedMessage = queueReceiver:receive().
  /* messageType     = receivedMessage:getClass(). */
  bytesMessage = CAST(receivedMessage,BytesMessage) NO-ERROR.
MESSAGE ERROR-STATUS:GET-MESSAGE(1)
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  lcFile = session:TEMP-DIR + receivedMessage:getstringProperty("FileName").
  MESSAGE lcFile
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  COPY-LOB FROM OBJECT bytesMessage:bytes TO FILE lcFile NO-CONVERT.
  receivedMessage:acknowledge().
  
  FINALLY:
    DELETE OBJECT receivedMessage NO-ERROR.
    DELETE OBJECT queueReceiver   NO-ERROR.
    DELETE OBJECT receiveQueue    NO-ERROR.
    DELETE OBJECT queueSession    NO-ERROR.
  END FINALLY.
END PROCEDURE.


PROCEDURE initialize:
  factory         = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613).
  queueConnection = factory:createQueueConnection(?, ?).
END PROCEDURE.


PROCEDURE finalize:
  DELETE OBJECT queueConnection NO-ERROR.
  DELETE OBJECT factory         NO-ERROR.
END PROCEDURE.


Logger:debugLogManager().
RUN initialize.

lText = FALSE.
IF lText THEN DO:
  /*RUN sendMessage('Test Properies').*/
  RUN receiveMessage.
END.
ELSE DO:
/*   cFile = "C:\progress\oejms\receivedText.txt". */
/*   
   cFile = "C:\opt\fuse-esb-4.1.0.0\conf\installsession_log.xml".
    cfile = "C:\oeafmb\pub\docs\image\connection.png". 
    cfile = "c:\tmp\maarten.log".
  COPY-LOB FILE cFile TO OBJECT mMemptr.
  cFile = ENTRY(NUM-ENTRIES(cFile,'\'),cfile,'\' ).
  RUN sendBytesMessage(cFile,mMemptr).
*/  
  RUN receiveBytesMessage.
END.

RUN finalize.

CATCH anyError AS Progress.Lang.Error:
  DEFINE VARIABLE errorNr  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE errorMsg AS CHARACTER NO-UNDO.
  DO errorNr = 1 TO anyError:NumMessages:
    errorMsg = errorMsg + anyError:GetMessage(errorNr) + '~r~n':U.
  END.

  MESSAGE 
    'Catch: ' SKIP
    errorMsg SKIP
    anyError:CallStack 
    VIEW-AS ALERT-BOX.
  RUN finalize.
END CATCH.
