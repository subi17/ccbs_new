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

CLASS nl.flusso.stomp.Message IMPLEMENTS javax.jms.Message:
  DEFINE PRIVATE VARIABLE thisSession AS Session NO-UNDO.
  
  
  DEFINE PUBLIC PROPERTY stompFrame AS StompFrame NO-UNDO
    GET.
    PRIVATE SET.


  DEFINE PUBLIC PROPERTY JMSCorrelationID AS CHARACTER NO-UNDO
    GET():
      RETURN stompFrame:currentHeaderValue('correlation-id':U).
    END GET.
    SET(propertyValue AS CHARACTER):
      stompFrame:assignHeaderValue('correlation-id':U, propertyValue).
    END SET.


  DEFINE PUBLIC PROPERTY JMSMessageID AS CHARACTER NO-UNDO
    GET():
      RETURN stompFrame:currentHeaderValue('message-id':U).
    END GET.

  DEFINE PUBLIC PROPERTY JMSDeliveryMode AS CHARACTER NO-UNDO
    GET():
      RETURN  stompFrame:currentHeaderValue('persistent':U).
    END GET.
    SET(propertyValue AS CHARACTER):
      stompFrame:assignHeaderValue('persistent':U,propertyValue).
    END SET.

  CONSTRUCTOR PUBLIC Message():
    stompFrame = NEW StompFrame('MESSAGE':U).
  END CONSTRUCTOR.


  CONSTRUCTOR PUBLIC Message(messageSession AS Session, aStompFrame AS StompFrame):
    thisSession = messageSession.
    stompFrame = aStompFrame.
  END CONSTRUCTOR.


  METHOD PUBLIC VOID acknowledge():
    thisSession:acknowledge(THIS-OBJECT).
  END METHOD.

  METHOD PUBLIC CHARACTER getStringProperty (PropertyName AS CHARACTER):
    RETURN stompFrame:currentHeaderValue(PropertyName).
  END METHOD.

  METHOD PUBLIC VOID setStringProperty (PropertyName AS CHARACTER, PropertyValue AS CHARACTER):
    stompFrame:assignHeaderValue(PropertyName,PropertyValue).
  END METHOD.

  METHOD PUBLIC INT64 getLongProperty (PropertyName AS CHARACTER):
    RETURN INT64 (stompFrame:currentHeaderValue(PropertyName)).
  END METHOD.
  
  METHOD PUBLIC VOID setLongProperty (PropertyName AS CHARACTER, PropertyValue AS INT64):
    stompFrame:assignHeaderValue(PropertyName,STRING(PropertyValue)).
  END METHOD.

  METHOD PUBLIC DECIMAL getDoubleProperty (PropertyName AS CHARACTER):
    RETURN DECIMAL (stompFrame:currentHeaderValue(PropertyName)).
  END METHOD.
  
  METHOD PUBLIC VOID setDoubleProperty (PropertyName AS CHARACTER, PropertyValue AS DECIMAL):
    stompFrame:assignHeaderValue(PropertyName,STRING(PropertyValue)).
  END METHOD.

END CLASS.
