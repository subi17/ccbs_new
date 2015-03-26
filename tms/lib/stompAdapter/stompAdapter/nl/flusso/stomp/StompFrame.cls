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
ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS nl.flusso.stomp.StompFrame:
  DEFINE PRIVATE TEMP-TABLE headerTable NO-UNDO
    FIELD headerName  AS CHARACTER
    FIELD headerValue AS CHARACTER
    INDEX pk IS UNIQUE PRIMARY
      headerName.
  

  DEFINE PUBLIC PROPERTY binary AS LOGICAL NO-UNDO
    GET:
      RETURN (currentHeaderValue('content-length':U) <> ?).
    END GET.
    
    
  DEFINE PUBLIC PROPERTY bytes AS MEMPTR NO-UNDO
    GET.
    SET.
  

  DEFINE PUBLIC PROPERTY frameType AS CHARACTER NO-UNDO
    GET.
    SET.
  

  DEFINE PUBLIC PROPERTY text1 AS LONGCHAR NO-UNDO
    GET.
    SET.

  
  CONSTRUCTOR PUBLIC StompFrame(headerText AS LONGCHAR):
      processHeaderText(headerText).
  END CONSTRUCTOR.
  

  CONSTRUCTOR PUBLIC StompFrame(headerBytes AS MEMPTR):
    DEFINE VARIABLE headerText AS LONGCHAR NO-UNDO.
    FIX-CODEPAGE(headerText) = 'UTF-8':U.
    COPY-LOB headerBytes TO headerText NO-CONVERT.
    processHeaderText(headerText).
  END CONSTRUCTOR.


  CONSTRUCTOR PUBLIC StompFrame(jmsMessage AS nl.flusso.stomp.Message):
    DEFINE VARIABLE textMessage  AS nl.flusso.stomp.TextMessage  NO-UNDO.
    DEFINE VARIABLE bytesMessage AS nl.flusso.stomp.BytesMessage NO-UNDO.
    DEFINE VARIABLE bytesBuffer  AS MEMPTR                 NO-UNDO.
    DEFINE VARIABLE bytesRead    AS INTEGER                NO-UNDO.

    THIS-OBJECT:frameType = 'SEND':U.
    assignHeaderValue('message-id':U, jmsMessage:JMSMessageID).
    assignHeaderValue('correlation-id':U, jmsMessage:JMSCorrelationID).
    textMessage = CAST(jmsMessage, nl.flusso.stomp.TextMessage) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      THIS-OBJECT:text1 = textMessage:text1.
    bytesMessage = CAST(jmsMessage, nl.flusso.stomp.BytesMessage) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
    DO:
      
      THIS-OBJECT:text1 = bytesMessage:text1.
      
      assignHeaderValue('content-length':U, STRING(bytesMessage:bodyLength)).
      /* downgrading bytesMessage:bodyLength to INTEGER, maximum length BytesMessage is 2GB */
      SET-SIZE(bytesBuffer) = INTEGER(bytesMessage:bodyLength).
      bytesRead = bytesMessage:readBytes(bytesBuffer).
      bytes = bytesBuffer.
    END.
   
    FINALLY:
      DELETE OBJECT textMessage  NO-ERROR.
      DELETE OBJECT bytesMessage NO-ERROR.
    END FINALLY.
  END CONSTRUCTOR.
  

  METHOD PUBLIC VOID assignHeaderValue(name AS CHARACTER, stringValue AS CHARACTER):
    DEFINE BUFFER headerRow FOR headerTable.
    FIND headerRow WHERE headerRow.headerName = name NO-ERROR.
    IF stringValue = ? THEN
    DO:
      IF AVAILABLE headerRow THEN
        DELETE headerRow.
    END.
    ELSE
    DO:                        
      IF NOT AVAILABLE headerRow THEN
      DO:
        CREATE headerRow.
        headerRow.headerName = name.
      END.
      headerRow.headerValue = stringValue.
    END.
    
  END METHOD.


  METHOD PUBLIC CHARACTER currentHeaderValue(name AS CHARACTER):
    DEFINE BUFFER headerRow FOR headerTable.
    DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
    
    IF NAME = ? THEN
    DO:
      FOR EACH HeaderRow:
        ASSIGN cReturn = SUBSTITUTE("&1;&2|", 
                                    headerRow.HeaderName, 
                                    HeaderRow.headerValue) + cReturn.
      END.
      RETURN cReturn.
    END.
    FIND headerRow WHERE headerRow.headerName = name NO-ERROR.
    
    RETURN (IF AVAILABLE headerRow THEN headerRow.headerValue ELSE ?).
  END METHOD.


  METHOD PUBLIC MEMPTR composeFrameBytes():
    DEFINE VARIABLE bytesToSend  AS MEMPTR   NO-UNDO.
    DEFINE VARIABLE headerString AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE separator    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE bodySize     AS INTEGER  NO-UNDO.
    DEFINE VARIABLE headerSize   AS INTEGER  NO-UNDO.
    DEFINE VARIABLE bufferSize   AS INTEGER  NO-UNDO.
    DEFINE VARIABLE bodyBytes    AS MEMPTR   NO-UNDO.
    DEFINE BUFFER headerRow FOR headerTable.
    FIX-CODEPAGE(headerString) = 'UTF-8':U.
    FIX-CODEPAGE(separator) = 'UTF-8':U.
    separator = '~r~n':U.
    headerString = frameType + separator.
    FOR EACH headerRow:
      headerString = headerString + headerRow.headerName + ': ':U + headerRow.headerValue + separator.
    END.
    headerString = headerString + separator.
    headerSize = LENGTH(headerString, 'RAW':U).
    
    IF binary THEN
      bodyBytes = THIS-OBJECT:bytes.
    ELSE
      COPY-LOB THIS-OBJECT:text1 TO bodyBytes NO-CONVERT.
    bodySize = GET-SIZE(bodyBytes).
    bufferSize = headerSize + bodySize + 1.
    SET-SIZE(bytesToSend) = bufferSize.
    
    COPY-LOB headerString TO bytesToSend OVERLAY AT 1 NO-CONVERT.
    COPY-LOB bodyBytes TO bytesToSend OVERLAY AT headerSize + 1 NO-CONVERT.
    PUT-BYTE(bytesToSend, bufferSize) = 0.
    RETURN bytesToSend.

    FINALLY:
      SET-SIZE(bodyBytes) = 0.
    END FINALLY.
  END METHOD.


  METHOD PRIVATE VOID processHeaderText(headerText AS LONGCHAR):
    DEFINE VARIABLE headerTextSeparator AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lineNumber          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE headerLine          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE colonLocation       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE headerId            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE headerValue         AS LONGCHAR  NO-UNDO.
    headerText = LEFT-TRIM(headerText).
    headerTextSeparator = locateHeaderTextSeparator(headerText).
    frameType = ENTRY(1, headerText, headerTextSeparator).
    DO lineNumber = 2 TO NUM-ENTRIES(headerText, headerTextSeparator):
      headerLine = ENTRY(lineNumber, headerText, headerTextSeparator).
      colonLocation = INDEX(headerLine, ':':U).
      IF colonLocation = 0 THEN
        NEXT.
      assignHeaderValue(SUBSTRING(headerLine, 1, colonLocation - 1), SUBSTRING(headerLine, colonLocation + 1)).
    END.
  END METHOD.
  

  METHOD PRIVATE CHARACTER locateHeaderTextSeparator(headerText AS LONGCHAR):
    DEFINE VARIABLE indexCarriageReturn AS INTEGER NO-UNDO.
    DEFINE VARIABLE indexLineFeed       AS INTEGER NO-UNDO.
    indexCarriageReturn = INDEX(headerText, '~r':U).
    indexLineFeed = INDEX(headerText, '~n':U).
    RETURN (IF indexCarriageReturn > 0 AND indexCarriageReturn + 1 = indexLineFeed THEN '~r~n':U ELSE '~n':U).
  END METHOD.  
END CLASS.
