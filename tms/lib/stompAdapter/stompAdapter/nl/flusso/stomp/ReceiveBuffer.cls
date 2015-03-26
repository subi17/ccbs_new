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

CLASS nl.flusso.stomp.ReceiveBuffer:
  DEFINE PRIVATE VARIABLE bytesBuffer AS MEMPTR  NO-UNDO.
  DEFINE PRIVATE VARIABLE analyzed    AS INTEGER NO-UNDO.
  

  CONSTRUCTOR PUBLIC ReceiveBuffer():
    SET-SIZE(bytesBuffer) = 0.
  END CONSTRUCTOR.
  

  DESTRUCTOR PUBLIC ReceiveBuffer():
    SET-SIZE(bytesBuffer) = 0.
  END DESTRUCTOR.


  METHOD PUBLIC VOID addBytes(bytesToAdd AS MEMPTR, sizeToAdd AS INTEGER):
    DEFINE VARIABLE oldSize        AS INTEGER NO-UNDO.
    DEFINE VARIABLE newSize        AS INTEGER NO-UNDO.
    DEFINE VARIABLE newBytesBuffer AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE dummyBuffer    AS MEMPTR  NO-UNDO.

    IF sizeToAdd > 0 THEN
    DO:
      oldSize = GET-SIZE(bytesBuffer).
      newSize = oldSize + sizeToAdd.
      SET-SIZE(newBytesBuffer) = newSize.
      IF oldSize > 0 THEN
        COPY-LOB bytesBuffer TO newBytesBuffer OVERLAY AT 1.
     
      /*  
	  # Cannot copy more than 32K from one MEMPTR variable into another MEMPTR variable
	  # Using PUT-BYTES and GET-BYTES functions, get the following errors:
	  # Attempt to exceed maximum size of a RAW variable. (9190)
	  # Unable to evaluate expression with UNKNOWN value in argument. (4391)

	  Cause:  
	  # Because the return value of GET-BYTES is passed immediately into the PUT-BYTES function, it is unable to determine if it should return RAW or MEMPTR.
	  Therefore, it will default to RAW, which is subject to the 32K limit.
	
	  Fixes:  
	  Use an intermediate MEMPTR variable to ensure GET-BYTES returns the correct datatype.
	
  	  In OpenEdge 10, it's also possible to treat the MEMPTRs as large objects, and use the COPY-LOB statement to copy data from one MEMPTR to the other.      
      
      Originale line of code:
      PUT-BYTES(newBytesBuffer, oldSize + 1) = GET-BYTES(bytesToAdd, 1, sizeToAdd).
      */ 
      
      COPY-LOB bytesToAdd STARTING AT 1 FOR sizeToAdd TO newBytesBuffer OVERLAY AT oldSize + 1.
      /* The FROM phrase is the equivalent of GET-BYTES and it's parameters */
      /* The TO phrase is the equivalent of PUT-BYTES and it's parameters */
      
      bytesBuffer = newBytesBuffer.
    END.
    
    SET-SIZE(dummyBuffer) = 0.
    
    RETURN.

    FINALLY:
      SET-SIZE(newBytesBuffer) = 0.
    END FINALLY.
  END METHOD.


  METHOD PUBLIC MEMPTR extractBodyBytes():
    RETURN bytesBuffer.
  END METHOD.
  
  METHOD PUBLIC MEMPTR extractBodyBytes(byteCount AS INTEGER):
    RETURN bytesBuffer.
  END METHOD.

  
  METHOD PUBLIC LONGCHAR extractHeader():
    RETURN extractText(TRUE).
  END METHOD.

  
  METHOD PUBLIC LONGCHAR extractBodyText():
    RETURN extractText(FALSE).
  END METHOD.


  METHOD PRIVATE MEMPTR extractBeforeSeparator(blankLine AS LOGICAL):
    DEFINE VARIABLE emptyBuffer     AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE resultBuffer    AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE found           AS INTEGER NO-UNDO INITIAL -1.
    DEFINE VARIABLE bufferSize      AS INTEGER NO-UNDO.
    DEFINE VARIABLE requiredBytes   AS INTEGER NO-UNDO.
    DEFINE VARIABLE currentPosition AS INTEGER NO-UNDO.
    DEFINE VARIABLE searchByte      AS INTEGER NO-UNDO.
    DEFINE VARIABLE separatorLength AS INTEGER NO-UNDO.

    SET-SIZE(emptyBuffer) = 0.
    bufferSize = GET-SIZE(bytesBuffer).
  
    IF bufferSize < 8000 AND bufferSize > 0 THEN
    DO:
      Logger:dbg(SUBSTITUTE('Analyze buffer content: "&1"':U, GET-STRING(bytesBuffer, 1, bufferSize))).
      DEFINE VARIABLE bytePosition AS INTEGER NO-UNDO.
      DO bytePosition = 1 TO bufferSize:
        Logger:dbg(SUBSTITUTE('byte &1: &2':U, STRING(bytePosition), STRING(GET-BYTE(bytesBuffer, bytePosition)))).
      END.
    END.
    

    requiredBytes = (IF blankLine THEN 3 ELSE 1).
    searchByte = (IF blankLine THEN 10 ELSE 0).
    IF bufferSize - analyzed >= requiredBytes THEN
    DO:
      currentPosition = analyzed + 1.
      DO WHILE found < 0 AND currentPosition <= bufferSize:
        IF GET-BYTE(bytesBuffer, currentPosition) = searchByte THEN
        DO:
          Logger:dbg('searchByte found.':U).
          IF blankLine THEN
          DO:
            IF bufferSize - currentPosition >= 1 AND GET-BYTE(bytesBuffer, currentPosition + 1) = 10 THEN
              separatorLength = 2.
            ELSE IF bufferSize - currentPosition >= 2 AND GET-BYTE(bytesBuffer, currentPosition + 1) = 13 AND GET-BYTE(bytesBuffer, currentPosition + 2) = 10 THEN
                separatorLength = 3.
            IF separatorLength >= 2 THEN
              found = currentPosition - 1.
            Logger:dbg(SUBSTITUTE('found (empty line): &1.':U, STRING(found))).
          END.
          ELSE
          DO:
            separatorLength = 1.
            found = currentPosition - 1.
            Logger:dbg(SUBSTITUTE('found (0): &1.':U, STRING(found))).
          END.
        END.
        analyzed = currentPosition.
        currentPosition = currentPosition + 1.
      END.
    END.

    IF found > 0 THEN
      COPY-LOB bytesBuffer FOR found  TO resultBuffer.
    ELSE
    DO:
      /* Workaround for OE 10.2A bug: 
       * SET-SIZE(local-mp) = 0 does not always work after reserving a larger size IN A PREVIOUS INVOCATION. 
       */ 
      resultBuffer = emptyBuffer.
    END.
    IF found >= 0 THEN
      leftTrimBytes(found + separatorLength).
    ELSE
      resultBuffer = ?.
    
    IF found < 0 THEN
      Logger:dbg(SUBSTITUTE('Not returning anything after analyzing &1 bytes.':U, STRING(analyzed))).
    ELSE
      Logger:dbg(SUBSTITUTE('returning &1 bytes':U, STRING(GET-SIZE(resultBuffer)))).

    RETURN resultBuffer.
  END METHOD.
  

  METHOD PRIVATE LONGCHAR extractText(blankLine AS LOGICAL):
    DEFINE VARIABLE bytes      AS MEMPTR   NO-UNDO.
    DEFINE VARIABLE textString AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE byteCount  AS INTEGER  NO-UNDO.
    bytes = extractBeforeSeparator(blankLine).
    IF bytes = ? THEN
      textString = ?.
    ELSE
    DO:
      FIX-CODEPAGE(textString) = 'UTF-8':U.
      byteCount = GET-SIZE(bytes).
      IF byteCount > 0 THEN
        COPY-LOB bytes TO textString NO-CONVERT.
    END.
    
    RETURN textString.

    FINALLY:
      SET-SIZE(bytes) = 0.
    END FINALLY.
  END METHOD.


  METHOD PRIVATE VOID leftTrimBytes(bytesToTrim AS INTEGER):
    DEFINE VARIABLE newBytesBuffer AS MEMPTR NO-UNDO.
    COPY-LOB bytesBuffer STARTING AT bytesToTrim + 1 TO newBytesBuffer.
    SET-SIZE(bytesBuffer) = 0.
    COPY-LOB newBytesBuffer TO bytesBuffer.
    analyzed = 0.

    FINALLY:
      SET-SIZE(newBytesBuffer) = 0.
    END FINALLY.
  END METHOD.
END CLASS.
