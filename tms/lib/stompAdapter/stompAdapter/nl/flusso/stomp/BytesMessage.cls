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

CLASS nl.flusso.stomp.BytesMessage INHERITS Message IMPLEMENTS javax.jms.BytesMessage:
  DEFINE PUBLIC PROPERTY bodyLength AS INT64 NO-UNDO
    GET:
      RETURN GET-SIZE(stompFrame:bytes).
    END GET.

    DEFINE PUBLIC PROPERTY bytes AS MEMPTR NO-UNDO
      GET:
        RETURN stompFrame:bytes.
      END GET.
      SET (bytes AS MEMPTR):
        stompFrame:bytes = bytes.
        
        /* Set the content-length for the bytes in the body */
        stompFrame:assignHeaderValue('content-length':U, STRING(GET-SIZE(bytes))).
      END SET.
    
  DEFINE PUBLIC PROPERTY text1 AS LONGCHAR NO-UNDO
    GET:
      RETURN stompFrame:text1.
    END GET.
    SET(body AS LONGCHAR):
      stompFrame:text1 = body.
    END SET.
    
  CONSTRUCTOR PUBLIC BytesMessage():
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC BytesMessage(thisSession AS Session, stompFrame AS StompFrame):
    SUPER(thisSession, stompFrame).
  END CONSTRUCTOR.


  METHOD PUBLIC INTEGER readByte().
  END METHOD.
    
  METHOD PUBLIC INTEGER readBytes(bytesBuffer AS MEMPTR).
  END METHOD.
    
  METHOD PUBLIC INTEGER readBytes(bytesBuffer AS MEMPTR, byteCount AS INTEGER).
  END METHOD.
    
  METHOD PUBLIC INTEGER readUnsignedByte().
  END METHOD.
    
  METHOD PUBLIC VOID writeByte(byteValue AS INTEGER).
  END METHOD.
    
  METHOD PUBLIC VOID writeBytes(bytesBuffer AS MEMPTR).
  END METHOD.
    
  METHOD PUBLIC VOID writeBytes(bytesBuffer AS MEMPTR, offset AS INTEGER, byteCount AS INTEGER).
  END METHOD.

END CLASS.
