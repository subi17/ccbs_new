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

/* Progress ondersteund nog niet dat een INTERFACE kan overerven, vandaar... */
INTERFACE javax.jms.BytesMessage /* INHERITS javax.jms.Message */:
  {javax/jms/Message-Details.i}
  /*
  DEFINE PUBLIC PROPERTY bodyLength AS INT64 NO-UNDO
    GET.
  
  DEFINE PUBLIC PROPERTY bytes AS MEMPTR NO-UNDO
    GET.
    SET.

  DEFINE PUBLIC PROPERTY text AS LONGCHAR NO-UNDO
    GET.
    SET.
*/
  METHOD PUBLIC INTEGER readByte().
    
  METHOD PUBLIC INTEGER readBytes(bytesBuffer AS MEMPTR).
    
  METHOD PUBLIC INTEGER readBytes(bytesBuffer AS MEMPTR, byteCount AS INTEGER).
    
  METHOD PUBLIC INTEGER readUnsignedByte().
    
  METHOD PUBLIC VOID writeByte(byteValue AS INTEGER).
    
  METHOD PUBLIC VOID writeBytes(bytesBuffer AS MEMPTR).
    
  METHOD PUBLIC VOID writeBytes(bytesBuffer AS MEMPTR, offset AS INTEGER, byteCount AS INTEGER).
END INTERFACE.
