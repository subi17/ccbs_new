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
USING nl.flusso.unit.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE receiveBuffer AS ReceiveBuffer NO-UNDO.
DEFINE VARIABLE testFrameType AS CHARACTER     NO-UNDO INITIAL 'TestFrame':U.
DEFINE VARIABLE simpleHeaders AS CHARACTER     NO-UNDO INITIAL '~nheader1:value1~nheader2:value2':U.
DEFINE VARIABLE simpleBody    AS CHARACTER     NO-UNDO INITIAL 'body1~nbody2':U.


FUNCTION frameText RETURNS CHARACTER PRIVATE (frameType AS CHARACTER, headers AS CHARACTER, body AS CHARACTER):
  RETURN SUBSTITUTE('&1&2~n~n&3':U, frameType, headers, body).
END FUNCTION.


PROCEDURE addText PRIVATE:
  DEFINE INPUT  PARAMETER frameText AS CHARACTER NO-UNDO.
  DEFINE VARIABLE frameBytes AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE byteCount  AS INTEGER NO-UNDO.
  byteCount = LENGTH(frameText, 'RAW':U) + 1.
  SET-SIZE(frameBytes) = byteCount.
  PUT-STRING(frameBytes, 1) = frameText.
  receiveBuffer:addBytes(frameBytes, byteCount).
  RETURN.
  
  FINALLY:
    SET-SIZE(frameBytes) = 0.
  END FINALLY.  
END PROCEDURE.


PROCEDURE testEmptyLF:
  DEFINE VARIABLE data   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE actual AS CHARACTER NO-UNDO.
  data = frameText(testFrameType, '':U, '':U).
  RUN addText(data).
  actual = receiveBuffer:extractHeader().
  Assert:assertEquals(testFrameType, actual).
  actual = receiveBuffer:extractBodyText().
  Assert:assertEquals('':U, actual).
END PROCEDURE.


PROCEDURE testSimpleLF:
  DEFINE VARIABLE data   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE actual AS CHARACTER NO-UNDO.
  data = frameText(testFrameType, simpleHeaders, simpleBody).
  RUN addText(data).
  actual = receiveBuffer:extractHeader().
  Assert:assertEquals(testFrameType + simpleHeaders, actual).
  actual = receiveBuffer:extractBodyText().
  Assert:assertEquals(simpleBody, actual).
END PROCEDURE.


PROCEDURE setUp:
  receiveBuffer = NEW ReceiveBuffer(). 
END PROCEDURE.


PROCEDURE tearDown:
  IF VALID-OBJECT(receiveBuffer) THEN
    DELETE OBJECT receiveBuffer NO-ERROR.
END PROCEDURE.
  
  
Logger:debugLogManager().
RUN nl/flusso/unit/procedureSuite.p (INPUT THIS-PROCEDURE).
RETURN.


CATCH e AS Progress.Lang.Error:  
  MESSAGE e:GetMessage(1) VIEW-AS ALERT-BOX ERROR TITLE 'Uncaught exception':U.		
END CATCH.
FINALLY:
  RUN tearDown.
END FINALLY.
