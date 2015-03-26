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
USING nl.flusso.unit.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT PARAMETER testObject AS HANDLE NO-UNDO.


PROCEDURE setUp:
END PROCEDURE.


PROCEDURE tearDown:
END PROCEDURE.


PROCEDURE runOneTest PRIVATE:
  DEFINE INPUT PARAMETER testProcedure AS CHARACTER NO-UNDO.
  
  Logger:dbg(SUBSTITUTE('Start &1.':U, testProcedure)).
  DO ON ERROR UNDO, THROW:
    RUN setUp IN testObject.
    RUN VALUE(testProcedure) IN testObject.
    
    CATCH e AS AssertionFailedError:
      MESSAGE e:GetMessage(1)
        VIEW-AS ALERT-BOX ERROR TITLE testProcedure.    		
    END CATCH.
    
    FINALLY:
      RUN tearDown IN testObject.
    END FINALLY.
  END.
  Logger:dbg(SUBSTITUTE('&1 done.':U, testProcedure)).
END PROCEDURE.


PROCEDURE runAllTests PRIVATE:
  DEFINE VARIABLE entries     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE entryNumber AS INTEGER   NO-UNDO.
  DEFINE VARIABLE entryName   AS CHARACTER NO-UNDO.
  entries = testObject:INTERNAL-ENTRIES.
  DO entryNumber = 1 TO NUM-ENTRIES(entries):
    entryName = ENTRY(entryNumber, entries).
    IF entryName BEGINS 'test':U THEN
      RUN runOneTest(entryName).
  END.
END PROCEDURE.
  

testObject:ADD-SUPER-PROCEDURE(THIS-PROCEDURE).
RUN runAllTests.
RETURN.


CATCH e AS Progress.Lang.Error:  
  MESSAGE e:GetMessage(1) VIEW-AS ALERT-BOX ERROR TITLE 'Uncaught exception':U.		
END CATCH.
FINALLY:
  IF VALID-HANDLE(testObject) THEN
  DO:
    RUN tearDown IN testObject.
    testObject:REMOVE-SUPER-PROCEDURE(THIS-PROCEDURE). 
  END.
END FINALLY.
