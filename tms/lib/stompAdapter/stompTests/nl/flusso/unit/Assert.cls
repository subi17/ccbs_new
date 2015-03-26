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
USING Progress.Lang.Object.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS nl.flusso.unit.Assert:
  METHOD PUBLIC STATIC VOID assertEquals(expected AS CHARACTER, actual AS CHARACTER):
    DEFINE VARIABLE errorMessage AS CHARACTER NO-UNDO.
    IF actual <> expected THEN
    DO:
      errorMessage = SUBSTITUTE('Unexpected value "&1" (expected "&2")':U, actual, expected).
      UNDO, THROW NEW AssertionFailedError(errorMessage).
    END.  
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID assertEquals(expected AS Object, actual AS Object):
    DEFINE VARIABLE errorMessage AS CHARACTER NO-UNDO.
    IF actual <> expected THEN
    DO:
      errorMessage = 'Unexpected object value.':U.
      UNDO, THROW NEW AssertionFailedError(errorMessage).
    END.  
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID assertTrue(condition AS LOGICAL):
    IF NOT condition THEN
    DO:
      UNDO, THROW NEW AssertionFailedError('Unexpected condition.':U).
    END.
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID assertTrue(condition AS LOGICAL, errorMessage AS CHARACTER):
    IF NOT condition THEN
    DO:
      UNDO, THROW NEW AssertionFailedError(errorMessage).
    END.
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID assertValidObject(actual AS Object):
    IF actual = ? OR VALID-OBJECT(actual) THEN
    DO:
      UNDO, THROW NEW AssertionFailedError('Not a valid object.':U).
    END.
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID assertNotValidObject(actual AS Object):
    IF actual <> ? AND VALID-OBJECT(actual) THEN
    DO:
      UNDO, THROW NEW AssertionFailedError('Object reference should not be valid.':U).
    END.
  END METHOD.
END CLASS.
