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
USING nl.flusso.util.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.


CLASS Logger:
  DEFINE PUBLIC STATIC VARIABLE logEngine AS LogEngine NO-UNDO.


  DEFINE PUBLIC STATIC PROPERTY engine AS LogEngine NO-UNDO
    GET:
      IF NOT VALID-OBJECT(logEngine) THEN
        logEngine = NEW LogEngine().
      RETURN logEngine.
    END.
    
    
  DEFINE PUBLIC STATIC PROPERTY ERROR AS INTEGER NO-UNDO
    GET:
      RETURN 10.
    END.

    
  DEFINE PUBLIC STATIC PROPERTY WARNING AS INTEGER NO-UNDO
    GET:
      RETURN 20.
    END.

    
  DEFINE PUBLIC STATIC PROPERTY INFORMATION AS INTEGER NO-UNDO
    GET:
      RETURN 23.
    END.

    
  DEFINE PUBLIC STATIC PROPERTY CONFIG AS INTEGER NO-UNDO
    GET:
      RETURN 26.
    END.

    
  DEFINE PUBLIC STATIC PROPERTY DEBUG AS INTEGER NO-UNDO
    GET:
      RETURN 30.
    END.

    
  DEFINE PUBLIC STATIC PROPERTY DETAIL AS INTEGER NO-UNDO
    GET:
      RETURN 40.
    END.

    
  DEFINE PUBLIC STATIC PROPERTY debugALL AS INTEGER NO-UNDO
    GET:
      RETURN 99.
    END.
  
  
  METHOD PUBLIC STATIC VOID addHandler(logHandler AS LogHandler):
    engine:addHandler(logHandler).
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID clearHandlers():
    engine:clearHandlers().
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID debugLogManager():
    engine:debugLogManager().
  END METHOD.
  
    
  METHOD PUBLIC STATIC VOID processMessage(logLevel AS INTEGER, logMessage AS CHARACTER, subsystem AS CHARACTER):
    engine:handleMessage(NOW, logLevel, logMessage, subsystem).
  END METHOD.
  

  METHOD PUBLIC STATIC VOID dbg(debugText AS CHARACTER):
    processMessage(Logger:DEBUG, debugText, ?).
  END METHOD.
END CLASS.
