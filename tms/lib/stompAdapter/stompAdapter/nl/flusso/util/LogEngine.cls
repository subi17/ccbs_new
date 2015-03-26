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

&SCOPED-DEFINE DEFAULT_SUBSYSTEM 'APPL':U


CLASS nl.flusso.util.LogEngine IMPLEMENTS LogHandler:
  DEFINE PRIVATE VARIABLE usingLogManager AS LOGICAL NO-UNDO.
  
  DEFINE PRIVATE TEMP-TABLE logTable NO-UNDO
    FIELD logHandler AS Progress.Lang.Object.

  
  METHOD PUBLIC VOID addHandler(newLogHandler AS LogHandler):
    DEFINE BUFFER logBuffer FOR logTable.
    CREATE logBuffer.
    logBuffer.logHandler = newLogHandler.
  END METHOD.
  
  
  METHOD PUBLIC VOID clearHandlers():
    DEFINE BUFFER logBuffer FOR logTable.
    FOR EACH logBuffer:
      DELETE OBJECT logBuffer.logHandler NO-ERROR.
      DELETE logBuffer.
    END.
    usingLogManager = FALSE.
  END METHOD.
  
  
  METHOD PUBLIC VOID debugLogManager():
    IF NOT usingLogManager THEN
    DO:
      addHandler(NEW LogManagerHandler(Logger:debugAll)).
      usingLogManager = TRUE.
    END.
  END METHOD.
  
    
  METHOD PUBLIC VOID handleMessage(logTime AS DATETIME-TZ, logLevel AS INTEGER, logMessage AS CHARACTER, subsystem AS CHARACTER):
    DEFINE BUFFER logBuffer FOR logTable.
    DEFINE VARIABLE currentHandler AS LogHandler  NO-UNDO.
    IF subsystem = ? THEN
      subsystem = {&DEFAULT_SUBSYSTEM}.
    FOR EACH logBuffer:
      IF VALID-OBJECT(logBuffer.logHandler) THEN
      DO:
        currentHandler = CAST(logBuffer.logHandler, LogHandler).
        currentHandler:handleMessage(logTime, logLevel, logMessage, subsystem).
      END.
      ELSE
        DELETE logBuffer.
    END.
  END METHOD.
END CLASS.
