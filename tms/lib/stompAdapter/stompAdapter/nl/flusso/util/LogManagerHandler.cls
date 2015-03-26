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

&SCOPED-DEFINE DEFAULT-LOGFILE 'logger.log':U

CLASS nl.flusso.util.LogManagerHandler IMPLEMENTS LogHandler:
  DEFINE PRIVATE VARIABLE explicitLevel AS INTEGER   NO-UNDO INITIAL 4.
  DEFINE PRIVATE VARIABLE filterSystem  AS CHARACTER NO-UNDO INITIAL ?.
  
  DEFINE PRIVATE PROPERTY maximumLevel AS INTEGER NO-UNDO
    GET:
      RETURN IF explicitLevel <> ? THEN explicitLevel ELSE LOG-MANAGER:LOGGING-LEVEL * 10 + 9.
    END.
  
  
  CONSTRUCTOR PUBLIC LogManagerHandler():
    THIS-OBJECT(?, ?).
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC LogManagerHandler(maxLevel AS INTEGER):
    THIS-OBJECT(maxLevel, ?).
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC LogManagerHandler(subsystem AS CHARACTER):
    THIS-OBJECT(?, subsystem).
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC LogManagerHandler(maxLevel AS INTEGER, subsystem AS CHARACTER):
    explicitLevel = maxLevel.
    filterSystem = subsystem.
  END CONSTRUCTOR.
  
  
  DEFINE PRIVATE STATIC PROPERTY logOpen AS LOGICAL NO-UNDO
    GET:
      RETURN LOG-MANAGER:LOGFILE-NAME <> ? OR SESSION:REMOTE.
    END.
  
  
  METHOD PUBLIC VOID handleMessage(logTime AS DATETIME-TZ, logLevel AS INTEGER, logMessage AS CHARACTER, subsystem AS CHARACTER):
    IF (filterSystem = ? OR subsystem = filterSystem) AND logLevel <= maximumLevel THEN
    DO:
      IF NOT logOpen THEN
        openFile({&DEFAULT-LOGFILE}).
      LOG-MANAGER:WRITE-MESSAGE(logMessage, subsystem).
    END.
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID closeFile():
    IF NOT SESSION:REMOTE THEN
      LOG-MANAGER:CLOSE-LOG().
  END METHOD.
  
  
  METHOD PUBLIC STATIC VOID openFile(logName AS CHARACTER):
    LOG-MANAGER:LOGFILE-NAME = logName.
  END METHOD.
END CLASS.
