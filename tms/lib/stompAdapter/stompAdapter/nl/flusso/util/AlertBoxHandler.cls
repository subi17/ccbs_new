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

CLASS nl.flusso.util.AlertBoxHandler IMPLEMENTS LogHandler:
  DEFINE PRIVATE VARIABLE maximumLevel AS INTEGER NO-UNDO.
  DEFINE PRIVATE VARIABLE filterSystem AS CHARACTER NO-UNDO INITIAL ?.
  
  
  CONSTRUCTOR PUBLIC AlertBoxHandler():
    THIS-OBJECT(?, ?).
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC AlertBoxHandler(maxLevel AS INTEGER):
    THIS-OBJECT(maxLevel, ?).
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC AlertBoxHandler(subsystem AS CHARACTER):
    THIS-OBJECT(?, subsystem).
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC AlertBoxHandler(maxLevel AS INTEGER, subsystem AS CHARACTER):
    maximumLevel = IF maxLevel = ? THEN Logger:debugALL ELSE maxLevel.
    filterSystem = subsystem.
  END CONSTRUCTOR.
  
  
  METHOD PUBLIC VOID handleMessage(logTime AS DATETIME-TZ, logLevel AS INTEGER, logMessage AS CHARACTER, subsystem AS CHARACTER):
    DEFINE VARIABLE logTitle AS CHARACTER NO-UNDO.
    IF logLevel <= maximumLevel AND (filterSystem = ? OR subsystem = filterSystem) THEN
    DO:
      logTitle = subsystem.
      IF logLevel = Logger:ERROR THEN
        MESSAGE logMessage VIEW-AS ALERT-BOX ERROR TITLE logTitle.
      ELSE IF logLevel = Logger:WARNING THEN
        MESSAGE logMessage VIEW-AS ALERT-BOX WARNING TITLE logTitle.
      ELSE IF logLevel = Logger:INFORMATION THEN
        MESSAGE logMessage VIEW-AS ALERT-BOX INFORMATION TITLE logTitle.
      ELSE
        MESSAGE logMessage VIEW-AS ALERT-BOX TITLE logTitle.
    END.
  END METHOD.
END CLASS.
