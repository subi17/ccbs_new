ROUTINE-LEVEL ON ERROR UNDO, THROW.

&SCOPED-DEFINE AVERAGE_OVER 10

CLASS gearbox.daemons.FileMonitor IMPLEMENTS gearbox.daemons.Monitor:

   DEF PRIVATE STREAM lLog.
   DEF PUBLIC VAR lLogName AS CHAR NO-UNDO.
   DEF PRIVATE VAR lTimeAverage AS INT NO-UNDO.

   CONSTRUCTOR PUBLIC FileMonitor ( iName AS CHAR ):
      lLogName = SUBST("../var/run/d-&1.monitor", iName).
   END CONSTRUCTOR.

   /* Updates the change time of the log file */
   METHOD PUBLIC VOID sendHeartbeat ():
      UNIX SILENT touch VALUE(lLogName).
   END METHOD.

   /* Logs a . for each iteration (* on start of batch) and every 1000
    * iterations some kind of an average of execution times */
   METHOD PUBLIC VOID updateStats ( iExecutionTimeMilliSeconds AS INT,
                                    iBatchIterations AS INT64,
                                    iTotalIterations AS INT64 ):
      IF lTimeAverage EQ 0 OR lTimeAverage EQ ? THEN
          lTimeAverage = iExecutionTimeMilliSeconds.
      ELSE IF iExecutionTimeMilliSeconds NE ? THEN
          lTimeAverage = (lTimeAverage * ({&AVERAGE_OVER} - 1) +
                          iExecutionTimeMilliSeconds) / {&AVERAGE_OVER}.
      OUTPUT STREAM lLog TO VALUE(lLogName) APPEND UNBUFFERED.
      IF iBatchIterations EQ 1 THEN
         PUT STREAM lLog UNFORMATTED "*".
      ELSE
         PUT STREAM lLog UNFORMATTED ".".
      IF iTotalIterations MOD 1000 EQ 0 THEN
         PUT STREAM lLog UNFORMATTED " " lTimeAverage SKIP.
      OUTPUT STREAM lLog CLOSE.
   END METHOD.

END CLASS.
