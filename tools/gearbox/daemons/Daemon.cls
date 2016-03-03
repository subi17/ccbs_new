ROUTINE-LEVEL ON ERROR UNDO, THROW.

&IF DEFINED(TIME_SOURCE) EQ 0 &THEN
   &GLOBAL-DEFINE TIME_SOURCE MTIME
&ENDIF

/**
 * A monitored interruptable daemon process.
 */
CLASS gearbox.daemons.Daemon:

   DEF PUBLIC VAR lDaemonName AS CHAR NO-UNDO.
   DEF PUBLIC VAR lMonitor AS CLASS gearbox.daemons.Monitor NO-UNDO.
   DEF PROTECTED VAR lLastHeartbeat AS INT INITIAL ? NO-UNDO.
   DEF PROTECTED VAR lHeartbeatRate AS INT INITIAL 5000 NO-UNDO.
   DEF PRIVATE VAR lConsecutiveErrors AS INT NO-UNDO.
   DEF PROTECTED VAR lMaxConsecutiveErrors AS INT INITIAL 5 NO-UNDO.
   DEF PROTECTED VAR lTotalIterations AS INT64 NO-UNDO.

   METHOD PUBLIC VOID setUp ():
   END METHOD.

   METHOD PUBLIC VOID shutDown ():
   END METHOD.

   /** ABSTRACT
    * This method must be implemented by each daemon to do a small action.
    * It has to return often for heartbeats to be sent, but only if
    * it returns FALSE will outside shutdown requests be executed. */
   METHOD PUBLIC LOG processOne ():
      UNDO, THROW NEW Progress.Lang.AppError("Not implemented", 300).
   END METHOD.

   /** Will pause execution for iMaxSecs seconds or until the next
    * heartbeat must be sent. Note that Progress ABL can only pause
    * seconds, so this will usually return AFTER the heartbeat time.
    * Also note, that the lHeartbeatRate is milliseconds. */
   METHOD PROTECTED VOID pauseUntilHeartbeat ( iMaxSecs AS INT ):
      DEF VAR lMillisecondsToWait AS INT NO-UNDO.
      IF lLastHeartbeat GT {&TIME_SOURCE} THEN
         lLastHeartbeat = lLastHeartbeat - (24 * 60 * 60 * 1000).
      lMillisecondsToWait = MIN(lHeartbeatRate,
                            lHeartbeatRate - ({&TIME_SOURCE} - lLastHeartbeat)).
      PAUSE MIN(iMaxSecs, TRUNCATE(lMillisecondsToWait / 1000, 0) + 1).
   END METHOD.

   /** Calls processOne, until that returns FALSE or the STOP condition
    * occurs. If the time is right, it will send the heartbeat to lMonitor. */
   METHOD PUBLIC VOID heartbeatLoop ():
      DEF VAR lBatchIterations AS INT64 NO-UNDO.
      DEF VAR lContinue AS LOG NO-UNDO.
      DEF VAR lTime AS INT NO-UNDO.
      lBatchIterations = 0.
      REPEAT:
         IF lLastHeartbeat GT {&TIME_SOURCE} THEN
            lLastHeartbeat = lLastHeartbeat - (24 * 60 * 60 * 1000).
         IF ({&TIME_SOURCE} - lLastHeartbeat) GE lHeartbeatRate THEN DO:
            lMonitor:sendHeartbeat().
            lLastHeartbeat = {&TIME_SOURCE}.
         END.
         lContinue = ?.
         DO ON ERROR UNDO, THROW:
            lTime = {&TIME_SOURCE}.
            lContinue = processOne().
            lConsecutiveErrors = 0.
            CATCH pe AS Progress.Lang.Error:
               lConsecutiveErrors = lConsecutiveErrors + 1.
               DEF VAR exceptionClass AS CLASS Progress.Lang.Class NO-UNDO.
               DEF VAR liExcMsgCounter AS INT NO-UNDO.
               exceptionClass = pe:getClass().
               LOG-MANAGER:WRITE-MESSAGE("Exception: " +
                                          exceptionClass:TypeName, "BOOT").
               DO liExcMsgCounter = 1 TO pe:numMessages:
                   LOG-MANAGER:WRITE-MESSAGE(pe:getmessage(liExcMsgCounter),
                                          "ERROR").
               END.
               DELETE OBJECT pe.
            END CATCH.
            FINALLY:
               lTime = {&TIME_SOURCE} - lTime.
               IF lTime LT 0 THEN
                  lTime = 24 * 60 * 60 * 1000 + lTime.
            END FINALLY.
         END.
         IF lContinue THEN DO:
            ASSIGN
               lTotalIterations = lTotalIterations + 1
               lBatchIterations = lBatchIterations + 1.
            lMonitor:updateStats(lTime, lBatchIterations, lTotalIterations).
         END. ELSE IF lContinue EQ ? THEN DO:
            lMonitor:updateStats(lTime, lBatchIterations, lTotalIterations).
            LEAVE.
         END. ELSE LEAVE.
      END.
   END METHOD.

   /** The main loop of the daemon. It calls the heartbeatLoop until
    * either the STOP condition occurred, lMaxConsecutiveErrors
    * exceptions happened or the iDieFile exists. */
   METHOD PUBLIC FINAL VOID mainLoop ( iDieFile AS CHAR ):
      IF NOT VALID-OBJECT(lMonitor) THEN
          lMonitor = NEW gearbox.daemons.FileMonitor(lDaemonName).
      lLastHeartbeat = 0 - lHeartbeatRate.
      lTotalIterations = 0.
      setUp().
      REPEAT ON STOP UNDO, LEAVE:
         IF SEARCH(iDieFile) NE ? THEN LEAVE.
         heartbeatLoop().
         IF lMaxConsecutiveErrors GT 0
         AND lConsecutiveErrors GE lMaxConsecutiveErrors THEN LEAVE.
      END.
      shutDown().
      IF VALID-OBJECT(lMonitor) THEN
         DELETE OBJECT lMonitor.
   END METHOD.

END CLASS.
