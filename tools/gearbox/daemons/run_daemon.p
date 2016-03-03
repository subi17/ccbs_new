/*
 * Runs a single daemon and cleans up pid and die files.
 * Parameters: DaemonClass,Number,StateDir
 *    DaemonClass: where daemons/DaemonClass.cls exists
 *    Number: Something to distinguish instances of the same daemon
 *    StateDir: location of the pid and die files
 */
ROUTINE-LEVEL ON ERROR UNDO,THROW.

DEF VAR lDaemonName AS CHAR NO-UNDO.
DEF VAR lDaemonClass AS CHAR NO-UNDO.
DEF VAR lStateDir AS CHAR NO-UNDO.
DEF VAR daemon AS CLASS gearbox.daemons.Daemon NO-UNDO.
DEF VAR lPidFile AS CHAR NO-UNDO.
DEF VAR lDieFile AS CHAR NO-UNDO.

ASSIGN
   lDaemonClass = ENTRY(1, SESSION:PARAMETER)
   lDaemonName  = lDaemonClass + ENTRY(2, SESSION:PARAMETER)
   lStateDir    = ENTRY(3, SESSION:PARAMETER)
   lPidFile = lStateDir + "/d-" + lDaemonName + ".pid"
   lDieFile = lStateDir + "/d-" + lDaemonName + ".die".

daemon = DYNAMIC-NEW lDaemonClass().
daemon:lDaemonName = lDaemonName.

IF LOG-MANAGER:LOGGING-LEVEL GE 2 THEN
   LOG-MANAGER:WRITE-MESSAGE("Started", "BOOT").

daemon:mainLoop(lDieFile).

IF LOG-MANAGER:LOGGING-LEVEL GE 2 THEN
   LOG-MANAGER:WRITE-MESSAGE("Finished successfully", "BOOT").

OS-DELETE VALUE(lPidFile) NO-ERROR.
OS-DELETE VALUE(lDieFile) NO-ERROR.

DELETE OBJECT daemon.

CATCH pe AS Progress.Lang.Error:
   DEF VAR exceptionClass AS CLASS Progress.Lang.Class NO-UNDO.
   DEF VAR liExcMsgCounter AS INT NO-UNDO.
   exceptionClass = pe:getClass().
   LOG-MANAGER:WRITE-MESSAGE("Finished with exception: " +
                              exceptionClass:TypeName, "BOOT").
   DO liExcMsgCounter = 1 TO pe:numMessages:
       LOG-MANAGER:WRITE-MESSAGE(pe:getmessage(liExcMsgCounter), "ERROR").
   END.
   DELETE OBJECT pe.
   OS-DELETE VALUE(lPidFile) NO-ERROR.
   OS-DELETE VALUE(lDieFile) NO-ERROR.
   UNDO, THROW NEW Progress.Lang.AppError("Return value").
END CATCH.
