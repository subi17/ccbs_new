/* ----------------------------------------------------------------------
  MODULE .......: callscannerdel.p 
  TASK .........: Delete CallScanner logs from day before yesterday
  APPLICATION ..: tms
  AUTHOR .......: Janne Tourunen
  CREATED ......: 09.11.2012
  Version ......: 1.0
---------------------------------------------------------------------- */

DEF VAR liNumbOfDeleted AS INT NO-UNDO. 
DEF VAR ldStartTime AS DEC NO-UNDO.
DEF VAR ldEndTime AS DEC NO-UNDO.
{Syst/tmsconst.i}
{Func/timestamp.i}

ldStartTime = fHMS2TS(today - 2, "00:00:00").
ldEndTime   = fHMS2TS(today - 1, "00:00:00") .


FOR EACH CallScanner EXCLUSIVE-LOCK WHERE
         CallScanner.TMSTime >= ldStartTime AND
         CallScanner.TMSTime < ldEndTime.
   DELETE CallScanner.
   liNumbOfDeleted = liNumbOfDeleted + 1.
END.

CREATE ActionLog.
ASSIGN
   ActionLog.Brand = "1"
   ActionLog.UserCode = "CRON"
   ActionLog.ActionID = "CallScannerDelete"
   ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
   ActionLog.TableName = "CallScanner"
   ActionLog.ActionChar = STRING(liNumbOfDeleted)
   ActionLog.FromDate = TODAY - 2
   ActionLog.ToDate = TODAY - 2
   ActionLog.ActionTS = fMakeTS().
   RELEASE ActionLog.

