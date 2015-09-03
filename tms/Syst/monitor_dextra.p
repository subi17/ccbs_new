/* ----------------------------------------------------------------------
  MODULE .......: monitor_dextra.p
  TASK .........: Used for External API monitoring (dextra). YPR-2483
  APPLICATION ..: TMS
  AUTHOR .......: ilkkasav
  CREATED ......: 31.07.15
  Version ......: Yoigo
----------------------------------------------------------------------- */
DEFINE VARIABLE liOk AS INTEGER NO-UNDO.
DEFINE VARIABLE liError AS INTEGER NO-UNDO. 
DEFINE VARIABLE liAppIdError AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldtFrom as datetime no-undo.

ASSIGN
   ldtFrom = DATETIME(TODAY,MTIME)
   ldtFrom = ldtFrom - 300000. /* -5 mins */

FOR EACH authlog NO-LOCK WHERE
         authlog.timestamp > ldtFrom AND
         authlog.username = "dextra" USE-INDEX timestamp:

   IF authlog.errorcode EQ 0 THEN liOK = liOK + 1.
   ELSE IF authlog.errorcode eq -32500 AND
           authlog.errorMsg eq "Application Id does not match" THEN
      liAppIdError = liAppIdError + 1. 
   ELSE liError = liError + 1.
END.

MESSAGE liOk liError liAppIdError.
QUIT.
