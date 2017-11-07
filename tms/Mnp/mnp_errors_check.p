/* monitoring script to check mnp message errors */

DEFINE VARIABLE liTotal AS INTEGER NO-UNDO.
DEFINE VARIABLE ldeFrom AS DECIMAL NO-UNDO.
DEFINE VARIABLE lierrors AS INTEGER NO-UNDO.

DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE liMinutes AS INTEGER NO-UNDO.
lcParam = SESSION:PARAMETER.

IF lcParam NE "" THEN DO:
   liMinutes = INT(lcParam).
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Parameter is not integer".
      RETURN.
   END.
END.
ELSE liMinutes = 15.

ldeFrom = Func.Common:mSecOffSet(Func.Common:mMakeTS(), liMinutes * -60).

FOR EACH mnpoperation WHERE
   createdts > ldeFrom NO-LOCK:
   if mnpoperation.errorcode ne "" then lierrors = lierrors + 1.
   liTotal = liTotal + 1.
END.

message liTotal lierrors.
quit.
