DEF VAR errLocked  AS INT NO-UNDO init 445.
DEF VAR errMissing AS INT NO-UNDO init 565.

FUNCTION fIsErr RETURNS logical
  (INPUT ErrNo AS INT).

   DEF VAR i AS   i  NO-UNDO.
   DEF VAR ret AS lo NO-UNDO.

   DO i = 1 TO error-status:num-messages.
      IF ErrNo = error-status:get-number(i) THEN ret = TRUE.
   END.

   RETURN ret.

END.


