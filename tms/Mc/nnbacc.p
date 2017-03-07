/* ----------------------------------------------------------------------
  MODULE .......: nnbacc.p
  TASK .........: checks FORMAT of a bank account no.
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 08-09-99
  CHANGED ......: 
  VERSION ......: M15
  ---------------------------------------------------------------------- */
{Syst/commali.i}

DEF INPUT  PARAMETER account AS C NO-UNDO.
DEF OUTPUT PARAMETER rc      AS I NO-UNDO.

DEF VAR msg      AS C  NO-UNDO. 
DEF VAR numeric  AS LO NO-UNDO.
DEF VAR i        AS I  NO-UNDO.
DEF VAR len_err  AS LO NO-UNDO.

numeric = TRUE.
DO i = 1 TO LENGTH(account).
   IF i NE 5 THEN
   IF INDEX("0123456789",SUBSTR(account,i,1)) = 0 
   THEN numeric = FALSE.
END.

IF NOT numeric 
THEN msg = "Invalid characters in Bank Account No !".

/* check LENGTH of account */
len_err = FALSE.

IF account BEGINS "3" THEN DO:
   IF LENGTH(account) NE 12 AND 
      LENGTH(account) NE 15  THEN len_err = TRUE.
END.
ELSE IF account BEGINS "6" THEN DO:
   IF (LENGTH(account) < 13   OR
       LENGTH(account) > 14)  THEN len_err = TRUE.
END.
ELSE IF account BEGINS "8" THEN DO:
   IF LENGTH(account) < 6    THEN len_err = TRUE.
END.
ELSE DO:  /* other banks */
   IF LENGTH(account) NE 12   THEN len_err = TRUE.
END.   
IF len_err THEN msg =  "Customer's Bank Account has Invalid Length !".

IF SUBSTR(account,5,1) NE "-" 
THEN msg = "There must be a DASH (-) as 5th position of Account No !".

IF msg ne "" THEN DO:
   MESSAGE msg VIEW-AS ALERT-BOX ERROR.
   rc = 1.
END.
ELSE rc = 0.  /* OK */


