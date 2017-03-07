
/* ----------------------------------------------------------------------
  MODULE .......: pwadmin.p
  TASK .........: Password administrator tool, with this program
                  password can be changed to anything. It doesn't have to
                  comply the normal password rules. (see: tmspass.i)
  APPLICATION ..: TMS 
  AUTHOR .......: mvi
  CREATED ......: 11.01.07
  CHANGED ......: 02.06.07 mvi Yoigo version 
  Version ......: Yoigo 
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}

DEFINE VARIABLE lcUsercode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldPasswd AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNewPasswd1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNewPasswd2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE llChange AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcPassword AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE llMatch AS LOGICAL NO-UNDO. 

/* --------------------------------------------------
   !!!!!!!  NOTE! STILL A LITTLE DANGEROUS !!!!!!!!! 
   Any user can change anyone's password. Might need 
   some group control to restrict users...           
   -------------------------------------------------- */
   

FORM
SKIP(2)
"          Username: " lcUsercode FORMAT "X(16)" SKIP
"          Password: " lcPassword FORMAT "X(16)" BLANK SKIP
WITH CENTERED ROW 5 WIDTH 60 NO-LABELS
TITLE " ADMINISTRATOR LOGIN "
FRAME loginFrame.


FORM
SKIP(2)
"          Leave username empty to exit...." SKIP(2)
"          Username: " lcUsercode FORMAT "X(16)" SKIP
"      New Password: " lcNewPasswd1 FORMAT "X(16)" BLANK SKIP
"      New Password: " lcNewPasswd2 FORMAT "X(16)" BLANK SKIP(3)
WITH CENTERED ROW 5 WIDTH 60 NO-LABELS
TITLE " ADMINISTRATOR PASSWORD CHANGE TOOL "
FRAME passFrame.


lcPassword = "".


UPDATE lcUsercode lcPassword WITH FRAME loginFrame.

IF lcUsercode NE katun THEN DO:
   MESSAGE "ACCESS DENIED!" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND FIRST tmsuser NO-LOCK WHERE 
           tmsuser.usercode = lcUsercode
           NO-ERROR.
IF NOT AVAIL tmsuser THEN DO:
   MESSAGE "ACCESS DENIED!" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND FIRST tmspass NO-LOCK WHERE 
           tmspass.usercode = lcUsercode
           NO-ERROR.

IF NOT AVAIL tmspass THEN DO:
   MESSAGE "ACCESS DENIED!" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.   
   

IF tmspass.password NE lcPassword THEN DO:
   MESSAGE "ACCESS DENIED!" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.


/* force char by char test... case sensitive this way */
DO liLoop = 1 TO LENGTH(lcPassword):
   IF ASC(SUBSTRING(lcPassword,liLoop,1)) NE
      ASC(SUBSTRING(tmspass.password,liLoop,1)) THEN DO:
      MESSAGE "ACCESS DENIED!" VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
END.


mainloop:
REPEAT:
   
   passloop:
   REPEAT:
      lcUsercode = "".
      lcNewPasswd2 = "".
      lcNewPasswd1 = "".
      
      UPDATE lcUsercode WITH FRAME passFrame EDITING:
         READKEY.
         nap = KEYLABEL(LASTKEY).
         IF LOOKUP(nap,"F4,F8") > 0 THEN LEAVE mainloop.
         APPLY LASTKEY.
      END.
      
      IF lcUserCode = "" THEN DO:
         RETURN.
      END.
     
      
      FIND FIRST tmspass NO-LOCK WHERE 
                 tmspass.usercode = lcUsercode
                 NO-ERROR.
      IF NOT AVAIL tmspass THEN DO:
         MESSAGE "User does not exist!" VIEW-AS ALERT-BOX ERROR.
         NEXT mainloop.
      END.

      UPDATE lcNewPasswd1 lcNewPasswd2 WITH FRAME passFrame.

      llMatch = TRUE.
      
      IF LENGTH(lcNewPasswd1) NE LENGTH(lcNewPasswd2) THEN llMatch = FALSE.
      
      DO liLoop = 1 TO LENGTH(lcNewPasswd1):
         IF ASC(SUBSTRING(lcNewPasswd1,liLoop,1)) NE
            ASC(SUBSTRING(lcNewPasswd2,liLoop,1)) THEN DO:
            llMatch = FALSE.    
            LEAVE.
         END.
      END.
      
      IF llMatch THEN DO:
         
         MESSAGE "Force password change?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE llChange.

         IF llChange THEN DO TRANS:
            FIND CURRENT tmspass EXCLUSIVE-LOCK.
            ASSIGN
               tmspass.createts = fMakeTS()
               tmspass.creator  = katun
               tmspass.password = lcNewPasswd1.
            RELEASE tmspass. 
            MESSAGE "Password changed! User must change it on first login."
            VIEW-AS ALERT-BOX.
         END.
      END.
      ELSE DO:
         MESSAGE "Passwords do not match. Try again.." 
         VIEW-AS ALERT-BOX.
      END.

   END.

END.

