
/* ----------------------------------------------------------------------
  MODULE .......: chpasswd.p
  TASK .........: Prompt user to change TMS password
  APPLICATION ..: TMS 
  AUTHOR .......: mvi
  CREATED ......: 10.01.07 mvi
  CHANGED ......: 02.06.07 mvi Yoigo version 
  Version ......: Yoigo
---------------------------------------------------------------------- */

DEFINE OUTPUT PARAMETER olPasswordChanged AS LOGICAL NO-UNDO.


{Syst/commali.i}
{Func/fgettxt.i}
{Func/timestamp.i}
{Func/tmspass.i}

/* ennakkoilmoitusraja        PassWdExpireNotify INT 
   salasanahistorian pituus   PassWdHistory      INT
   salasana voimassaoloaika   PassWDValidDays    INT */


DEFINE TEMP-TABLE ttHistory
FIELD passwd AS CHAR.

DEFINE VARIABLE lcUserCode AS CHARACTER NO-UNDO FORMAT "X(16)". 
lcUserCode = katun.

DEFINE VARIABLE lcNewPass  AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE lcOldPass  AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE lcNewPass2 AS CHARACTER FORMAT "X(16)"  NO-UNDO. 
DEFINE VARIABLE lcCheckMessage AS CHARACTER NO-UNDO EXTENT 2. 
DEFINE VARIABLE liPassWdHistory AS INTEGER NO-UNDO INIT 4 FORMAT "9". 
DEFINE VARIABLE lcErrorTitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE liResult AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRetries AS INTEGER NO-UNDO INIT 3.
DEFINE VARIABLE liCurrent AS INTEGER NO-UNDO INIT 0. 
DEFINE VARIABLE liHistoryCount AS INTEGER NO-UNDO.

{Func/tmsparam.i PassWdHistory} liPassWdHistory = TMSParam.IntVal.

lcCheckMessage[1] = fGetTxt("GENERAL","PassWdCheck1", TODAY, 5). /* english */
lcCheckMessage[2] = fGetTxt("GENERAL","PassWdCheck2", TODAY, 5). /* english */
lcErrorTitle    = " CHECK THE PASSWORD ".                         


FORM 
SKIP(1)
"            Enter your current password and new password twice." 
SKIP(1)
"   Requirements for the password:  - Length must be 8-16 characters" 
SKIP
"                                   - Must contain an upper case letter" 
SKIP
"                                   - Must contain a number" 
SKIP
"                                   - Must differ from last" 
                                    liPassWdHistory 
                                    "passwords" 
SKIP
"                                   - allowed chars [a-z][A-Z][0-9] and '_'" 
SKIP
"                                   - one character can occur only 3 times"
SKIP(1)
"                      Username     : " lcUserCode  SKIP
"                      Old password : " lcOldPass   SKIP(1)
"                      New password : " lcNewPass   SKIP
"                      New password : " lcNewPass2  SKIP(1)

WITH 
   WIDTH 80 ROW 2 
     TITLE " " + ynimi + " PASSWORD CHANGE "  +
           STRING(pvm,"99-99-99") + " "
   NO-LABELS
FRAME passFrame.

/* --------------------------------------------------------------------
   Start main program 
   -------------------------------------------------------------------- */
   
FIND FIRST tmsuser NO-LOCK WHERE 
           tmsuser.usercode = katun.
           
/* get the user's password history in temptable */
liHistoryCount = 0.
FOR EACH tmspass NO-LOCK WHERE   
         tmspass.usercode = tmsuser.usercode:
   liHistoryCount = liHistoryCount + 1.
   CREATE ttHistory.
   ttHistory.passwd = tmspass.password.
END.         



DISPLAY liPassWdHistory lcUserCode WITH FRAME passFrame.
 
ufk = 0.
ufk[8] = 8.
ehto = 3.

mainLoop:
REPEAT:

   lcNewPass = "".
   lcNewPass2 = "".
   lcOldPass = "".
   
   RUN Syst/ufkey.
   
   passLoop:
   REPEAT WITH FRAME passFrame ON ENDKEY UNDO passLoop, NEXT mainLoop:

      lcOldPass:BLANK = YES.
      lcNewPass:BLANK = YES.
      lcNewPass2:BLANK = YES.
      
      UPDATE lcOldPass lcNewPass lcNewPass2 WITH FRAME passFrame EDITING:
         
         READKEY.
         nap = KEYLABEL(LASTKEY).

         IF LOOKUP(nap,"f4,f8") > 0 THEN DO:
            olPasswordChanged = FALSE.
            HIDE FRAME passFrame.
            RETURN.
         END.
         
         
         IF LOOKUP(nap,poisnap) > 0 THEN DO:
            
            IF FRAME-FIELD = "lcOldPass" THEN DO:
            END.
            
            IF FRAME-FIELD = "lcNewPass" THEN DO:
            END.
            
            IF FRAME-FIELD = "lcNewPass2" THEN DO:
               IF lcNewPass2:SCREEN-VALUE NE lcNewPass:SCREEN-VALUE THEN DO:
                  MESSAGE "Passwords do not match!" 
                     VIEW-AS ALERT-BOX ERROR.
                  NEXT mainLoop.
               END.    
               
               liResult = fCheckPassword(lcNewPass:SCREEN-VALUE).
                 
               IF liResult > 0 THEN DO:
                  MESSAGE lcCheckMessage[liResult] VIEW-AS ALERT-BOX 
                     TITLE " CHECK NEW PASSWORD ".
                  NEXT mainLoop.
               END.    
               
               
               FIND FIRST tmspass NO-LOCK WHERE 
                          tmspass.usercode = tmsuser.usercode.
               
               
               
               IF lcOldPass:SCREEN-VALUE NE tmspass.password THEN DO:
                  liCurrent = liCurrent + 1.
                  IF liCurrent >= liRetries THEN DO:
                     MESSAGE "Too many attempts!" VIEW-AS ALERT-BOX ERROR.
                     olPasswordChanged = FALSE.
                     HIDE FRAME passFrame.
                     RETURN.
                  END.
                  ELSE DO: 
                     MESSAGE "Incorrect password!" VIEW-AS ALERT-BOX ERROR.
                     NEXT mainLoop.
                  END.
               END.
               
               /* check that history doesn't contain the new one */
               FIND FIRST ttHistory WHERE 
                          ttHistory.passwd = lcNewPass:SCREEN-VALUE
                          NO-ERROR.
               IF AVAIL ttHistory THEN DO:
                  MESSAGE lcCheckMessage[2] VIEW-AS ALERT-BOX 
                     TITLE " CHECK NEW PASSWORD ".
                  NEXT mainLoop.
               END.
               
               /* change password */
               IF liHistoryCount < liPassWdHistory THEN DO:
                  /* user has not yet filled the history so create new */
                  CREATE tmspass.
                  ASSIGN
                     tmspass.createts = fMakeTS()
                     tmspass.usercode = tmsuser.usercode
                     tmspass.creator  = tmsuser.usercode
                     tmspass.password = lcNewPass:SCREEN-VALUE.
               END.
               ELSE DO:
                  /* user has already full history, replace oldest */
                  FIND LAST tmspass EXCLUSIVE-LOCK WHERE  
                            tmspass.usercode = tmsuser.usercode.
                  ASSIGN     
                     tmspass.createts = fMakeTS()
                     tmspass.creator  = tmsuser.usercode
                     tmspass.password = lcNewPass:SCREEN-VALUE.
               END.
               MESSAGE "Password changed!" VIEW-AS ALERT-BOX INFO.
               HIDE FRAME passFrame.
               olPasswordChanged = TRUE. 
               RETURN.
            
            END.   
         END.
         APPLY LASTKEY.
      END.
   END.
END.

/* if for some reason we will come here then return false */
olPasswordChanged = FALSE.
HIDE FRAME passFrame.
RETURN.

