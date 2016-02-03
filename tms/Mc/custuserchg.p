/* ------------------------------------------------------
  MODULE .......: custuserchg.p
  FUNCTION .....: change customer's web id and password
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 27.01.06
  MODIFIED .....: 13.03.06/aam change also without sending letter
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}

DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO. 
DEF INPUT PARAMETER ilLetter  AS LOG  NO-UNDO.  /* send letter */

DEF VAR llOk        AS LOG  NO-UNDO.
DEF VAR liCreated   AS INT  NO-UNDO.
DEF VAR lcError     AS CHAR NO-UNDO. 
DEF VAR lcLetterRow AS CHAR NO-UNDO EXTENT 2.
DEF VAR lcDispPwd   AS CHAR NO-UNDO.
DEF VAR lcAskPwd    AS CHAR NO-UNDO. 

/* is there a change request already */
IF fPendingRequest(iiCustNum,5) THEN DO:
   MESSAGE "There is a pending request for user account change."
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END. 
                   

FORM
   SKIP(1)
   "A new user account will be created for customer,"  AT 5 SPACE(5) SKIP
   "i.e. both login id and password will be changed."  AT 5 SKIP(1) 
   lcLetterRow[1] FORMAT "X(50)" NO-LABEL              AT 5 SKIP
   lcLetterRow[2] FORMAT "X(50)" NO-LABEL              AT 5 SKIP(1)
   WITH ROW 7 OVERLAY SIDE-LABELS CENTERED 
        TITLE " CHANGE USER ACCOUNT, CUSTOMER " + STRING(iiCustNum) + " " 
        FRAME fCriter.

IF ilLetter THEN ASSIGN
   lcLetterRow[1] = "A letter containing new user account data will be"
   lcLetterRow[2] = "sent to customer.".
ELSE ASSIGN 
   lcLetterRow[1] = "User account letter will not be sent to customer."
   lcLetterRow[2] = "".

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

lcDispPwd = fCParamC("UserAccountPwd").

IF NOT ilLetter AND lcDispPwd > "" THEN DO:

   lcAskPwd = "".
   
   PAUSE 0.
   UPDATE lcAskPwd 
      BLANK
      FORMAT "X(20)" 
      LABEL "Password"
      HELP "Password for changing user account data"
   WITH OVERLAY ROW 10 CENTERED TITLE " CHANGE USER ACCOUNT " 
      SIDE-LABELS FRAME fPass.
   HIDE FRAME fPass NO-PAUSE.
 
   IF lcAskPwd NE lcDispPwd THEN DO:
      HIDE FRAME fCriter NO-PAUSE.
      RETURN.
   END. 
END. 


lCustMark:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO lCustMark, NEXT lCustMark:

   PAUSE 0.
   DISPLAY lcLetterRow WITH FRAME fCriter.

   ASSIGN
      ufk   = 0  
      ufk[5]= 1027  
      ufk[8]= 8 
      ehto = 0.
   RUN ufkey.

   IF toimi = 5 THEN DO:

      llOk = FALSE.
      MESSAGE "Customer's user account data will be changed." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
      
      liCreated = fUserAccountRequest(iiCustNum,
                                      IF ilLetter 
                                      THEN "loginpwd"
                                      ELSE "chglogin",
                                      "",
                                      OUTPUT lcError).
                                      
      IF liCreated > 0 THEN 
         MESSAGE "Request was created with ID" liCreated
         VIEW-AS ALERT-BOX INFORMATION.
         
      ELSE 
         MESSAGE "Request could not be created;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.

      LEAVE.
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. /* lCustMark */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

