/* ------------------------------------------------------
  MODULE .......: custuserid.p
  FUNCTION .....: customer's web id and password
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 18.01.06
  MODIFIED .....: 02.02.06/aam Active 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/fwebuser.i}
{Func/cparam2.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 

DEF VAR llOk      AS LOG  NO-UNDO.
DEF VAR lcUID     AS CHAR NO-UNDO.
DEF VAR lcUPwd    AS CHAR NO-UNDO.
DEF VAR lcDispPwd AS CHAR NO-UNDO.
DEF VAR lcAskPwd  AS CHAR NO-UNDO. 
DEF VAR liActive  AS INT  NO-UNDO.

FORM
   SKIP(1)
   lcUID COLON 18
      LABEL "User ID"
      FORMAT "X(30)"
      SKIP
   lcUPwd COLON 18   
      LABEL "Password"
      FORMAT "X(30)"
      SKIP
   liActive COLON 18
      FORMAT "9"
      LABEL "Selfcare Status"
   SKIP(1)

   WITH ROW 7 OVERLAY SIDE-LABELS CENTERED 
        TITLE " USER ACCOUNT, CUSTOMER " + STRING(iiCustNum) + " " 
        FRAME fCriter.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

lcDispPwd = fCParamC("UserAccountPwd").
IF lcDispPwd > "" THEN DO:

   lcAskPwd = "".
   
   PAUSE 0.
   UPDATE lcAskPwd 
      BLANK
      FORMAT "X(20)" 
      LABEL "Password"
      HELP "Password for viewing user account data"
   WITH OVERLAY ROW 10 CENTERED TITLE " VIEW USER ACCOUNT " 
      SIDE-LABELS FRAME fPass.
   HIDE FRAME fPass NO-PAUSE.
 
   IF lcAskPwd NE lcDispPwd THEN DO:
      HIDE FRAME fCriter NO-PAUSE.
      RETURN.
   END. 

   /* write to eventlog */
   DO FOR EventLog TRANS:
      CREATE EventLog.
      ASSIGN EventLog.TableName      = "UserID"
             EventLog.Key            = STRING(iiCustNum)
             EventLog.EventDate      = TODAY
             EventLog.EventTime      = STRING(TIME,"hh:mm:ss")
             EventLog.UserCode       = katun
             EventLog.Action         = "View"
             EventLog.EventLogStatus = 5.
      RELEASE EventLog.
   END. 
END. 

get_account_data(iiCustNum,
                 OUTPUT lcUID,
                 OUTPUT lcUPwd).
IF AVAILABLE UserAccount THEN liActive = UserAccount.Active.
            
lCustMark:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO lCustMark, NEXT lCustMark:

   PAUSE 0.
   DISPLAY lcUID
           lcUPwd 
           liActive
   WITH FRAME fCriter.

   ASSIGN
      ufk   = 0  
      ufk[8]= 8 
      ehto = 0.
   RUN ufkey.

   IF toimi = 8 THEN LEAVE.

END. /* lCustMark */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

