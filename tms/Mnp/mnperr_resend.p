/* ----------------------------------------------------------------------
MODULE .......: mnperr_resend.p 
TASK .........: Automatically resends errorneous messages
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 21.12.09
Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "MNPResend".
gcBrand = "1".

{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/eventval.i}
{Func/log.i}
{Func/heartbeat.i}
llDoEvent = FALSE. /* could generate too logs */

DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTime     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPause    AS INTEGER   NO-UNDO.
DEFINE VARIABLE llNagBeat  AS LOG       NO-UNDO INIT TRUE.
DEFINE VARIABLE lcNagios   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liNagios AS INTEGER NO-UNDO.  
DEFINE VARIABLE liTimeOut  AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcURL      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liResent   AS INTEGER NO-UNDO. 
 
DEF BUFFER bMNPOperation FOR MNPOperation.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMNPOperation AS HANDLE NO-UNDO.
   lhMNPOperation = BUFFER bMNPOperation:HANDLE.
   RUN StarEventInitialize ( lhMNPOperation ).

END.

FORM
   SKIP(1)
   " Loops ......: " liLoop   FORMAT ">>>>>>>9" skip
   " Re-sent ....: " liResent FORMAT ">>>>>>>9" skip
   " Loop started: " lcTime FORMAT "X(20)"
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " MNP ERROR RESENDING " WIDTH 40 ROW 8
FRAME frmMain .

DO WHILE TRUE:

   liLoop = liLoop + 1.

   DISP
      liLoop
      liResent
      STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
   WITH FRAME frmMain.
   PAUSE 0.

   RUN pResendErrors.   

   PUT SCREEN ROW 22 COL 1
      "F8 TO QUIT, OTHER KEYS START HANDLING IMMEDIATELLY".
    
   liPause = 590.
   
   IF llNagBeat THEN fKeepAlive("mnperr_resend:MNP Auto Error Resend"). 

   READKEY PAUSE liPause.
   
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
      fCloseLog().
      IF llDoEvent THEN fCleanEventObjects().
      QUIT.
   END.
   
   PUT SCREEN ROW 22 COL 1
      "RESENDING MESSAGES ............................".

END.

PROCEDURE pResendErrors:

   DEFINE VARIABLE lcErrorCodes AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcStatusCodes AS CHARACTER NO-UNDO. 

   lcErrorCodes = {&MNP_ERRORCODE_ADAPTER} + ",GENE ERINT,AREC FSABF".
   lcStatusCodes = SUBST("&1,&2,&3,&4", {&MNP_ST_AREC}, {&MNP_ST_AREC_CLOSED}, {&MNP_ST_APOR}, {&MNP_ST_ACAN}).

   MNPPROCESS_LOOP:
   FOR EACH MNPOperation WHERE
      MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_NO} AND
      LOOKUP(MNPOperation.ErrorCode,lcErrorCodes) > 0 AND
      MNPOperation.Sender = {&MNP_SENDER_TMS} AND
      MNPOperation.StatusCode > 10 NO-LOCK,
      FIRST MNPProcess NO-LOCK WHERE
            MNPProcess.MNPSeq = MNPOperation.MNPSeq AND
            LOOKUP(STRING(MNPProcess.StatusCode),lcStatusCodes) = 0:
      
      fLogBasic("Resent " +
                MNPOperation.MessageType + ":" +
                MNPOperation.ErrorCode + ":" +
                MNPOperation.ErrorDesc + ":" +
                MNPProcess.Portrequest + ":" +
                MNPProcess.FormRequest).

      FIND bMNPOperation EXCLUSIVE-LOCK WHERE
           RECID(bMNPOperation) = RECID(MNPOperation).
      
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMNPOperation). 
      
      ASSIGN
         bMNPOperation.ErrorHandled = 0
         bMNPOperation.ErrorCode = ""
         bMNPOperation.ErrorDesc = ""
         bMNPOperation.MsgTurn = MNPOperation.MsgTurn + 1
         bMNPOperation.StatusCode = {&MNP_MSG_WAITING_SEND}.
   
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMNPOperation).

      RELEASE bMNPOperation. 
      
      liResent = liResent + 1.
   
   END.

END PROCEDURE. 
