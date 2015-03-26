/*-----------------------------------------------------------------------------
  MODULE .......: resetreqmenu.p
  FUNCTION .....: request reseting menu
  SOVELLUTUS ...: 
  AUTHOR .......: JT
  CREATED ......: 
  changePVM ....: 05.01.07/mvi cancel request set status to 4 (option 3)
                  24.01.07 kl  delete KillMs with status => 4
                  26.01.07 kl  update MsRequest.UserCode
                  02.01.06 kl  IF MsRequest.ReqStatus = 2 OR 
                               MsRequest.ReqStatus = 4
                  17.04.07/aam handle request (9), validity checks, 
                               ask user for confirmation, clear eventlog,
                               don't change original UserCode 
                  24.04.07/aam eventlog removed, caller takes care of it
                  15.05.07/mvi simchange request confirmation
                  13.08.07/aam type 13 added back to 'bypass' check
  Version ......: YoiGO
  SHARED .......: INPUT: msseq
  -------------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable MsRequest

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'OrdStat'}
{msisdn.i}
{msreqfunc.i}

DEF INPUT  PARAMETER iiMsRequest  AS INT  NO-UNDO.

DEF VAR menuc          AS CHAR NO-UNDO EXTENT 6.
DEF VAR llOk           AS LOG  NO-UNDO.
DEF VAR lcCancelReason AS CHAR NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.

IF NOT avail MsRequest THEN DO:
   MESSAGE "Request not found!" VIEW-AS ALERT-BOX.
END.

IF MsRequest.ReqStatus = 2 OR MsRequest.ReqStatus = 9 THEN DO:
   MESSAGE
      "This request can't be modified any more!" SKIP
      "Status:" MsRequest.ReqStatus
   VIEW-AS ALERT-BOX.

   RETURN.
END.

DO WHILE TRUE:

   ASSIGN 
      ufk    = 0 
      ufk[8] = 8 
      ehto   = 3. 
   RUN ufkey. 

   PAUSE 0.
   DISPLAY
   " A) Reset request (set status to 0)                    " @ menuc[1] SKIP
   " B) Reset request and bypass network (set status to 6) " @ menuc[2] SKIP
   " C) Cancel request (set status to 4)                   " @ menuc[3] SKIP
   " D) Handle request (set status to 9)"                    @ menuc[4] SKIP
   " E) Approve request (set status to 15)"                  @ menuc[5] SKIP
   " X) QUIT (F8)                                          " @ menuc[6]    
   WITH OVERLAY WIDTH 60 FRAME choices NO-LABELS.
   
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " REQUEST FUNCTIONS "  
      CENTERED WITH COL 1 ROW 6.
   
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,8,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      
      IF lcRight NE "RW" OR MsRequest.ReqStatus NE 3 THEN DO:
         MESSAGE "You do not have permission to do this!" 
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.   
      
      ELSE DO:

         llOk = FALSE.
         MESSAGE 
            "Request will be processed again."
            "Continue with status change?"
         VIEW-AS ALERT-BOX QUESTION 
         BUTTONS YES-NO 
         TITLE " REPROCESS "
         UPDATE llOk.

         IF NOT llOk THEN NEXT. 

         fReqStatus(0,"").
         
         MESSAGE "Request will be processed again, status set to 0"
         VIEW-AS ALERT-BOX TITLE " Status Changed ".
      END.

      LEAVE.
   END.

   ELSE IF FRAME-INDEX  = 2 THEN DO :
      
      IF lcRight NE "RW" OR MsRequest.ReqStatus NE 3 THEN DO:
         MESSAGE "You do not have permission to do this!" 
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.

      IF LOOKUP(STRING(MsRequest.ReqType),"1,13,18,19,15") = 0 THEN DO:
         MESSAGE "This is not a network related request"
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.
      
      ELSE DO:

         llOk = FALSE.
         MESSAGE 
            "Network handling will be bypassed."
            "Continue with status change?"
         VIEW-AS ALERT-BOX QUESTION 
         BUTTONS YES-NO 
         TITLE " NW BYPASS "
         UPDATE llOk.

         IF NOT llOk THEN NEXT. 
         
         fReqStatus(6,"").
         
         MESSAGE "Network bypassed, status set to 6" 
         VIEW-AS ALERT-BOX TITLE " Status Changed ".
      END.
   
      LEAVE.
   END.

   ELSE IF FRAME-INDEX = 3 THEN DO:

      /* cancel request */
      IF lcRight NE "RW" OR LOOKUP(STRING(MsRequest.ReqStatus),"0,19") = 0
      THEN DO:
         MESSAGE "You do not have permission to do this!" 
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.
      
      ELSE DO:

         llOk = FALSE.
         MESSAGE 
            "Request will be cancelled, i.e. it cannot be handled"
            "any further." SKIP
            "Continue with cancellation?"
         VIEW-AS ALERT-BOX QUESTION 
         BUTTONS YES-NO 
         TITLE " CANCEL "
         UPDATE llOk.

         IF NOT llOk THEN NEXT. 

         lcCancelReason = "".
         
         IF LOOKUP(STRING(MsRequest.ReqType),"13,18,19,15") > 0 AND
            (MsRequest.ReqStatus = 19 OR MsRequest.ReqStatus = 0 ) THEN DO:
            
            PAUSE 0.    
            UPDATE lcCancelReason  
            WITH TITLE " CANCELLATION REASON " 
               CENTERED ROW 10
               OVERLAY NO-LABELS
               FRAME formReqCancel.

            HIDE FRAME formReqCancel.
               
            llOk = NO.
               
            MESSAGE "Release SIM card" Msrequest.ReqCParam2 SKIP
                    "which was assigned to this request?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO 
            TITLE " SIM "
            UPDATE llOk.
               
            IF llOk THEN DO:
               
               FIND FIRST sim EXCLUSIVE-LOCK WHERE
                          sim.icc = Msrequest.ReqCParam2 NO-ERROR.
               IF NOT AVAIL sim THEN DO:
 
                  MESSAGE "Couldn't release SIM" SKIP
                          "RELEASE ICC: " Msrequest.ReqCParam2 " manually!"
                  VIEW-AS ALERT-BOX.
               END.
               ELSE DO:
                  sim.simstat = 1.
                  MESSAGE "SIM card set back to available status"
                  VIEW-AS ALERT-BOX 
                  TITLE " SIM RELEASED ".
               END.   
               
            END.
         END.

         fReqStatus(4,lcCancelReason).
         
         MESSAGE "Request cancelled, status set to 4" 
         VIEW-AS ALERT-BOX TITLE " Status Changed ".
      END.

      LEAVE.
   END.

   ELSE IF FRAME-INDEX = 4 THEN DO:

      /* mark request handled */
      IF lcRight NE "RW" OR 
            (MsRequest.ReqStatus NE 3 AND
             MsRequest.ReqStatus NE 19) THEN DO:
         MESSAGE "You do not have permission to do this!" 
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.
      
      ELSE DO:
         DEFINE VARIABLE lcConfirmMessage AS CHARACTER NO-UNDO. 
         DEFINE VARIABLE liNewStatus AS INTEGER NO-UNDO. 

         llOk = FALSE.
         IF MsRequest.ReqStatus = 3 THEN DO:
            lcConfirmMessage = 
               "Request will be mark handled, i.e. to status 9." +
               "Continue with status change?".
            liNewStatus = 9.
         END.
         ELSE IF MsRequest.ReqStatus = 19 THEN DO:
            lcConfirmMessage = 
               "Sim Change Request will be confirmed, i.e. to status 0." +
               "Continue with status change?".
            liNewStatus = 0.
         END.

         MESSAGE 
            lcConfirmMessage
         VIEW-AS ALERT-BOX QUESTION 
         BUTTONS YES-NO 
         TITLE " CONFIRM / MARK HANDLED "
         UPDATE llOk.

         IF NOT llOk THEN NEXT. 

         /* msisdn change cancel 
            (NOTE: possible msisdn quarantine time (validto)
             is currently reseted to infinite value)
         */
         IF MsRequest.ReqType   EQ 19 AND
            MsRequest.ReqStatus EQ 3  AND
            liNewStatus         EQ 9  THEN DO:
            
            DEFINE BUFFER msisdn-back FOR MSISDN.
            DEFINE VARIABLE msisdn-recid AS RECID NO-UNDO.
            
            FIND FIRST MSISDN-back NO-LOCK WHERE
               MSISDN-back.Brand = gcBrand AND
               MSISDN-back.CLI   = MsRequest.ReqCParam2 USE-INDEX CLI.
            IF MSISDN-back.StatusCode = 27 THEN DO:
               msisdn-recid = recid(msisdn-back).
               FIND NEXT MSISDN-back.
               fMakeMSIDNHistory(msisdn-recid).
               BUFFER-COPY msisdn-back 
                  EXCEPT validfrom validto actiondate
                  TO msisdn.
               msisdn.statuscode = 4.
            END.
         END.
         
         fReqStatus(liNewStatus,"").
         
         MESSAGE "Request mark as handled, status set to " liNewStatus
         VIEW-AS ALERT-BOX TITLE " Status Changed ".
      END.

      LEAVE.
   
   END.
   
   ELSE IF FRAME-INDEX = 5 THEN DO:
      
      IF lcRight NE "RW" THEN DO:
         MESSAGE "You do not have permission to do this!" 
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.

      IF LOOKUP(STRING(MsRequest.ReqType),"15") = 0 THEN DO:
         MESSAGE "This is not a ICC related request"
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.
   
      fReqStatus(0,"").  
   
      MESSAGE "Request mark as handled, status set to 15"
      VIEW-AS ALERT-BOX TITLE " Status Changed ".
   
   END.

   ELSE IF FRAME-INDEX = 6 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */


HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.


