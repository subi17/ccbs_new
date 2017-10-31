/* ----------------------------------------------------------------------
  MODULE .......: FuncRunQueue_run_ui.p
  TASK .........: Ui for starting FuncRunQueue run
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 13.04.10
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FuncRunQueue'}
{Syst/tmsconst.i}

DEF VAR liFRQueueID     AS INT  NO-UNDO.
DEF VAR liFRQScheduleID AS INT  NO-UNDO.
DEF VAR llRunMode         AS LOG  NO-UNDO.
DEF VAR llOk              AS LOG  NO-UNDO.

FORM 
   SKIP(4)
   "Start a function queue run according to configuration." AT 11 SKIP(1)
   liFRQueueID  COLON 25 
      LABEL "Function Queue"
      FORMAT ">>>9"
      HELP "Queue to be handled"
      SKIP
   FuncRunQueue.QueueDesc COLON 25
      FORMAT "X(50)"
      NO-LABEL 
      SKIP
   liFRQScheduleID COLON 25 
      LABEL "Function Queue Schedule" 
      FORMAT ">>>9"
      HELP "Instance to be handled"
   llRunMode COLON 25
      LABEL "Run Mode"
      HELP "Run mode, (P)roduction or (T)est"
      FORMAT "Production/Test"
   SKIP(7)
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.CUICommon:ynimi + "  RUN FUNCTION QUEUE " + STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.

FUNCTION fDispFuncRunQueue RETURNS LOGIC
   (iiFRQueueID AS INT):

   FIND FIRST FuncRunQueue WHERE 
      FuncRunQueue.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
   IF AVAILABLE FuncRunQueue THEN DO:
      DISPLAY FuncRunQueue.QueueDesc WITH FRAME fCrit.
      RETURN TRUE.
   END.
   
   ELSE RETURN FALSE.

END FUNCTION.


ASSIGN
   Syst.CUICommon:toimi     = -1
   llRunMode = TRUE.

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   CLEAR FRAME fCrit ALL NO-PAUSE.
   DISPLAY 
      liFRQueueID 
      liFRQScheduleID 
      llRunMode
   WITH FRAME fCrit.
   
   IF liFRQueueID > 0 THEN DO:
      fDispFuncRunQueue(liFRQueueID).
   END.

   IF Syst.CUICommon:toimi < 0 THEN Syst.CUICommon:toimi = 1.
   ELSE DO:
      ASSIGN
         Syst.CUICommon:ufk    = 0
         Syst.CUICommon:ufk[1] = 7
         Syst.CUICommon:ufk[5] = 795
         Syst.CUICommon:ufk[8] = 8 
         Syst.CUICommon:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   
   IF Syst.CUICommon:toimi = 1 THEN 
   REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

      Syst.CUICommon:ehto = 9.
      RUN Syst/ufkey.p.
    
      UPDATE liFRQueueID liFRQScheduleID WITH FRAME fCrit 
      EDITING:
         READKEY.

         IF KEYLABEL(LASTKEY) = "F9" AND 
            LOOKUP(FRAME-FIELD,"liFRQueueID,liFRQScheduleID") > 0
         THEN DO:

            IF FRAME-FIELD = "liFRQueueID" THEN DO:
               ASSIGN
                  si-recid    = ?
                  gcHelpParam = "FRQueueID".
               RUN Syst/funcrunqueue.p.
               gcHelpParam = "".
            
               IF si-recid NE ? THEN DO:
                  FIND FuncRunQueue WHERE RECID(FuncRunQueue) = si-recid 
                     NO-LOCK NO-ERROR.
                  IF AVAILABLE FuncRunQueue THEN 
                     DISP FuncRunQueue.FRQueueID @ liFRQueueID 
                        WITH FRAME fCrit.
               END.
            END.
            
            ELSE IF FRAME-FIELD = "liFRQScheduleID" THEN DO:
               ASSIGN
                  si-recid    = ?
                  gcHelpParam = "FRQScheduleID".
               RUN Syst/funcrunqschedule.p (INPUT INPUT FRAME fCrit liFRQueueID).
               gcHelpParam = "".
            
               IF si-recid NE ? THEN DO:
                  FIND FuncRunQSchedule WHERE 
                     RECID(FuncRunQSchedule) = si-recid NO-LOCK NO-ERROR.
                  IF AVAILABLE FuncRunQSchedule THEN 
                     DISP FuncRunQSchedule.FRQScheduleID @ liFRQScheduleID 
                          (FuncRunQSchedule.RunMode = "Production") @ llRunMode
                     WITH FRAME fCrit.
               END.
            END.
            
            Syst.CUICommon:ehto = 9.
            RUN Syst/ufkey.p.

            NEXT. 
         END.
         
         ELSE IF LOOKUP(KEYLABEL(LASTKEY),Syst.CUICommon:poisnap) > 0 THEN 
         DO WITH FRAME fCrit:
         
            PAUSE 0.

            IF FRAME-FIELD = "liFRQueueID" THEN DO:
               IF NOT fDispFuncRunQueue(INPUT INPUT liFRQueueID) THEN DO:
                  MESSAGE "Unknown queue"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
         END.
      
         APPLY LASTKEY.
      END.
   
      LEAVE.
   
   END.
   
   ELSE IF Syst.CUICommon:toimi = 5 THEN DO:
   
      FIND FIRST FuncRunQSchedule WHERE 
         FuncRunQSchedule.FRQScheduleID = liFRQScheduleID 
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FuncRunQSchedule THEN DO:
         MESSAGE "Unknown queue instance"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
                                            
      IF FuncRunQSchedule.RunState NE "Scheduled" THEN DO:
         MESSAGE "Instance has already been handled"
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END.
      
      llOk = FALSE.
      MESSAGE "Start handling the queue?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.
      IF NOT llOk THEN NEXT. 
        
      RUN Syst/funcrunqueue_run.p(liFRQueueID,
                             liFRQScheduleID).

      MESSAGE "Queue has been launched." +
              (IF RETURN-VALUE > "" 
               THEN CHR(10) + RETURN-VALUE
               ELSE "")
      VIEW-AS ALERT-BOX 
      TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.CUICommon:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

