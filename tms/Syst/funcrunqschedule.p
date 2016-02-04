/* ----------------------------------------------------------------------
  MODULE .......: FuncRunQSchedule.p
  TASK .........: UPDATEs table FuncRunQSchedule
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 12.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FuncRunQSchedule'}
{Syst/eventval.i}
{Func/timestamp.i}
{Syst/tmsconst.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFuncRunQSchedule AS HANDLE NO-UNDO.
   lhFuncRunQSchedule = BUFFER FuncRunQSchedule:HANDLE.
   RUN StarEventInitialize(lhFuncRunQSchedule).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhFuncRunQSchedule).
   END.

END.

DEF INPUT PARAMETER iiFRQueueID AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

DEF VAR lcStatus        AS CHAR NO-UNDO.
DEF VAR lcField         AS CHAR NO-UNDO. 
DEF VAR lcCode          AS CHAR NO-UNDO. 
DEF VAR lcQueueDesc     AS CHAR NO-UNDO.
DEF VAR lcStartTime     AS CHAR NO-UNDO.
DEF VAR lcDoneTime      AS CHAR NO-UNDO.
DEF VAR liSeq           AS INT  NO-UNDO.
DEF VAR ldaUpdStartDate AS DATE NO-UNDO.
DEF VAR lcUpdStartTime  AS CHAR NO-UNDO.

FORM
    FuncRunQSchedule.FRQScheduleID COLUMN-LABEL "ID" 
       HELP "Unique ID for schedule"
    lcStartTime               FORMAT "X(19)" COLUMN-LABEL "Scheduled"
    FuncRunQSchedule.RunMode  FORMAT "X(10)" 
    FuncRunQSchedule.RunState FORMAT "X(13)" COLUMN-LABEL "Status"
    lcDoneTime                FORMAT "X(19)" COLUMN-LABEL "Done"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " SCHEDULING QUEUE " + STRING(iiFRQueueID) + " "
    FRAME sel.

FORM
    FuncRunQSchedule.FRQueueID     COLON 20
       lcQueueDesc NO-LABEL FORMAT "X(30)" SKIP(1)

    FuncRunQSchedule.FRQScheduleID  COLON 20
    FuncRunQSchedule.RunMode   COLON 20
    ldaUpdStartDate            COLON 20
       FORMAT "99-99-9999" 
       LABEL "Scheduled"
       HELP "Day when queue will be run"
    lcUpdStartTime  
       FORMAT "X(5)" 
       NO-LABEL 
       HELP "Time (hh:mm) when queue will be run" 
       SPACE(4)
    FuncRunQSchedule.StartTS NO-LABEL SKIP(1)
    
    FuncRunQSchedule.RunState COLON 20 LABEL "Status"
    lcDoneTime                COLON 20 
       FORMAT "X(19)" LABEL "Done" 
       FuncRunQSchedule.DoneTS NO-LABEL SKIP
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FIND FIRST FuncRunQueue WHERE FuncRunQueue.FRQueueID = iiFRQueueID 
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE FuncRunQueue THEN DO:
   MESSAGE "Queue not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
lcQueueDesc = FuncRunQueue.QueueDesc.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE FuncRunQSchedule THEN ASSIGN
   Memory       = recid(FuncRunQSchedule)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No scheduling available" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a FuncRunQSchedule  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY iiFRQueueID @ FuncRunQSchedule.FRQueueID.

           liSeq = 1.
           FIND LAST FuncRunQSchedule USE-INDEX FRQScheduleID 
              NO-LOCK NO-ERROR.
           IF AVAILABLE FuncRunQSchedule THEN 
              liSeq = FuncRunQSchedule.FRQScheduleID + 1.
           
           CREATE FuncRunQSchedule.
           ASSIGN 
              FuncRunQSchedule.FRQueueID     = iiFRQueueID
              FuncRunQSchedule.FRQScheduleID = liSeq
              FuncRunQSchedule.RunState      = "Initialized"
              FuncRunQSchedule.RunMode       = "Production".

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              FuncRunQSchedule.StartTS = 0  THEN
           UNDO add-row, LEAVE add-row.

           FuncRunQSchedule.RunState = "Scheduled".
           RUN Syst/funcrunqsparam_initialize.p (FuncRunQSchedule.FRQScheduleID).

           RUN pInvoiceTypeParameters.
           
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFuncRunQSchedule).

           ASSIGN
           Memory = recid(FuncRunQSchedule)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE FuncRunQSchedule THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND FuncRunQSchedule WHERE recid(FuncRunQSchedule) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FuncRunQSchedule THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FuncRunQSchedule).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk   = 0
        ufk[5] = (IF lcRight = "RW" AND FuncRunQueue.Active THEN 5 ELSE 0)  
        ufk[6] = (IF lcRight = "RW" AND FuncRunQueue.Active THEN 4 ELSE 0)  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FuncRunQSchedule.FRQScheduleID ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FuncRunQSchedule.FRQScheduleID 
           WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND FuncRunQSchedule WHERE recid(FuncRunQSchedule) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FuncRunQSchedule THEN
              ASSIGN FIRSTrow = i Memory = recid(FuncRunQSchedule).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE FuncRunQSchedule THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(FuncRunQSchedule)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE FuncRunQSchedule THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(FuncRunQSchedule).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FuncRunQSchedule WHERE recid(FuncRunQSchedule) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FuncRunQSchedule THEN DO:
           Memory = recid(FuncRunQSchedule).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FuncRunQSchedule THEN 
                 Memory = recid(FuncRunQSchedule).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */       

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND FuncRunQSchedule WHERE 
              recid(FuncRunQSchedule) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANS:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF FuncRunQSchedule.RunState NE "Scheduled" THEN DO:
          MESSAGE "Scheduling cannot be deleted anymore"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
       
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          FuncRunQSchedule.FRQScheduleID
          lcStartTime
          FuncRunQSchedule.RunState.

       RUN local-find-NEXT.
       IF AVAILABLE FuncRunQSchedule THEN Memory = recid(FuncRunQSchedule).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FuncRunQSchedule THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FuncRunQSchedule).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          FuncRunQSchedule.FRQScheduleID
          lcStartTime
          FuncRunQSchedule.RunState.
       
       IF ok THEN DO:

           FOR EACH FuncRunQSParam EXCLUSIVE-LOCK WHERE
              FuncRunQSParam.FRQScheduleID = FuncRunQSchedule.FRQScheduleID:
              DELETE FuncRunQSParam.
           END.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFuncRunQSchedule).

           DELETE FuncRunQSchedule.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE FuncRunQSchedule THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFuncRunQSchedule).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSDoneTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFuncRunQSchedule).

       RUN local-disp-row.
       xrecid = recid(FuncRunQSchedule).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FuncRunQSchedule) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FuncRunQSchedule) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FuncRunQSchedule WHERE recid(FuncRunQSchedule) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FuncRunQSchedule WHERE recid(FuncRunQSchedule) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST FuncRunQSchedule USE-INDEX FRQueueID WHERE 
      FuncRunQSchedule.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST FuncRunQSchedule USE-INDEX FRQueueID WHERE 
      FuncRunQSchedule.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT FuncRunQSchedule USE-INDEX FRQueueID WHERE 
      FuncRunQSchedule.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV FuncRunQSchedule USE-INDEX FRQueueID WHERE 
      FuncRunQSchedule.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       FuncRunQSchedule.FRQScheduleID
       lcStartTime
       FuncRunQSchedule.RunMode
       FuncRunQSchedule.RunState
       lcDoneTime
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

    ASSIGN
       lcStartTime = ""
       lcDoneTime = "".
       
    IF FuncRunQSchedule.StartTS > 0 THEN 
       lcStartTime = fTS2HMS(FuncRunQSchedule.StartTS).
    IF FuncRunQSchedule.DoneTS > 0 THEN 
       lcDoneTime = fTS2HMS(FuncRunQSchedule.DoneTS).

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR liTime    AS INT  NO-UNDO.
   DEF VAR liCnt     AS INT  NO-UNDO.
   DEF VAR lcOldMode AS CHAR NO-UNDO.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      ASSIGN 
         ldaUpdStartDate = ?
         lcUpdStartTime = "".
         
      IF FuncRunQSchedule.StartTS > 0 THEN DO:
         fSplitTS(FuncRunQSchedule.StartTS,
                  OUTPUT ldaUpdStartDate,
                  OUTPUT liTime).
         lcUpdStartTime = SUBSTRING(STRING(liTime,"hh:mm:ss"),1,5).
      END.
      
      DISP 
         FuncRunQSchedule.FRQueueID        
         lcQueueDesc
         FuncRunQSchedule.FRQScheduleID       
         FuncRunQSchedule.RunMode
         ldaUpdStartDate
         lcUpdStartTime
         FuncRunQSchedule.StartTS
         FuncRunQSchedule.RunState
         lcDoneTime
         FuncRunQSchedule.DoneTS
      WITH FRAME lis.

      IF NOT NEW FuncRunQSchedule THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN FuncRunQSchedule.RunState = "Scheduled"
            ufk[2] = 1125
            ufk[3] = 1181
            ufk[4] = 1180
            /*
            ufk[5] = 1189 WHEN FuncRunQSchedule.RunState = "Running"
            ufk[5] = 1190 WHEN FuncRunQSchedule.RunState = "Paused"
            */
            ufk[6] = 1188 WHEN LOOKUP(FuncRunQSchedule.RunState, 
                                      "Running,Paused") > 0
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 THEN DO:

         UpdateField:
         REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
            FIND CURRENT FuncRunQSchedule EXCLUSIVE-LOCK.
            ehto = 9.
            RUN Syst/ufkey.
         
            lcOldMode = FuncRunQSchedule.RunMode.
             
            UPDATE
               FuncRunQSchedule.RunMode
               ldaUpdStartDate
               lcUpdStartTime
            WITH FRAME lis EDITING:
 
               READKEY.

               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
               DO WITH FRAME lis:
                  PAUSE 0.
                  
                  IF FRAME-FIELD = "ldaUpdStartDate" THEN DO:
                     IF INPUT ldaUpdStartDate < TODAY THEN DO:
                        MESSAGE "Runs cannot be scheduled into past"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.

                  ELSE IF FRAME-FIELD = "lcUpdStartTime" THEN DO:
                     IF INPUT lcUpdStartTime = "" THEN DO:
                        MESSAGE "Time is mandatory"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     
                     DO liCnt = 1 TO NUM-ENTRIES(INPUT lcUpdStartTime,":"):

                        IF liCnt > 2 THEN DO:
                           MESSAGE "Invalid time"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT UpdateField.
                        END.
                         
                        liTime = INTEGER(ENTRY(liCnt,INPUT lcUpdStartTime,":"))
                                 NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN DO:
                           MESSAGE "Invalid time"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT UpdateField.
                        END.
                     
                        IF liCnt = 1 AND liTime > 23 THEN DO:
                           MESSAGE "Hour cannot be more than 23"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT UpdateField.
                        END.

                        ELSE IF liCnt = 2 AND liTime > 59 THEN DO:
                           MESSAGE "Minutes cannot be more than 59"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT UpdateField.
                        END.
                     END.
                  END.
 
                  ELSE IF FRAME-FIELD = "RunMode" THEN DO:
                     IF LOOKUP(INPUT FRAME lis FuncRunQSchedule.RunMode,
                                  "Production,Test") = 0
                     THEN DO:
                        MESSAGE "Unknown mode"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
        
               END.
      
               ELSE IF FRAME-FIELD = "RunMode" THEN DO WITH FRAME lis:
                  APPLY LASTKEY.
                  CASE SUBSTRING(INPUT RunMode,1,1):
                  WHEN "T" THEN DISPLAY "Test" @ FuncRunQSchedule.RunMode.
                  WHEN "P" THEN DISPLAY "Production" @ 
                                        FuncRunQSchedule.RunMode.
                  END CASE.
                  NEXT.
               END.
            
               APPLY LASTKEY.
            END.

            IF ldaUpdStartDate NE ? AND lcUpdStartTime > "" THEN DO:
            
               liTime = INTEGER(ENTRY(1,INPUT lcUpdStartTime,":")) * 3600.
               IF NUM-ENTRIES(INPUT lcUpdStartTime,":") > 1 THEN 
                  liTime = liTime + 
                           INTEGER(ENTRY(2,INPUT lcUpdStartTime,":")) * 60.
               FuncRunQSchedule.StartTS = fMake2DT(ldaUpdStartDate,     
                                              liTime).
            END.
            
            LEAVE UpdateField.
         END.

         IF NEW FuncRunQSchedule THEN LEAVE.
         ELSE IF FuncRunQSchedule.RunMode NE lcOldMode THEN
            RUN pInvoiceTypeParameters.
      END.

      ELSE IF toimi = 2 THEN 
         RUN Mc/errorlog.p ("FuncRunQSchedule",
                         STRING(FuncRunQSchedule.FRQScheduleID),
                         "").

      ELSE IF toimi = 3 THEN DO:
         RUN Syst/funcrunexec.p (0,FuncRunQSchedule.FRQScheduleID).
         IF RETURN-VALUE = "Updated" THEN 
            LEAVE. 
      END.
      
      ELSE IF toimi = 4 THEN DO:
         RUN Syst/funcrunqsparam.p (FuncRunQSchedule.FRQScheduleID).
      END.

      ELSE IF toimi = 5 THEN DO:
         RUN pUpdateStatus(FuncRunQSchedule.FRQScheduleID,
                           IF FuncRunQSchedule.RunState = "Paused"
                           THEN "Running"
                           ELSE "Paused",
                           OUTPUT i).
         IF i > 0 THEN LEAVE.                  
      END.
      
      ELSE IF toimi = 6 THEN DO:
         RUN pUpdateStatus(FuncRunQSchedule.FRQScheduleID,
                           "Cancelled",
                           OUTPUT i).
         IF i > 0 THEN LEAVE.                  
      END.
      
      ELSE IF toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.

PROCEDURE pUpdateStatus:

   DEF INPUT PARAMETER  iiFRQScheduleID AS INT  NO-UNDO.     
   DEF INPUT PARAMETER  icNewState      AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER oiUpdated       AS INT  NO-UNDO.

   DEF VAR lcNewState         AS CHAR NO-UNDO.
   DEF VAR llUpdate           AS LOG  NO-UNDO.
   DEF VAR lcCurrentExecState AS CHAR NO-UNDO.
   DEF VAR lcNewExecState     AS CHAR NO-UNDO.
   DEF VAR liState            AS INT  NO-UNDO.
   DEF VAR lcChangeState      AS CHAR NO-UNDO.

   FIND FIRST FuncRunQSchedule WHERE 
      FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunQSchedule THEN DO:
      MESSAGE "Scheduling not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   IF LOOKUP(FuncRunQSchedule.RunState,"Running,Paused") = 0 THEN DO:
      MESSAGE "Status of queue schedule doesn't allow this action"
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.

   
   ASSIGN
      lcNewState = ""
      llUpdate   = FALSE
      oiUpdated  = 0.
   
   CASE icNewState:
   WHEN "Paused" THEN DO:
      MESSAGE "Do You want to pause the execution of this queue?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llUpdate.

      IF llUpdate THEN ASSIGN
         lcNewState         = "Paused"
         lcCurrentExecState = "Initialized,Running".
   END.
   
   WHEN "Running" THEN DO:
      MESSAGE "Do You want to continue the execution of this queue?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llUpdate.

      IF llUpdate THEN ASSIGN
         lcNewState         = "Running"
         lcCurrentExecState = "Paused".
   END.

   WHEN "Cancelled" THEN DO:
      MESSAGE "Do You want to stop the execution of this queue permanently"
              "(it cannot be continued anymore)?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llUpdate.

      IF llUpdate THEN ASSIGN
         lcNewState         = "Cancelled"
         lcCurrentExecState = "Initialized,Running,Paused".
   END.
   
   OTHERWISE DO:
      MESSAGE "Nothing to do"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   END CASE.
   
   IF lcNewState > "" THEN DO TRANS:

      DO liState = 1 TO NUM-ENTRIES(lcCurrentExecState):
      
         lcChangeState = ENTRY(liState,lcCurrentExecState).
         
         FOR EACH FuncRunExec EXCLUSIVE-LOCK WHERE
               FuncRunExec.FRQScheduleID = FuncRunQSchedule.FRQScheduleID AND
               FuncRunExec.RunState      = lcChangeState:
               
            CASE lcNewState:
            WHEN "Running" THEN DO:
               IF CAN-FIND(FIRST FuncRunProcess WHERE
                        FuncRunProcess.FRConfigID = FuncRunExec.FRConfigID AND
                        FuncRunProcess.FRExecID = FuncRunExec.FRExecID)
               THEN lcNewExecState = lcNewState.
               ELSE lcNewExecState = "Initialized".
            END.
            OTHERWISE lcNewExecState = lcNewState.
            END CASE.
            
            ASSIGN
               FuncRunExec.RunState = lcNewExecState
               oiUpdated            = oiUpdated + 1.
               
            CREATE FuncRunExecLog.
            ASSIGN 
               FuncRunExecLog.FRExecID    = FuncRunExec.FRExecID
               FuncRunExecLog.StatusStamp = fMakeTS()
               FuncRunExecLog.FRStatus    = FuncRunExec.RunState.
               
            IF lcChangeState NE "Initialized" THEN 
            FOR EACH FuncRunProcess EXCLUSIVE-LOCK WHERE
               FuncRunProcess.FRConfigID = FuncRunExec.FRConfigID AND
               FuncRunProcess.FRExecID   = FuncRunExec.FRExecID AND
               FuncRunProcess.RunState   = lcChangeState:
               FuncRunProcess.RunState = lcNewExecState.
            END.      
            
         END.   
      END.
 
      IF oiUpdated > 0 THEN DO:
         FIND CURRENT FuncRunQSchedule EXCLUSIVE-LOCK.
         ASSIGN 
            FuncRunQSchedule.RunState = lcNewState
            FuncRunQSchedule.DoneTS   = fMakeTS().
         
         IF lcNewState = "Cancelled" THEN DO:   
            CREATE ErrorLog.
            ASSIGN 
               ErrorLog.Brand     = gcBrand
               ErrorLog.ActionID  = "FRQUEUERUN" + STRING(iiFRQScheduleID)
               ErrorLog.TableName = "FuncRunQSchedule"
               ErrorLog.KeyValue  = STRING(iiFRQScheduleID)
               ErrorLog.ErrorMsg  = "Queue run stopped manually"
               ErrorLog.UserCode  = katun.
               ErrorLog.ActionTS  = fMakeTS().

            FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.TableName = "FuncRunQueue" AND
                 ActionLog.KeyValue  = STRING(FuncRunQSchedule.FRQueueID) AND
                 ActionLog.ActionID  = "FRQUEUE" + 
                                       STRING(FuncRunQSchedule.FRQueueID) AND
                 ActionLog.ActionStatus = 0 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE ActionLog THEN ActionLog.ActionStatus = 5.
         END.      

      END.
      
      ELSE MESSAGE "Nothing could be updated"
           VIEW-AS ALERT-BOX INFORMATION.
 
   END.

END PROCEDURE.

PROCEDURE pInvoiceTypeParameters:

   DEF VAR liFromType AS INT  NO-UNDO.
   DEF VAR liToType   AS INT  NO-UNDO.
  
   DEF BUFFER bParam FOR FuncRunQSParam.
    
   IF FuncRunQSchedule.RunState NE "Scheduled" THEN RETURN. 

   /* set invoice type according to selected run mode */
   IF FuncRunQSchedule.RunMode = "test" THEN ASSIGN
      liFromType = {&INV_TYPE_NORMAL}
      liToType   = {&INV_TYPE_TEST}.
   ELSE ASSIGN 
      liFromType = {&INV_TYPE_TEST}
      liToType   = {&INV_TYPE_NORMAL}.
  
   FOR EACH FuncRunQSParam NO-LOCK WHERE
            FuncRunQSParam.FRQScheduleID = FuncRunQSchedule.FRQScheduleID,
      FIRST FuncRunQRow NO-LOCK WHERE
            FuncRunQRow.FRQueueID = FuncRunQSchedule.FRQueueID AND
            FuncRunQRow.FRQRowSeq = FuncRunQSParam.FRQRowSeq,
      FIRST FuncRunParam NO-LOCK WHERE
            FuncRunParam.FRConfigID = FuncRunQRow.FRConfigID AND
            FuncRunParam.ParamSeq   = FuncRunQSParam.ParamSeq:
             
      CASE FuncRunParam.ParamName:
      WHEN "Invoice Type" THEN DO:
         FIND FIRST bParam WHERE RECID(bParam) = RECID(FuncRunQSParam)
            EXCLUSIVE-LOCK.
         bParam.IntParam = liToType.
      END.           
      WHEN "Period From" THEN DO:
         FIND FIRST bParam WHERE RECID(bParam) = RECID(FuncRunQSParam)
            EXCLUSIVE-LOCK.
         bParam.CharParam = IF FuncRunQSchedule.RunMode = "test"
                            THEN "#RUNBEGIN"
                            ELSE FuncRunParam.DefaultValue. 
      END.                      
      WHEN "Period To" THEN DO:
         FIND FIRST bParam WHERE RECID(bParam) = RECID(FuncRunQSParam)
            EXCLUSIVE-LOCK.
         bParam.CharParam = IF FuncRunQSchedule.RunMode = "test"
                            THEN "#RUNEND"
                            ELSE FuncRunParam.DefaultValue. 
      END.                      
      WHEN "Fee Period" THEN DO:
         FIND FIRST bParam WHERE RECID(bParam) = RECID(FuncRunQSParam)
            EXCLUSIVE-LOCK.
         bParam.CharParam = IF FuncRunQSchedule.RunMode = "test"
                            THEN "#RUNMONTH"
                            ELSE FuncRunParam.DefaultValue. 
      END.
      END CASE.
   END.

   RUN Syst/funcrunqsparam_initialize.p (FuncRunQSchedule.FRQScheduleID).
   
END PROCEDURE.


