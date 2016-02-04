/* ----------------------------------------------------------------------
  MODULE .......: FuncRunExec.p
  TASK .........: UPDATEs table FuncRunExec
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FuncRunExec'}
{Syst/eventval.i}
{Func/timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFuncRunExec AS HANDLE NO-UNDO.
   lhFuncRunExec = BUFFER FuncRunExec:HANDLE.
   RUN StarEventInitialize(lhFuncRunExec).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhFuncRunExec).
   END.

END.

DEF INPUT PARAMETER iiFRConfigID    AS INT  NO-UNDO.
DEF INPUT PARAMETER iiFRQScheduleID AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 14.
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

DEF VAR lcStatus       AS CHAR NO-UNDO.
DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcConfName     AS CHAR NO-UNDO.
DEF VAR lcStartTime    AS CHAR NO-UNDO.
DEF VAR lcEndTime      AS CHAR NO-UNDO.
DEF VAR liSeq          AS INT  NO-UNDO.
DEF VAR lcTitle        AS CHAR NO-UNDO.

DEF VAR llFollow         AS LOG  NO-UNDO INIT FALSE.
DEF VAR liFollowInterval AS INT  NO-UNDO INIT 60. 
DEF VAR ldProcessed      AS DEC  NO-UNDO.
DEF VAR lcUpdateReturn   AS CHAR NO-UNDO. 
DEF VAR lcReturnValue    AS CHAR NO-UNDO.
DEF VAR lcDuration       AS CHAR NO-UNDO.

FORM
    FuncRunExec.FRExecID  
    lcConfName              FORMAT "X(13)" COLUMN-LABEL "Name" 
    lcStartTime             FORMAT "X(19)" COLUMN-LABEL "Started"
    lcEndTime               FORMAT "X(19)" COLUMN-LABEL "Ended"
    FuncRunExec.RunState    FORMAT "X(14)"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) lcTitle
    FRAME sel.

FORM
    FuncRunExec.FRConfigID      COLON 20
       lcConfName NO-LABEL FORMAT "X(30)" SKIP(1)
    FuncRunExec.FRExecID      COLON 20
    FuncRunExec.FRExecSeq     COLON 20
    FuncRunExec.MinStartTime  COLON 20 
    lcStartTime                COLON 20 
       FORMAT "X(19)" LABEL "Started" 
       FuncRunExec.StartTS NO-LABEL SKIP
    lcEndTime                  COLON 20 
       FORMAT "X(19)" LABEL "Ended" 
       FuncRunExec.EndTS  NO-LABEL SKIP
    lcDuration COLON 20
       FORMAT "X(20)" LABEL "Duration" SKIP
    FuncRunExec.RunState      COLON 20
    FuncRunExec.FRQScheduleID COLON 20
    FuncRunExec.FRQRowSeq    COLON 20 SKIP(1)
    ldProcessed  COLON 20
       FORMAT "->>>>>>>>>9" 
       LABEL "Processed Events"
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM
   liFollowInterval AT 2 
      FORMAT ">>>>>>9"
      LABEL "Interval"
      HELP "Interval (seconds) between refreshing screen"
   WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " FOLLOW " FRAME fInterval.


IF iiFRQScheduleID > 0 THEN DO:
   FIND FIRST FuncRunQSchedule WHERE 
      FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunQSchedule THEN DO:
      MESSAGE "Scheduling data not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   lcTitle = " SCHEDULED EXECUTIONS ".
END.

IF iiFRConfigID > 0 THEN DO:
   FIND FIRST FuncRunConfig WHERE FuncRunConfig.FRConfigID = iiFRConfigID 
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunConfig THEN DO:
      MESSAGE "Configuration rule not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   ASSIGN 
      lcConfName = FuncRunConfig.ConfName
      lcTitle    = " EXECUTIONS OF " + lcConfName.
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE FuncRunExec THEN ASSIGN
   Memory       = recid(FuncRunExec)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No executions available" VIEW-AS ALERT-BOX INFORMATION.
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

   IF must-add THEN DO:  /* Add a FuncRunExec  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY iiFRConfigID @ FuncRunExec.FRConfigID.

           i = 1.
           FIND LAST FuncRunExec USE-INDEX FRExecID NO-LOCK NO-ERROR.
           IF AVAILABLE FuncRunExec THEN i = FuncRunExec.FRExecID + 1.
           
           liSeq = 1.
           FIND LAST FuncRunExec WHERE FuncRunExec.FRConfigID = iiFRConfigID
              USE-INDEX FRExecSeq NO-LOCK NO-ERROR.
           IF AVAILABLE FuncRunExec THEN liSeq = FuncRunExec.FRExecSeq + 1.
           
           CREATE FuncRunExec.
           ASSIGN 
              FuncRunExec.FRConfigID  = iiFRConfigID
              FuncRunExec.FRExecID  = i
              FuncRunExec.FRExecSeq = liSeq.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              FuncRunExec.FRExecID = 0  THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFuncRunExec).

           ASSIGN
           Memory = recid(FuncRunExec)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE FuncRunExec THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND FuncRunExec WHERE recid(FuncRunExec) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FuncRunExec THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FuncRunExec).
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
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.

        IF iiFRQScheduleID > 0 THEN DO:
           IF llFollow THEN ASSIGN 
              ufk[3] = 1187
              ufk[8] = 0.
           ELSE ASSIGN 
              ufk[3] = 1186 
              ufk[8] = 8.
 
           IF AVAILABLE FuncRunQSchedule AND 
              FuncRunQSchedule.RunState NE "Running" THEN
              ufk[3] = 0.
        END.
          
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[3] = 0
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.

      IF llFollow THEN DO:
         READKEY PAUSE 0.
         PAUSE liFollowInterval NO-MESSAGE.
         IF LOOKUP(KEYLABEL(LASTKEY),"3,F3") > 0 THEN ASSIGN
            llFollow = FALSE
            ufkey    = TRUE.
    
         RUN local-find-first.
         ASSIGN                
            must-print = TRUE
            Memory     = RECID(FuncRunExec).
         
         NEXT LOOP.
      END.
       
      IF order = 1 THEN DO:
        CHOOSE ROW FuncRunExec.FRExecID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FuncRunExec.FRExecID WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"3,f3,5,f5,8,f8") = 0 THEN DO:
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
        FIND FuncRunExec WHERE recid(FuncRunExec) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FuncRunExec THEN
              ASSIGN FIRSTrow = i Memory = recid(FuncRunExec).
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
           IF NOT AVAILABLE FuncRunExec THEN DO:
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
                rtab[1] = recid(FuncRunExec)
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
           IF NOT AVAILABLE FuncRunExec THEN DO:
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
              rtab[FRAME-DOWN] = recid(FuncRunExec).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FuncRunExec WHERE recid(FuncRunExec) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FuncRunExec THEN DO:
           Memory = recid(FuncRunExec).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FuncRunExec THEN Memory = recid(FuncRunExec).
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
           FIND FuncRunExec WHERE recid(FuncRunExec) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* refresh screen every xx seconds */
     ELSE IF LOOKUP(nap,"3,F3") > 0 AND ufk[3] > 0  THEN DO : 
     
        PAUSE 0.
        UPDATE liFollowInterval WITH FRAME fInterval.
        HIDE FRAME fInterval NO-PAUSE.
        
        ASSIGN 
           llFollow   = NOT llFollow
           must-print = TRUE
           ufkey      = TRUE
           Memory     = rtab[1].  
        NEXT LOOP.
     END.

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

       IF CAN-FIND(FIRST FuncRunProcess WHERE 
            FuncRunProcess.FRConfigID = FuncRunExec.FRConfigID AND
            FuncRunProcess.FRExecID = FuncRunExec.FRExecID)
       THEN DO:
          MESSAGE "Processes exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       IF CAN-FIND(FIRST FuncRunExecLog WHERE 
            FuncRunExecLog.FRExecID = FuncRunExec.FRExecID)
       THEN DO:
          MESSAGE "Status data exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
  
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          FuncRunExec.FRExecID
          FuncRunExec.RunState.

       RUN local-find-NEXT.
       IF AVAILABLE FuncRunExec THEN Memory = recid(FuncRunExec).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FuncRunExec THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FuncRunExec).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          FuncRunExec.FRExecID
          FuncRunExec.RunState.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFuncRunExec).

           DELETE FuncRunExec.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE FuncRunExec THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFuncRunExec).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       lcUpdateReturn = RETURN-VALUE.
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFuncRunExec).

       RUN local-disp-row.
       xrecid = recid(FuncRunExec).
       
       IF lcUpdateReturn = "Updated" THEN DO:
          lcReturnValue = lcUpdateReturn.
          LEAVE loop.
       END.   
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FuncRunExec) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FuncRunExec) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

RETURN lcReturnValue.

FINALLY:
   fCleanEventObjects().
END.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FuncRunExec WHERE recid(FuncRunExec) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FuncRunExec WHERE recid(FuncRunExec) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiFRQScheduleID > 0 THEN DO:
      IF order = 1 THEN FIND FIRST FuncRunExec USE-INDEX FRQScheduleID WHERE
         FuncRunExec.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN FIND FIRST FuncRunExec USE-INDEX FRExecSeq WHERE 
         FuncRunExec.FRConfigID = iiFRConfigID NO-LOCK NO-ERROR.
   END.      
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiFRQScheduleID > 0 THEN DO:
      IF order = 1 THEN FIND LAST FuncRunExec USE-INDEX FRQScheduleID WHERE
         FuncRunExec.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN FIND LAST FuncRunExec USE-INDEX FRExecSeq WHERE 
         FuncRunExec.FRConfigID = iiFRConfigID NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiFRQScheduleID > 0 THEN DO:
      IF order = 1 THEN FIND NEXT FuncRunExec USE-INDEX FRQScheduleID WHERE
         FuncRunExec.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN FIND NEXT FuncRunExec USE-INDEX FRExecSeq WHERE 
         FuncRunExec.FRConfigID = iiFRConfigID NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiFRQScheduleID > 0 THEN DO:
      IF order = 1 THEN FIND PREV FuncRunExec USE-INDEX FRQScheduleID WHERE
         FuncRunExec.FRQScheduleID = iiFRQScheduleID NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF order = 1 THEN FIND PREV FuncRunExec USE-INDEX FRExecSeq WHERE 
         FuncRunExec.FRConfigID = iiFRConfigID NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       FuncRunExec.FRExecID
       lcConfName
       lcStartTime
       lcEndTime
       FuncRunExec.RunState
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

    ASSIGN 
       lcStartTime = ""
       lcEndTime   = "".
       
    IF FuncRunExec.StartTS > 0 THEN 
       lcStartTime = fTS2HMS(FuncRunExec.StartTS).
    IF FuncRunExec.EndTS > 0 THEN 
       lcEndTime = fTS2HMS(FuncRunExec.EndTS).

    IF iiFRQScheduleID > 0 THEN DO:
       lcConfName = "".
       FIND FIRST FuncRunConfig WHERE 
          FuncRunConfig.FRConfigID = FuncRunExec.FRConfigID NO-LOCK NO-ERROR.
       IF AVAILABLE FuncRunConfig THEN lcConfName = FuncRunConfig.ConfName.
    END.
        
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR lcReturn  AS CHAR NO-UNDO.
   DEF VAR liDurDays AS INT  NO-UNDO.
   DEF VAR liDurTime AS INT  NO-UNDO.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      lcDuration = "".
      IF FuncRunExec.EndTS > FuncRunExec.StartTS AND
         FuncRunExec.StartTS > 0  THEN DO:
            liDurDays = DYNAMIC-FUNCTION("fTSDuration" IN ghfunc1,
                                         FuncRunExec.StartTS,
                                         FuncRunExec.EndTS,
                                         OUTPUT liDurTime).
            lcDuration = (IF liDurDays > 0 
                          THEN STRING(liDurDays) + " days and "
                          ELSE "") +
                          STRING(liDurTime,"hh:mm:ss").
      END.
      
      DISP 
         FuncRunExec.FRConfigID        
         lcConfName
         FuncRunExec.FRExecID        
         FuncRunExec.FRExecSeq
         FuncRunExec.MinStartTime
         lcStartTime
         FuncRunExec.StartTS
         lcEndTime
         FuncRunExec.EndTS
         lcDuration
         FuncRunExec.RunState
         FuncRunExec.FRQScheduleID
         FuncRunExec.FRQRowSeq
      WITH FRAME lis.

      ldProcessed = 0.
      FOR EACH FuncRunProcess NO-LOCK WHERE
               FuncRunProcess.FRExecId = FuncRunExec.FRExecID:
         ldProcessed = ldProcessed + FuncRunProcess.Processed.
      END.
      DISPLAY ldProcessed WITH FRAME lis.
      
      IF NOT NEW FuncRunExec THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN FuncRunExec.RunState = "Initialized"
            ufk[2] = 1125
            ufk[3] = 1182
            ufk[4] = 1183
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 THEN DO:

         UpdateField:
         REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
            FIND CURRENT FuncRunExec EXCLUSIVE-LOCK.
            ehto = 9.
            RUN Syst/ufkey.
         
            UPDATE
               FuncRunExec.MinStartTime
               FuncRunExec.StartTS  WHEN NEW FuncRunExec
               FuncRunExec.EndTS    WHEN NEW FuncRunExec
               FuncRunExec.RunState WHEN NEW FuncRunExec
            WITH FRAME lis EDITING:
 
               READKEY.

               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
               DO WITH FRAME lis:
                  PAUSE 0.
               END.
            
               APPLY LASTKEY.
            END.
            
            LEAVE UpdateField.
         END.

         lcReturn = "Updated".
         LEAVE.
      END.

      ELSE IF toimi = 2 THEN 
         RUN Mc/errorlog.p ("FuncRunExec",
                         STRING(FuncRunExec.FRExecID),
                         "").

      ELSE IF toimi = 3 THEN RUN Syst/funcrunexeclog.p(FuncRunExec.FRExecID).
      
      ELSE IF toimi = 4 THEN RUN Syst/funcrunprocess.p(FuncRunExec.FRConfigID,
                                                  FuncRunExec.FRExecID).
        
      ELSE IF toimi = 8 THEN LEAVE. 
   END.

   RETURN lcReturn.
   
END PROCEDURE.

