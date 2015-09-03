/* ----------------------------------------------------------------------
  MODULE .......: FuncRunProcess.p
  TASK .........: UPDATEs table FuncRunProcess
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 12.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'FuncRunProcess'}
{eventval.i}
{timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFuncRunProcess AS HANDLE NO-UNDO.
   lhFuncRunProcess = BUFFER FuncRunProcess:HANDLE.
   RUN StarEventInitialize(lhFuncRunProcess).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhFuncRunProcess).
   END.

END.

DEF INPUT PARAMETER iiFRConfigID AS INT  NO-UNDO.
DEF INPUT PARAMETER iiFRExecID AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
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

DEF VAR lcStatus         AS CHAR NO-UNDO.
DEF VAR lcField          AS CHAR NO-UNDO. 
DEF VAR lcCode           AS CHAR NO-UNDO. 
DEF VAR lcConfName       AS CHAR NO-UNDO.
DEF VAR lcStartTime      AS CHAR NO-UNDO.
DEF VAR lcLastTime       AS CHAR NO-UNDO.
DEF VAR lcEndTime        AS CHAR NO-UNDO.
DEF VAR lcShortLastTime  AS CHAR NO-UNDO.
DEF VAR liSeq            AS INT  NO-UNDO.
DEF VAR llFollow         AS LOG  NO-UNDO INIT FALSE.
DEF VAR liFollowInterval AS INT  NO-UNDO INIT 60. 
DEF VAR lcDuration       AS CHAR NO-UNDO.

FORM
    FuncRunProcess.ProcSeq  FORMAT ">>9"   COLUMN-LABEL "Seq"
    lcStartTime             FORMAT "X(19)" COLUMN-LABEL "Started"
    lcShortLastTime         FORMAT "X(8)"  COLUMN-LABEL "Last Upd"
    lcEndTime               FORMAT "X(19)" COLUMN-LABEL "Ended"
    FuncRunProcess.ProcessHost FORMAT "X(5)" 
    FuncRunProcess.RunState   FORMAT "X(9)"
    FuncRunProcess.Processed  FORMAT "->>>>>>>9"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " PROCESSES OF " + lcConfName + "/" + STRING(iiFRExecID) + " "
    FRAME sel.

FORM
    FuncRunProcess.FRConfigID      COLON 20
       FuncRunConfig.ConfName NO-LABEL SKIP(1)
    FuncRunProcess.FRExecID      COLON 20
    FuncRunProcess.FRProcessID      COLON 20
    FuncRunProcess.ProcSeq       COLON 20 
    lcStartTime                COLON 20 
       FORMAT "X(19)" LABEL "Started" 
       FuncRunProcess.StartTS NO-LABEL SKIP
    lcLastTime                 COLON 20
       FORMAT "X(19)" LABEL "Last Update"
       FuncRunProcess.LastTS NO-LABEL SKIP
    lcEndTime                  COLON 20 
       FORMAT "X(19)" LABEL "Ended" 
       FuncRunProcess.EndTS  NO-LABEL SKIP
    lcDuration COLON 20
       FORMAT "X(20)" LABEL "Duration" SKIP
    FuncRunProcess.ProcessHost   COLON 20 LABEL "Host"   
    FuncRunProcess.RunState      COLON 20
    FuncRunProcess.Processed FORMAT "->>>>>>>9" COLON 20 
    FuncRunProcess.ProcessID     COLON 20
    FuncRunProcess.RunCommand    COLON 20 
       VIEW-AS EDITOR SIZE 45 BY 3
WITH  OVERLAY ROW 2 centered
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


FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = iiFRExecID 
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE FuncRunExec THEN DO:
   MESSAGE "Execution data not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND FIRST FuncRunConfig WHERE FuncRunConfig.FRConfigID = iiFRConfigID 
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE FuncRunConfig THEN DO:
   MESSAGE "Configuration rule not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
lcConfName = FuncRunConfig.ConfName.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE FuncRunProcess THEN ASSIGN
   Memory       = recid(FuncRunProcess)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No processes available" VIEW-AS ALERT-BOX INFORMATION.
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

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND FuncRunProcess WHERE recid(FuncRunProcess) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FuncRunProcess THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FuncRunProcess).
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
        ehto  = 3 
        ufkey = FALSE.

        IF llFollow THEN ASSIGN 
           ufk[3] = 1187
           ufk[8] = 0.
        ELSE ASSIGN 
           ufk[3] = 1186 
           ufk[8] = 8.
        
        IF AVAILABLE FuncRunExec AND FuncRunExec.RunState NE "Running" THEN
           ufk[3] = 0.
           
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[3] = 0.
           
        RUN ufkey.
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
            Memory     = RECID(FuncRunProcess).
         
         NEXT LOOP.
      END.
            
      IF order = 1 THEN DO:
        CHOOSE ROW FuncRunProcess.ProcSeq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FuncRunProcess.ProcSeq WITH FRAME sel.
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
        FIND FuncRunProcess WHERE recid(FuncRunProcess) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FuncRunProcess THEN
              ASSIGN FIRSTrow = i Memory = recid(FuncRunProcess).
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
           IF NOT AVAILABLE FuncRunProcess THEN DO:
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
                rtab[1] = recid(FuncRunProcess)
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
           IF NOT AVAILABLE FuncRunProcess THEN DO:
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
              rtab[FRAME-DOWN] = recid(FuncRunProcess).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FuncRunProcess WHERE recid(FuncRunProcess) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FuncRunProcess THEN DO:
           Memory = recid(FuncRunProcess).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FuncRunProcess THEN Memory = recid(FuncRunProcess).
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
           FIND FuncRunProcess WHERE recid(FuncRunProcess) = Memory NO-LOCK.
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

       IF CAN-FIND(FIRST FuncRunResult WHERE 
            FuncRunResult.FRProcessID = FuncRunProcess.FRProcessID)
       THEN DO:
          MESSAGE "Feeds exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
  
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          FuncRunProcess.ProcSeq
          FuncRunProcess.RunState.

       RUN local-find-NEXT.
       IF AVAILABLE FuncRunProcess THEN Memory = recid(FuncRunProcess).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FuncRunProcess THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FuncRunProcess).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          FuncRunProcess.ProcSeq
          FuncRunProcess.RunState.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFuncRunProcess).

           DELETE FuncRunProcess.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE FuncRunProcess THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFuncRunProcess).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFuncRunProcess).

       RUN local-disp-row.
       xrecid = recid(FuncRunProcess).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FuncRunProcess) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FuncRunProcess) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FuncRunProcess WHERE recid(FuncRunProcess) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FuncRunProcess WHERE recid(FuncRunProcess) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF order = 1 THEN FIND FIRST FuncRunProcess USE-INDEX FRConfigID WHERE 
      FuncRunProcess.FRConfigID = iiFRConfigID AND
      FuncRunProcess.FRExecID = iiFRExecID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST FuncRunProcess USE-INDEX FRConfigID WHERE 
      FuncRunProcess.FRConfigID = iiFRConfigID AND
      FuncRunProcess.FRExecID = iiFRExecID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT FuncRunProcess USE-INDEX FRConfigID WHERE 
      FuncRunProcess.FRConfigID = iiFRConfigID AND
      FuncRunProcess.FRExecID = iiFRExecID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV FuncRunProcess USE-INDEX FRConfigID WHERE 
      FuncRunProcess.FRConfigID = iiFRConfigID AND
      FuncRunProcess.FRExecID = iiFRExecID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       FuncRunProcess.ProcSeq
       lcStartTime
       lcShortLastTime
       lcEndTime
       FuncRunProcess.ProcessHost
       FuncRunProcess.RunState
       FuncRunProcess.Processed
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

    ASSIGN 
       lcStartTime = ""
       lcEndTime   = ""
       lcLastTime  = ""
       lcShortLastTime = "".
       
    IF FuncRunProcess.StartTS > 0 THEN 
       lcStartTime = fTS2HMS(FuncRunProcess.StartTS).
    IF FuncRunProcess.EndTS > 0 THEN 
       lcEndTime = fTS2HMS(FuncRunProcess.EndTS).
    IF FuncRunProcess.LastTS > 0 THEN 
       lcLastTime = fTS2HMS(FuncRunProcess.LastTS).

    IF lcLastTime > "" THEN 
       lcShortLastTime = ENTRY(2,lcLastTime," ").
       
END PROCEDURE.

PROCEDURE local-UPDATE-record:
 
   DEF VAR liDurDays AS INT  NO-UNDO.
   DEF VAR liDurTime AS INT  NO-UNDO.
 
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      lcDuration = "".
      IF FuncRunProcess.EndTS > FuncRunProcess.StartTS AND
         FuncRunProcess.StartTS > 0  THEN DO:
            liDurDays = DYNAMIC-FUNCTION("fTSDuration" IN ghfunc1,
                                         FuncRunProcess.StartTS,
                                         FuncRunProcess.EndTS,
                                         OUTPUT liDurTime).
            lcDuration = (IF liDurDays > 0 
                          THEN STRING(liDurDays) + " days and "
                          ELSE "") +
                          STRING(liDurTime,"hh:mm:ss").
      END.
       
      DISP 
         FuncRunProcess.FRConfigID        
         FuncRunConfig.ConfName
         FuncRunProcess.FRExecID        
         FuncRunProcess.FRProcessID
         FuncRunProcess.ProcSeq
         lcStartTime
         FuncRunProcess.StartTS
         lcLastTime
         FuncRunProcess.LastTS
         lcEndTime
         FuncRunProcess.EndTS
         lcDuration
         FuncRunProcess.ProcessHost
         FuncRunProcess.RunState
         FuncRunProcess.Processed
         FuncRunProcess.ProcessID
         FuncRunProcess.RunCommand
      WITH FRAME lis.

      IF NOT NEW FuncRunProcess THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[2] = 1125
            ufk[4] = 1184
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 THEN DO:

         UpdateField:
         REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
            FIND CURRENT FuncRunProcess EXCLUSIVE-LOCK.
            ehto = 9.
            RUN ufkey.
         
            UPDATE
               FuncRunProcess.StartTS
               FuncRunProcess.EndTS
               FuncRunProcess.RunState  
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

         IF NEW FuncRunProcess THEN LEAVE.
         
      END.

      ELSE IF toimi = 2 THEN 
         RUN errorlog.p ("FuncRunProcess",
                         STRING(FuncRunProcess.FRProcessID),
                         "").

      ELSE IF toimi = 4 THEN RUN funcrunresult.p(FuncRunProcess.FRProcessID).
        
      ELSE IF toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.

