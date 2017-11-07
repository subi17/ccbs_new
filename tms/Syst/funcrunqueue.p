/* ----------------------------------------------------------------------
  MODULE .......: FuncRunQueue
  TASK .........: UPDATEs table FuncRunQueue
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 12.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable FuncRunQueue

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FuncRunQueue'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFuncRunQueue AS HANDLE NO-UNDO.
   lhFuncRunQueue = BUFFER FuncRunQueue:HANDLE.
   RUN StarEventInitialize(lhFuncRunQueue).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhFuncRunQueue).
   END.

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR liFRQueueID    AS INT                    NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 1.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.

DEF VAR lcField         AS CHAR NO-UNDO. 
DEF VAR lcCode          AS CHAR NO-UNDO. 

FORM
    FuncRunQueue.FRQueueID  FORMAT ">>>>>>9" COLUMN-LABEL "ID"
    FuncRunQueue.QueueDesc  FORMAT "X(45)"
    FuncRunQueue.Active     
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
       "  FUNCTION QUEUE  " +
       string(TODAY,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    FuncRunQueue.Brand       COLON 25
    FuncRunQueue.FRQueueID   COLON 25
    FuncRunQueue.Active      COLON 25
    FuncRunQueue.QueueDesc   VIEW-AS EDITOR SIZE 60 BY 3
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FORM 
    "Brand:" lcBrand skip
    "ID   :" liFRQueueID FORMAT ">>>>>>>9" 
    HELP "Enter name"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND ID "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.


IF Syst.Var:gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE FuncRunQueue THEN ASSIGN
   Memory       = recid(FuncRunQueue)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No queues available!" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a FuncRunQueue  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ FuncRunQueue.Brand.

           FIND LAST FuncRunQueue USE-INDEX FRQueueID NO-LOCK NO-ERROR.
           IF AVAILABLE FuncRunQueue
           THEN i = FuncRunQueue.FRQueueID + 1.
           ELSE i = 1.

           CREATE FuncRunQueue.
           ASSIGN 
              FuncRunQueue.Brand     = lcBrand
              FuncRunQueue.FRQueueID = i.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              FuncRunQueue.QueueDesc = "" THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFuncRunQueue).

           ASSIGN
           Memory = recid(FuncRunQueue)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE FuncRunQueue THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND FuncRunQueue WHERE recid(FuncRunQueue) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FuncRunQueue THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FuncRunQueue).
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
        Syst.Var:ufk    = 0
        Syst.Var:ufk[1] = 816
        Syst.Var:ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        Syst.Var:ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        Syst.Var:ufk[7] = 0  
        Syst.Var:ufk[8] = 8 
        Syst.Var:ehto   = 3 
        ufkey  = FALSE.
        
        /* used as help */
        IF Syst.Var:gcHelpParam > "" THEN ASSIGN
           Syst.Var:ufk[5] = 11
           Syst.Var:ufk[6] = 0
           Syst.Var:ufk[7] = 0.
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FuncRunQueue.FRQueueID {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) FuncRunQueue.FRQueueID WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND FuncRunQueue WHERE recid(FuncRunQueue) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FuncRunQueue THEN
              ASSIGN FIRSTrow = i Memory = recid(FuncRunQueue).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE FuncRunQueue THEN DO:
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
                rtab[1] = recid(FuncRunQueue)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE FuncRunQueue THEN DO:
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
              rtab[FRAME-DOWN] = recid(FuncRunQueue).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FuncRunQueue WHERE recid(FuncRunQueue) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FuncRunQueue THEN DO:
           Memory = recid(FuncRunQueue).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FuncRunQueue THEN Memory = recid(FuncRunQueue).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND FuncRunQueue WHERE recid(FuncRunQueue) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN Syst.Var:gcAllBrand 
           liFRQueueID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liFRQueueID > 0 THEN DO:
          FIND FIRST FuncRunQueue WHERE 
                     FuncRunQueue.Brand = lcBrand AND
                     FuncRunQueue.FRQueueID >= liFRQueueID
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND Syst.Var:ufk[5] > 0 THEN DO:  /* add */
        IF Syst.Var:gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND Syst.Var:ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST FuncRunQRow WHERE 
            FuncRunQRow.FRQueueID = FuncRunQueue.FRQueueID)
       THEN DO:
          MESSAGE "Rows exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       IF CAN-FIND(FIRST FuncRunQSchedule WHERE 
            FuncRunQSchedule.FRQueueID = FuncRunQueue.FRQueueID)
       THEN DO:
          MESSAGE "Timetables exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       FuncRunQueue.FRQueueID FuncRunQueue.QueueDesc.
        
       RUN local-find-NEXT.
       IF AVAILABLE FuncRunQueue THEN Memory = recid(FuncRunQueue).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FuncRunQueue THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FuncRunQueue).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       FuncRunQueue.FRQueueID FuncRunQueue.QueueDesc.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFuncRunQueue).

           DELETE FuncRunQueue.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE FuncRunQueue THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF Syst.Var:gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFuncRunQueue).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY FuncRunQueue.FRQueueID.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFuncRunQueue).

       RUN local-disp-row.
       xrecid = recid(FuncRunQueue).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FuncRunQueue) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FuncRunQueue) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

IF Syst.Var:gcHelpParam > "" THEN DO:
   FIND FIRST FuncRunQueue WHERE RECID(FuncRunQueue) = xRecid NO-LOCK.
   siirto = STRING(FuncRunQueue.FRQueueID).
END.
 
Syst.Var:ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FuncRunQueue WHERE recid(FuncRunQueue) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FuncRunQueue WHERE recid(FuncRunQueue) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST FuncRunQueue USE-INDEX BrandQueue WHERE 
                 FuncRunQueue.Brand = lcBrand 
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST FuncRunQueue USE-INDEX BrandQueue WHERE 
                FuncRunQueue.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT FuncRunQueue USE-INDEX BrandQueue WHERE 
                FuncRunQueue.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV FuncRunQueue USE-INDEX BrandQueue WHERE 
                FuncRunQueue.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       FuncRunQueue.FRQueueID 
       FuncRunQueue.QueueDesc
       FuncRunQueue.Active
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR lcValue AS CHAR NO-UNDO.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         FuncRunQueue.Brand          
         FuncRunQueue.FRQueueID        
         FuncRunQueue.Active
         FuncRunQueue.QueueDesc   
      WITH FRAME lis.

      IF NEW FuncRunQueue THEN Syst.Var:toimi = 1.

      ELSE DO:
         ASSIGN 
            Syst.Var:ufk    = 0
            Syst.Var:ufk[1] = 7    WHEN lcRight = "RW"
            Syst.Var:ufk[3] = 1728
            Syst.Var:ufk[4] = 1185
            Syst.Var:ufk[8] = 8
            Syst.Var:ehto   = 0.
         
         RUN Syst/ufkey.p.
      END.
                  
      IF Syst.Var:toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT FuncRunQueue EXCLUSIVE-LOCK.
      
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
   
         UPDATE
            FuncRunQueue.Active
            FuncRunQueue.QueueDesc
         WITH FRAME lis EDITING:
 
            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
            END.
            
            APPLY LASTKEY.
         END.
   
         IF NEW FuncRunQueue THEN LEAVE MaintMenu.
         LEAVE.
      END.

      ELSE IF Syst.Var:toimi = 3 THEN DO:
         RUN Syst/funcrunqrow.p(FuncRunQueue.FRQueueID).
      END.
 
      ELSE IF Syst.Var:toimi = 4 THEN DO:
         RUN Syst/funcrunqschedule.p(FuncRunQueue.FRQueueID).
      END.
       
      ELSE IF Syst.Var:toimi = 8 THEN LEAVE.  

   END.
   
END PROCEDURE.



