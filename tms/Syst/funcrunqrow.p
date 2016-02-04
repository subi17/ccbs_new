/* ----------------------------------------------------------------------
  MODULE .......: FuncRunQRow.p
  TASK .........: UPDATEs table FuncRunQRow
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 12.04.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FuncRunQRow'}
{Syst/eventval.i}
{Func/timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFuncRunQRow AS HANDLE NO-UNDO.
   lhFuncRunQRow = BUFFER FuncRunQRow:HANDLE.
   RUN StarEventInitialize(lhFuncRunQRow).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhFuncRunQRow).
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

DEF VAR lcStatus       AS CHAR NO-UNDO.
DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcQueueDesc    AS CHAR NO-UNDO.
DEF VAR lcStartTime    AS CHAR NO-UNDO.
DEF VAR lcEndTime      AS CHAR NO-UNDO.
DEF VAR liSeq          AS INT  NO-UNDO.
DEF VAR lcConfName     AS CHAR NO-UNDO.

FORM
    FuncRunQRow.FRQRowSeq 
    lcConfName                  FORMAT "X(25)" COLUMN-LABEL "Name"
    FuncRunQRow.WaitForRowSeq  COLUMN-LABEL "Wait For"
    FuncRunQRow.FeedFromRowSeq COLUMN-LABEL "Feeds From"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " ROWS OF QUEUE " + STRING(iiFRQueueID) + " "
    FRAME sel.

FORM
    FuncRunQRow.FRQueueID      COLON 20
       lcQueueDesc NO-LABEL FORMAT "X(30)" SKIP(1)
    FuncRunQRow.FRQRowSeq         COLON 20
    FuncRunQRow.FRConfigID       COLON 20
       lcConfName NO-LABEL FORMAT "X(30)" SKIP
    FuncRunQRow.MinStartTime   COLON 20
    FuncRunQRow.WaitForRowSeq  COLON 20
    FuncRunQRow.FeedFromRowSeq COLON 20 
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

IF AVAILABLE FuncRunQRow THEN ASSIGN
   Memory       = recid(FuncRunQRow)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rows available" VIEW-AS ALERT-BOX INFORMATION.
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

   IF must-add THEN DO:  /* Add a FuncRunQRow  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY iiFRQueueID @ FuncRunQRow.FRQueueID.

           liSeq = 5.
           FIND LAST FuncRunQRow WHERE 
              FuncRunQRow.FRQueueID = iiFRQueueID
              USE-INDEX FuncRunQRow NO-LOCK NO-ERROR.
           IF AVAILABLE FuncRunQRow THEN liSeq = FuncRunQRow.FRQRowSeq + 5.
           
           CREATE FuncRunQRow.
           ASSIGN 
              FuncRunQRow.FRQueueID  = iiFRQueueID
              FuncRunQRow.FRQRowSeq     = liSeq.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              FuncRunQRow.FRConfigID = 0  THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFuncRunQRow).

           ASSIGN
           Memory = recid(FuncRunQRow)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE FuncRunQRow THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND FuncRunQRow WHERE recid(FuncRunQRow) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FuncRunQRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FuncRunQRow).
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
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
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
        CHOOSE ROW FuncRunQRow.FRQRowSeq ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FuncRunQRow.FRQRowSeq WITH FRAME sel.
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
        FIND FuncRunQRow WHERE recid(FuncRunQRow) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FuncRunQRow THEN
              ASSIGN FIRSTrow = i Memory = recid(FuncRunQRow).
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
           IF NOT AVAILABLE FuncRunQRow THEN DO:
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
                rtab[1] = recid(FuncRunQRow)
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
           IF NOT AVAILABLE FuncRunQRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(FuncRunQRow).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FuncRunQRow WHERE recid(FuncRunQRow) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FuncRunQRow THEN DO:
           Memory = recid(FuncRunQRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FuncRunQRow THEN Memory = recid(FuncRunQRow).
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
           FIND FuncRunQRow WHERE recid(FuncRunQRow) = Memory NO-LOCK.
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

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          FuncRunQRow.FRQRowSeq
          lcConfName.

       RUN local-find-NEXT.
       IF AVAILABLE FuncRunQRow THEN Memory = recid(FuncRunQRow).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FuncRunQRow THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FuncRunQRow).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          FuncRunQRow.FRQRowSeq
          lcConfName.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFuncRunQRow).

           DELETE FuncRunQRow.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE FuncRunQRow THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFuncRunQRow).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFuncRunQRow).

       RUN local-disp-row.
       xrecid = recid(FuncRunQRow).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FuncRunQRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FuncRunQRow) must-print = TRUE.
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
      FIND FuncRunQRow WHERE recid(FuncRunQRow) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FuncRunQRow WHERE recid(FuncRunQRow) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST FuncRunQRow USE-INDEX FuncRunQRow WHERE 
      FuncRunQRow.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST FuncRunQRow USE-INDEX FuncRunQRow WHERE 
      FuncRunQRow.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT FuncRunQRow USE-INDEX FuncRunQRow WHERE 
      FuncRunQRow.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV FuncRunQRow USE-INDEX FuncRunQRow WHERE 
      FuncRunQRow.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       FuncRunQRow.FRQRowSeq
       lcConfName
       FuncRunQRow.WaitForRowSeq  
       FuncRunQRow.FeedFromRowSeq 
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

    lcConfName = "".
       
    IF FuncRunQRow.FRConfigID > 0 THEN DO:
       FIND FIRST FuncRunConfig WHERE 
          FuncRunConfig.FRConfigID = FuncRunQRow.FRConfigID NO-LOCK NO-ERROR.
          IF AVAILABLE FuncRunConfig THEN lcConfName = FuncRunConfig.ConfName.
    END.      
       
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bQueueRow FOR FuncRunQRow.
   

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         FuncRunQRow.FRQueueID        
         lcQueueDesc
         FuncRunQRow.FRQRowSeq       
         FuncRunQRow.FRConfigID
         lcConfName
         FuncRunQRow.MinStartTime
         FuncRunQRow.WaitForRowSeq  
         FuncRunQRow.FeedFromRowSeq 
      WITH FRAME lis.

      IF NOT NEW FuncRunQRow THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 THEN DO:

         UpdateField:
         REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
            FIND CURRENT FuncRunQRow EXCLUSIVE-LOCK.
            ehto = 9.
            RUN Syst/ufkey.
         
            UPDATE
               FuncRunQRow.FRQRowSeq WHEN NEW FuncRunQRow     
               FuncRunQRow.FRConfigID  
               FuncRunQRow.MinStartTime
               FuncRunQRow.WaitForRowSeq  
               FuncRunQRow.FeedFromRowSeq 
            WITH FRAME lis EDITING:
 
               READKEY.

               IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "FRConfigID"
               THEN DO:

                  ASSIGN 
                     gcHelpParam  = "FuncRunQRow"
                     siirto = ?.

                  RUN Syst/funcrunconfig.p.
                     
                  gcHelpParam = "".
                  
                  IF siirto NE "" AND siirto NE ? THEN 
                     DISPLAY INTEGER(siirto) @ FuncRunQRow.FRConfigID
                     WITH FRAME lis.

                  ehto = 9.
                  RUN Syst/ufkey.
                  NEXT. 
               END.

               ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
               DO WITH FRAME lis:
                  PAUSE 0.

                  IF FRAME-FIELD = "FRConfigID" THEN DO:
                     IF INPUT FuncRunQRow.FRConfigID > 0 AND 
                        NOT CAN-FIND(FIRST FuncRunConfig WHERE 
                          FuncRunConfig.FRConfigID = 
                             INPUT FuncRunQRow.FRConfigID)
                     THEN DO:
                        MESSAGE "Unknown function configuration ID"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.

                  ELSE IF FRAME-FIELD = "WaitForRowSeq" THEN DO:
                     IF INPUT FuncRunQRow.WaitForRowSeq > 0 AND 
                        NOT CAN-FIND(FIRST bQueueRow WHERE
                           bQueueRow.FRQueueID = FuncRunQRow.FRQueueID AND
                           bQueueRow.FRQRowSeq = 
                              INPUT FuncRunQRow.WaitForRowSeq)
                     THEN DO:
                        MESSAGE "Unknown row sequence"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     IF INPUT FuncRunQRow.WaitForRowSeq >=
                         FuncRunQRow.FRQRowSeq
                     THEN DO:
                        MESSAGE "Not a valid sequence for this purpose"
                        VIEW-AS ALERT-BOX INFORMATION.
                        NEXT.
                     END.
                  END.

                  ELSE IF FRAME-FIELD = "FeedFromRowSeq" THEN DO:
                     IF INPUT FuncRunQRow.FeedFromRowSeq > 0 AND 
                        NOT CAN-FIND(FIRST bQueueRow WHERE
                           bQueueRow.FRQueueID = FuncRunQRow.FRQueueID AND
                           bQueueRow.FRQRowSeq = 
                              INPUT FuncRunQRow.FeedFromRowSeq)
                     THEN DO:
                        MESSAGE "Unknown row sequence"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.

                     IF INPUT FuncRunQRow.FeedFromRowSeq >= 
                        FuncRunQRow.FRQRowSeq
                     THEN DO:
                        MESSAGE "Not a valid sequence for this purpose"
                        VIEW-AS ALERT-BOX INFORMATION.
                        NEXT.
                     END.
                  END.
               END.
            
               APPLY LASTKEY.
            END.

            LEAVE UpdateField.
         END.

         IF NEW FuncRunQRow THEN LEAVE.
         
      END.

      ELSE IF toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.


