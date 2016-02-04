/* ----------------------------------------------------------------------
  MODULE .......: BRTestQResult.p
  TASK .........: UPDATEs table BRTestQResult
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'BRTestQResult'}
{Syst/eventval.i}
{Func/timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBRTestQResult AS HANDLE NO-UNDO.
   lhBRTestQResult = BUFFER BRTestQResult:HANDLE.
   RUN StarEventInitialize(lhBRTestQResult).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhBRTestQResult).
   END.

END.

DEF INPUT PARAMETER iiBRTestQueueID AS INT  NO-UNDO.

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

DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR liSeq          AS INT  NO-UNDO.
DEF VAR lcConfName     AS CHAR NO-UNDO.
DEF VAR liUpdateRow    AS INT  NO-UNDO INIT 4. 
DEF VAR lcRunStamp     AS CHAR NO-UNDO.
DEF VAR lcUserCode     AS CHAR NO-UNDO.

FORM
    BRTestQResult.BRTestQResultID 
    lcRunStamp       FORMAT "X(20)" COLUMN-LABEL "Test Done"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " TEST RESULTS OF " + STRING(iiBRTestQueueID) + " "
    FRAME sel.

FORM
    BRTestQResult.BRTestQueueID     COLON 25
       lcConfName NO-LABEL FORMAT "X(30)" SKIP
    BRTestQResult.BRTestQResultID COLON 25 SKIP(1)
    BRTestQResult.TestRunStamp      COLON 25 
       lcRunStamp NO-LABEL FORMAT "X(30)" SKIP
    BRTestQResult.UserCode        COLON 25
       lcUserCode NO-LABEL FORMAT "X(30)" 
WITH  OVERLAY ROW liUpdateRow centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fRunStamp RETURNS LOGIC
   (idRunStamp AS DEC):

   IF idRunStamp > 0 THEN lcRunStamp = fTS2HMS(idRunStamp).
   ELSE lcRunStamp = "".
   
END FUNCTION.



FIND FIRST BRTestQueue WHERE BRTestQueue.BRTestQueueID = iiBRTestQueueID 
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE BRTestQueue THEN DO:
   MESSAGE "Test queue not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
lcConfName = BRTestQueue.Description.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE BRTestQResult THEN ASSIGN
   Memory       = recid(BRTestQResult)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No results available" VIEW-AS ALERT-BOX INFORMATION.
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
        FIND BRTestQResult WHERE recid(BRTestQResult) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BRTestQResult THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BRTestQResult).
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
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW BRTestQResult.BRTestQResultID ;(uchoose.i;) 
           NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) BRTestQResult.BRTestQResultID 
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
        FIND BRTestQResult WHERE recid(BRTestQResult) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE BRTestQResult THEN
              ASSIGN FIRSTrow = i Memory = recid(BRTestQResult).
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
           IF NOT AVAILABLE BRTestQResult THEN DO:
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
                rtab[1] = recid(BRTestQResult)
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
           IF NOT AVAILABLE BRTestQResult THEN DO:
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
              rtab[FRAME-DOWN] = recid(BRTestQResult).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND BRTestQResult WHERE recid(BRTestQResult) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE BRTestQResult THEN DO:
           Memory = recid(BRTestQResult).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE BRTestQResult THEN Memory = recid(BRTestQResult).
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
           FIND BRTestQResult WHERE recid(BRTestQResult) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBRTestQResult).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndTS */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBRTestQResult).

       RUN local-disp-row.
       xrecid = recid(BRTestQResult).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(BRTestQResult) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(BRTestQResult) must-print = TRUE.
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
      FIND BRTestQResult WHERE recid(BRTestQResult) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND BRTestQResult WHERE recid(BRTestQResult) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST BRTestQResult USE-INDEX BRTestQueueID WHERE 
      BRTestQResult.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST BRTestQResult USE-INDEX BRTestQueueID WHERE 
      BRTestQResult.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT BRTestQResult USE-INDEX BRTestQueueID WHERE 
      BRTestQResult.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV BRTestQResult USE-INDEX BRTestQueueID WHERE 
      BRTestQResult.BRTestQueueID = iiBRTestQueueID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       BRTestQResult.BRTestQResultID
       lcRunStamp
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   fRunStamp(BRTestQResult.TestRunStamp).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      FIND FIRST TMSUser WHERE TMSUser.UserCode = BRTestQResult.UserCode
         NO-LOCK NO-ERROR.
      IF AVAILABLE TMSUser THEN lcUserCode = TMSUser.UserName.
      ELSE lcUserCode = "".
      
      DISP 
         BRTestQResult.BRTestQueueID        
         lcConfName
         BRTestQResult.BRTestQResultID       
         BRTestQResult.TestRunStamp
         lcRunStamp
         BRTestQResult.UserCode
         lcUserCode
      WITH FRAME lis.

      IF NOT NEW BRTestQResult THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[4] = 1728
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.

      IF toimi = 4 THEN DO:
         RUN Inv/brtestqresultrow.p (BRTestQResult.BRTestQResultID).
      END.
      
      ELSE IF toimi = 8 THEN LEAVE. 
   END.
   
END PROCEDURE.


