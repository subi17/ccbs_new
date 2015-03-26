/* ----------------------------------------------------------------------
  MODULE .......: VASBdest
  TASK .........: UPDATEs table VASBdest
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 23.03.04
  CHANGED ......: 
                  
  Version ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'mobsub' }
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhVASBdest AS HANDLE NO-UNDO.
   lhVASBdest = BUFFER VASBdest:HANDLE.
   RUN StarEventInitialize(lhVASBdest).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhVASBdest).
   END.

END.

DEF INPUT PARAMETER operid  LIKE VASBdest.OperID        NO-UNDO.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR VASBdest      LIKE VASBdest.Bdest        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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

form
    VASBdest.Bdest     /* COLUMN-LABEL FORMAT */
    VASBdest.OperID     /* COLUMN-LABEL FORMAT */
    VASBdest.InvEvent

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  VAS B-DESTINATIONS   "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    VASBdest.Bdest     /* LABEL FORMAT */
    VASBdest.OperID    /* LABEL FORMAT */
    VASBdest.InvEvent  /* LABEL FORMAT */
    HELP "0 = MO , 1 = MT 2 = MO & MT"

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  VASBdest */
    VASBdest
    HELP "Enter B-destination "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND B-DESTINATION "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  OperID */
    OperID
    HELP "Enter Name of the oper id"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) "FIND OPER ID"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST VASBdest
WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
IF AVAILABLE VASBdest THEN ASSIGN
   Memory       = recid(VASBdest)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No VAS B-Destination items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a VASBdest  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           PROMPT-FOR VASBdest.Bdest
           VALIDATE
           (VASBdest.Bdest NOT ENTERED OR
           NOT CAN-FIND(VASBdest using  VASBdest.Bdest),
           "VAS B-destination " + string(INPUT VASBdest.Bdest) +
           " already exists !").
           IF INPUT FRAME lis VASBdest.Bdest = "" THEN 
           LEAVE add-row.
           CREATE VASBdest.
           
           ASSIGN
              VASBdest.Bdest = INPUT FRAME lis VASBdest.Bdest.
           
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhVASBdest).

           ASSIGN
           Memory = recid(VASBdest)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST VASBdest
      WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VASBdest THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND VASBdest WHERE recid(VASBdest) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE VASBdest THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(VASBdest).
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
        ufk[1]= 704  ufk[2]= 770 ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW VASBdest.Bdest ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VASBdest.Bdest WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW VASBdest.OperID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VASBdest.OperID WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND VASBdest WHERE recid(VASBdest) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE VASBdest THEN
              ASSIGN FIRSTrow = i Memory = recid(VASBdest).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE VASBdest THEN DO:
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
                rtab[1] = recid(VASBdest)
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
           IF NOT AVAILABLE VASBdest THEN DO:
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
              rtab[FRAME-DOWN] = recid(VASBdest).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND VASBdest WHERE recid(VASBdest) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE VASBdest THEN DO:
           Memory = recid(VASBdest).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE VASBdest THEN Memory = recid(VASBdest).
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
           FIND VASBdest WHERE recid(VASBdest) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET VASBdest WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF VASBdest ENTERED THEN DO:
          FIND FIRST VASBdest WHERE VASBdest.Bdest >= VASBdest
          AND (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VASBdest THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some VASBdest/VASBdest was found */
          ASSIGN order = 1 Memory = recid(VASBdest) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET OperID WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF OperID ENTERED THEN DO:
          FIND FIRST VASBdest WHERE VASBdest.OperID >= OperID
           AND (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VASBdest THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some VASBdest/OperID was found */
          ASSIGN order = 2 Memory = recid(VASBdest) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       VASBdest.Bdest VASBdest.OperID Vasbdest.invevent.

       RUN local-find-NEXT.
       IF AVAILABLE VASBdest THEN Memory = recid(VASBdest).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE VASBdest THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(VASBdest).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       VASBdest.Bdest VASBdest.OperID Vasbdest.invevent.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhVASBdest).

           DELETE VASBdest.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST VASBdest
           WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE)) THEN DO:
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
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhVASBdest).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY VASBdest.Bdest Vasbdest.invevent.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhVASBdest).

       RUN local-disp-row.
       xrecid = recid(VASBdest).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(VASBdest) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(VASBdest) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND VASBdest WHERE recid(VASBdest) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND VASBdest WHERE recid(VASBdest) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST VASBdest
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST VASBdest USE-INDEX OperID
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST VASBdest
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST VASBdest USE-INDEX OperID
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT VASBdest
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT VASBdest USE-INDEX OperID
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV VASBdest
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV VASBdest USE-INDEX OperID
       WHERE (IF operid ne "" THEN Vasbdest.operid = operid ELSE TRUE) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       VASBdest.Bdest 
       VASBdest.OperID
       Vasbdest.invevent

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          VASBdest.OperID
          VASBdest.InvEvent
          VASBdest.Bdest
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
            VASBdest.OperID
            VASBdest.InvEvent
         
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

