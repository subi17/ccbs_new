/* ----------------------------------------------------------------------
  MODULE .......: SIMStat.P
  TASK .........: UPDATE SIM Status Codes
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 31-05-99
  CHANGED ......: 07-10-99 jp urights added
                  21.05.02/tk Event logging added
                  07.03.03/tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SIMStat'}

IF llDoEvent THEN DO:
   &GLOBAl-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSIMStat AS HANDLE NO-UNDO.
   lhSIMStat = BUFFER SIMStat:HANDLE.
   RUN StarEventInitialize(lhSIMStat).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhSIMStat).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR SIMStat  LIKE SIMStat.SIMStat  NO-UNDO.
DEF VAR SSName LIKE SIMStat.SSName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
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
DEF VAR ok           AS log format "Yes/No"  NO-UNDO.

form
    SIMStat.SIMStat      /* COLUMN-LABEL FORMAT */
    SIMStat.SSName     /* COLUMN-LABEL FORMAT */
    WITH width 80 OVERLAY SCROLL 1 15 DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " SIM Status Codes "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    SIMStat.SIMStat     /* LABEL FORMAT */
    SIMStat.SSName    /* LABEL FORMAT */
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* seek SIM Status  BY  SIMStat */
    SimStat
    help "Enter ...."
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek SIM Status  BY SSName */
    SSName
    help "Enter ..."
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".

FIND FIRST SIMStat
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE SIMStat THEN ASSIGN
   Memory       = recid(SIMStat)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No SIM Status codes available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order THEN DO:
       pr-order = order.
       PUT SCREEN ROW 19 col 37 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a SIMStat  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:
           PROMPT-FOR SIMStat.SIMStat
           VALIDATE
              (SIMStat.SIMStat = 0 OR
              NOT CAN-FIND(SIMStat using  SIMStat.SIMStat),
              "SIM Status " + string(INPUT SIMStat.SIMStat) +
              " already exists !").
           IF INPUT SIMStat.SIMStat = 0 THEN LEAVE ADD-ROW.
           CREATE SIMStat.
           ASSIGN
           SIMStat.SIMStat = INPUT FRAME lis SIMStat.SIMStat.
           UPDATE SIMStat.SSName.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSIMStat).       

           ASSIGN
           Memory = recid(SIMStat)
           xrecid = Memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST SIMStat
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SIMStat THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND SIMStat WHERE recid(SIMStat) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SIMStat THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SIMStat).
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
        PAUSE 0 no-MESSAGE.

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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SIMStat.SIMStat ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SIMStat.SIMStat WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SIMStat.SSName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SIMStat.SSName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND SIMStat WHERE recid(SIMStat) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE SIMStat THEN
              ASSIGN FIRSTrow = i Memory = recid(SIMStat).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND SIMStat WHERE recid(SIMStat) = rtab[FRAME-LINE] NO-LOCK.
           RUN local-find-prev.
           IF NOT AVAILABLE SIMStat THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(SIMStat)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND SIMStat WHERE recid(SIMStat) = rtab[FRAME-DOWN] NO-LOCK .
           RUN local-find-NEXT.
           IF NOT AVAILABLE SIMStat THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(SIMStat).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SIMStat WHERE recid(SIMStat) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE SIMStat THEN DO:
           Memory = recid(SIMStat).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE SIMStat THEN Memory = recid(SIMStat).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND SIMStat WHERE recid(SIMStat) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       SIMStat = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE SIMStat WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF SIMStat <> 0 THEN DO:
          FIND FIRST SIMStat WHERE SIMStat.SIMStat >= SimStat
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE SIMStat THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some SIMStat/SimStat was found */
          ASSIGN order = 1 Memory = recid(SIMStat) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       SSName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE SSName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF SSName <> "" THEN DO:
          FIND FIRST SIMStat WHERE SIMStat.SSName >= SSName
          USE-INDEX SSName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE SIMStat THEN DO:
             bell. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some SIMStat/SSName was found */
          ASSIGN order = 2 Memory = recid(SIMStat) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       FIND SIMStat WHERE recid(SIMStat) = rtab[FRAME-LINE] NO-LOCK.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       SIMStat.SIMStat SIMStat.SSName /* sd */.

       RUN local-find-NEXT.
       IF AVAILABLE SIMStat THEN Memory = recid(SIMStat).
       ELSE DO:
          /* read back the record that is TO be  removed */
          FIND SIMStat WHERE recid(SIMStat) = rtab[FRAME-LINE] NO-LOCK.
          /* THEN previous record */

          IF AVAILABLE SIMStat THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(SIMStat).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       FIND SIMStat WHERE recid(SIMStat) = rtab[FRAME-LINE]
       EXCLUSIVE-LOCK.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       SIMStat.SIMStat SIMStat.SSName /* sd */.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSIMStat).

           DELETE SIMStat.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST SIMStat
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND SIMStat WHERE recid(SIMStat) = rtab[FRAME-line(sel)]
       EXCLUSIVE-LOCK.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSIMStat).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY SIMStat.SIMStat.

       UPDATE SIMStat.SSName.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSIMStat).

       HIDE FRAME lis NO-PAUSE.
       DISPLAY SIMStat.SSName
       WITH FRAME sel.
       xrecid = recid(SIMStat).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SIMStat) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SIMStat) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST SIMStat
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST SIMStat USE-INDEX SSName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST SIMStat
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST SIMStat USE-INDEX SSName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT SIMStat
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT SIMStat USE-INDEX SSName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev SIMStat
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev SIMStat USE-INDEX SSName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       DISPLAY
       SIMStat.SIMStat SIMStat.SSName 
       WITH FRAME sel.
END PROCEDURE.

