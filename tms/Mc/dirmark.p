/* ----------------------------------------------------------------------
  MODULE .......: dirmark.p
  TASK .........: UPDATE Direct Marketing Codes
  APPLICATION ..: nn
  AUTHOR .......: ht
  CREATED ......: 19-03-02
  CHANGED ......: 16.09.03 jp Brand 
  VERSION ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable dmarketing

{Syst/commali.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR DirMark  LIKE DMarketing.DirMark  NO-UNDO.
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
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

form
    DMarketing.Brand 
    DMarketing.DirMark      /* COLUMN-LABEL FORMAT */
    DMarketing.DirMarkName     /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " DIRECT MARKETING CODES "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    "Direct marketing code ..:"  DMarketing.DirMark     NO-LABEL SKIP
    "Direct marketing name ..:"  DMarketing.DirMarkName NO-LABEL 
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr /* WITH side-labels 1 columns */
    FRAME lis.

form /* seek VAT Code  BY  DirMark */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "DirMarket.:" DirMark
    help "Enter Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Code  BY DirMarkName */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "DirName ..:" DirMarkName
    help "Enter Name"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = " By Code , By Name , By 3 , By 4" .


FIND FIRST DMarketing
WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE DMarketing THEN ASSIGN
   memory       = recid(DMarketing)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a DMarketing  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR DMarketing.DirMark.
           IF INPUT DMarketing.DirMark = "" THEN LEAVE ADD-ROW.
           CREATE DMarketing.
           ASSIGN
           DMarketing.DirMark = INPUT FRAME lis DMarketing.DirMark.

           RUN local-update-record.

           ASSIGN
           memory = recid(DMarketing)
           xrecid = memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST DMarketing
      WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DMarketing THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND DMarketing WHERE recid(DMarketing) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DMarketing THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DMarketing).
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
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"5,6"'}
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DMarketing.DirMark ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DMarketing.DirMark WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW DMarketing.DirMarkName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DMarketing.DirMarkName WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND DMarketing WHERE recid(DMarketing) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE DMarketing THEN
              ASSIGN FIRSTrow = i memory = recid(DMarketing).
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
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE DMarketing THEN DO:
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
                rtab[1] = recid(DMarketing)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE DMarketing THEN DO:
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
              rtab[FRAME-DOWN] = recid(DMarketing).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND DMarketing WHERE recid(DMarketing) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE DMarketing THEN DO:
           memory = recid(DMarketing).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE DMarketing THEN memory = recid(DMarketing).
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
           memory = rtab[FRAME-DOWN].
           FIND DMarketing WHERE recid(DMarketing) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       DirMark = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       Disp lcBrand With FRAME f1.
       UPDATE lcBrand WHEN gcAllBrand = TRUE
              DirMark WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF DirMark <> "" THEN DO:
          FIND FIRST DMarketing WHERE 
                     DMarketing.DirMark >= DirMark  AND 
                     DMarketing.Brand    = lcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       DirMarkName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        Disp lcBrand With FRAME f2.
       UPDATE  lcBrand WHEN gcAllBrand = TRUE
               DirMarkName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF DirMarkName <> "" THEN DO:
          FIND FIRST DMarketing WHERE 
                     DMarketing.DirMarkName >= DirMarkName AND 
                     DMarketing.Brand        = lcBrand 
          NO-LOCK NO-ERROR.

           IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       DMarketing.DirMark DMarketing.DirMarkName .

       RUN local-find-NEXT.
       IF AVAILABLE DMarketing THEN memory = recid(DMarketing).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE DMarketing THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(DMarketing).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       DMarketing.DirMark DMarketing.DirMarkName .
       IF ok THEN DO:

           DELETE DMarketing.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST DMarketing
           WHERE DMarketing.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION:
       {Syst/uright2.i} 
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       CLEAR FRAME lis NO-PAUSE.
       DISPLAY DMarketing.DirMark.
       RUN local-update-record.
       HIDE FRAME lis NO-PAUSE.
       RUN local-disp-row.
       xrecid = recid(DMarketing).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(DMarketing) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(DMarketing) must-print = TRUE.
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
      FIND DMarketing WHERE recid(DMarketing) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DMarketing WHERE recid(DMarketing) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST DMarketing
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST DMarketing USE-INDEX DirMarkName
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST DMarketing
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST DMarketing USE-INDEX DirMarkName
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT DMarketing
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT DMarketing USE-INDEX DirMarkName
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev DMarketing
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev DMarketing USE-INDEX DirMarkName
       WHERE DMarketing.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       DMarketing.Brand 
       DMarketing.DirMark
       DMarketing.DirMarkName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:
   RUN local-find-others.
   UPDATE
       DMarketing.DirMarkName

   WITH FRAME lis.

END PROCEDURE.

