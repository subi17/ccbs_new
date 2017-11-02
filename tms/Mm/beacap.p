/* ----------------------------------------------------------------------
  MODULE .......: BeaCap.P
  TASK .........: Bearer Capability codes
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 09-06-99
  CHANGED ......: 04-10-99 jp urights added
                  20.05.02/tk Event logging added
                  28.02.03 tk tokens
                  04.09.03 jp brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'beacap'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhBeaCap AS HANDLE NO-UNDO.
   lhBeaCap = BUFFER BeaCap:HANDLE.
   RUN StarEventInitialize(lhBeaCap).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhBeaCap).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR BeaCap    LIKE BeaCap.BeaCap  NO-UNDO.
DEF VAR BcName    LIKE BeaCap.BcName NO-UNDO.
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

&GLOBAL-DEFINE BrTable BeaCap

form
    BeaCap.Brand 
    BeaCap.BeaCap   /* COLUMN-LABEL FORMAT */
    BeaCap.BcName     /* COLUMN-LABEL FORMAT */
                       /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)
    title COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
    " Bearer Capability codes "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.

form
    BeaCap.BeaCap     /* LABEL FORMAT */
    BeaCap.BcName       /* LABEL FORMAT */
                         /* LABEL FORMAT */

    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* seek BeaCap  BY  BeaCap */
    BeaCap
    help "Enter BeaCap's number"
    WITH row 4 col 2 title COLOR VALUE(Syst.Var:ctc) " FIND number "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek BeaCap  BY BcName */
    BCName
    help "Enter BeaCap's Name"
    WITH row 4 col 2 title COLOR VALUE(Syst.Var:ctc) " FIND Name "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
view FRAME sel.

orders = "By Number,By Name  ,By 3, By 4".
{Func/brand.i}

FIND FIRST BeaCap
WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE BeaCap THEN ASSIGN
   Memory       = recid(BeaCap)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No beacaps available !" VIEW-AS ALERT-BOX.
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a BeaCap  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.
        DO TRANSACTION:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR BeaCap.BeaCap
           VALIDATE
              (BeaCap.BeaCap NOT ENTERED OR
              NOT CAN-FIND(BeaCap using  BeaCap.BeaCap),
              "BeaCap " + string(INPUT BeaCap.BeaCap) +
              " already exists !").
           IF BeaCap.BeaCap NOT ENTERED THEN LEAVE add-row.
           CREATE BeaCap.
           ASSIGN
           BeaCap.Brand  = lcBrand
           BeaCap.BeaCap = INPUT FRAME lis BeaCap.BeaCap.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY), "ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBeaCap).

           ASSIGN
           Memory = recid(BeaCap)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST BeaCap
      WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BeaCap THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND BeaCap WHERE recid(BeaCap) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BeaCap THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BeaCap).
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
        Syst.Var:ufk[1]= 36  Syst.Var:ufk[2]= 30 Syst.Var:ufk[3]= 0 Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        Syst.Var:ufk[6]= (IF lcRIght = "RW" THEN 4 ELSE 0)
        Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW BeaCap.BeaCap {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) BeaCap.BeaCap WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW BeaCap.BcName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) BeaCap.BcName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND BeaCap WHERE recid(BeaCap) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE BeaCap THEN
              ASSIGN FIRSTrow = i Memory = recid(BeaCap).
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

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE BeaCap THEN DO:
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
                rtab[1] = recid(BeaCap)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE BeaCap THEN DO:
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
              rtab[FRAME-DOWN] = recid(BeaCap).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND BeaCap WHERE recid(BeaCap) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE BeaCap THEN DO:
           Memory = recid(BeaCap).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE BeaCap THEN Memory = recid(BeaCap).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND BeaCap WHERE recid(BeaCap) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET BeaCap WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF BeaCap ENTERED THEN DO:
          FIND FIRST BeaCap WHERE BeaCap.BeaCap >= BeaCap AND
           BeaCap.Brand = lcBrand
          NO-LOCK NO-ERROR.

           IF NOT fRecFound(1) THEN NEXT .

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       SET BcName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF BcName ENTERED THEN DO:
          FIND FIRST BeaCap USE-INDEX bcName  WHERE 
                     BeaCap.BcName >= BCName   AND 
                     BeaCap.Brand   = lcBrand 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE BeaCap THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some BeaCap/BCName was found */
          ASSIGN order = 2 Memory = recid(BeaCap) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       BeaCap.BeaCap BeaCap.BcName BeaCap.Brand.

       RUN local-find-NEXT.
       IF AVAILABLE BeaCap THEN Memory = recid(BeaCap).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE BeaCap THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(BeaCap).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       BeaCap.BeaCap BeaCap.BcName BeaCap.Brand .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBeaCap).

           DELETE BeaCap.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST BeaCap
           WHERE BeaCap.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBeaCap).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9.
       RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p.
       CLEAR FRAME lis NO-PAUSE.
       DISPLAY BeaCap.BeaCap.
       RUN local-UPDATE-record.
       HIDE FRAME lis NO-PAUSE.
       RUN local-disp-row.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBeaCap).

       xrecid = recid(BeaCap).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(BeaCap) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(BeaCap) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.



PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND BeaCap WHERE recid(BeaCap) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND BeaCap WHERE recid(BeaCap) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST BeaCap
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST BeaCap USE-INDEX bCName
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST BeaCap
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST BeaCap USE-INDEX BCName
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT BeaCap
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT BeaCap USE-INDEX BCName
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev BeaCap
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev BeaCap USE-INDEX BCName
       WHERE BeaCap.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       BeaCap.Brand
       BeaCap.BeaCap
       BeaCap.BcName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          BeaCap.BcName
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
         UPDATE
            BeaCap.BcName
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

