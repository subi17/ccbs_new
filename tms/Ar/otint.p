/* ----------------------------------------------------------------------
  MODULE .......: Interest.P
  TASK .........: UPDATE Overtime Interests
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 15-06-99
  CHANGED ......: 07-10-99 jp urights added
                  11.02.02 ht IntType added
                  27.02.02 HT previous continued
                  31.10.02 jr Eventlog
                  05.03.03 tk tokens
                  15.09.03 aam brand
  Version ......: M15  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable Interest

{commali.i}
{eventval.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'interest'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ValidFrom  LIKE Interest.ValidFrom  NO-UNDO.
DEF VAR IntPerc LIKE Interest.IntPerc NO-UNDO.
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
DEF VAR IntTypeName  AS C                      NO-UNDO.
DEF VAR TypeNames    AS C                      NO-UNDO.
DEF VAR New_Int      AS LOG                    NO-UNDO INIT FALSE.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInterest AS HANDLE NO-UNDO.
   lhInterest = BUFFER Interest:HANDLE.
   RUN StarEventInitialize(lhInterest).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhInterest).
   END.
END.

TypeNames = "General,Fixed,Added to reference rate,Unknown !".

form
    Interest.Brand
    Interest.ValidFrom      /* COLUMN-LABEL FORMAT */
    Interest.IntPerc     /* COLUMN-LABEL FORMAT */
    Interest.IntType
    Interest.Memo
             /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Overtime interests "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Interest.ValidFrom     COLON 20 LABEL "Valid"            SKIP
    Interest.IntPerc      COLON 20 LABEL "Interest"         SKIP
    Interest.IntType      COLON 20 LABEL "Interest Type"
HELP "0=General, 1=Fixed, 2=Added to confirmed reference rate"
    IntTypeName              NO-LABEL FORMAT "x(25)"     SKIP
    Interest.Memo         COLON 20 LABEL "memo"

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{brand.i}

form /* seek Interest record  BY  ValidFrom */
    "Brand:" lcBrand skip
    "Date :" ValidFrom
    HELP "Enter Start Date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND DATE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Interest record  BY IntPerc */
    "Brand:" lcBrand skip
    "Perc.:" IntPerc
    HELP "Enter Percent"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND PERCENT "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By DATE,By PERC,By 3, By 4".


FIND FIRST Interest
WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Interest THEN ASSIGN
   Memory       = recid(Interest)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a Interest  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        New_Int = TRUE.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR Interest.ValidFrom
           VALIDATE
              (Interest.ValidFrom NOT ENTERED OR
              NOT CAN-FIND(Interest using  Interest.ValidFrom 
                            WHERE Interest.Brand = lcBrand),
              "Interest record " + string(INPUT Interest.ValidFrom) +
              " already exists !").
           IF INPUT FRAME lis Interest.ValidFrom NOT ENTERED THEN 
           LEAVE add-row.
           CREATE Interest.
           ASSIGN
           Interest.Brand     = lcBrand 
           Interest.ValidFrom = INPUT FRAME lis Interest.ValidFrom.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(Interest)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST Interest
      WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Interest THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Interest WHERE recid(Interest) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Interest THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Interest).
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
        ufk[1]= 28  ufk[2]= 789 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Interest.ValidFrom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Interest.ValidFrom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Interest.IntPerc ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Interest.IntPerc WITH FRAME sel.
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
        FIND Interest WHERE recid(Interest) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Interest THEN
              ASSIGN FIRSTrow = i Memory = recid(Interest).
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
           IF NOT AVAILABLE Interest THEN DO:
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
                rtab[1] = recid(Interest)
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
           IF NOT AVAILABLE Interest THEN DO:
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
              rtab[FRAME-DOWN] = recid(Interest).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Interest WHERE recid(Interest) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Interest THEN DO:
           Memory = recid(Interest).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Interest THEN Memory = recid(Interest).
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
           FIND Interest WHERE recid(Interest) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              ValidFrom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF ValidFrom NE ? THEN DO:

          FIND FIRST Interest WHERE 
              Interest.Brand = lcBrand AND
              Interest.ValidFrom <= ValidFrom
              NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              IntPerc WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF IntPerc >= 0 THEN DO:
          FIND FIRST Interest WHERE 
              Interest.Brand = lcBrand and
              Interest.IntPerc >= IntPerc
              USE-INDEX IntPerc  NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

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
          Interest.ValidFrom 
          Interest.IntPerc 
          Interest.IntType  
          Interest.Memo.

       RUN local-find-NEXT.
       IF AVAILABLE Interest THEN Memory = recid(Interest).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Interest THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Interest).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          Interest.ValidFrom 
          Interest.IntPerc 
          Interest.IntType
          Interest.Memo.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhInterest).
           DELETE Interest.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST Interest
           WHERE Interest.Brand = lcBrand) THEN DO:
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
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY Interest.ValidFrom.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(Interest).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Interest) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Interest) must-print = TRUE.
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
      FIND Interest WHERE recid(Interest) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Interest WHERE recid(Interest) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST Interest
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Interest USE-INDEX IntPerc
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST Interest
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Interest USE-INDEX IntPerc
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT Interest
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Interest USE-INDEX IntPerc
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV Interest
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV Interest USE-INDEX IntPerc
       WHERE Interest.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Interest.Brand
       Interest.ValidFrom
       Interest.IntPerc
       Interest.IntType
       Interest.Memo
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      IF llDoEvent AND NOT New_Int
      THEN RUN StarEventSetOldBuffer(lhInterest).

      IntTypeName = ENTRY(Interest.IntType + 1,TypeNames).
      DISP 
         IntTypeName 
         Interest.IntPerc
         Interest.IntType
         Interest.Memo
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:

         UPDATE
             Interest.IntPerc
             Interest.IntType
             Interest.Memo
         WITH FRAME lis
         EDITING:
                READKEY.
                IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                   PAUSE 0.
                   IF FRAME-FIELD = "IntType" THEN DO:
                      IF      INPUT FRAME lis IntType = 0 THEN 
                         IntTypeName = ENTRY(1,TypeNames).
                      ELSE IF INPUT FRAME lis IntType = 1 THEN 
                         IntTypeName = ENTRY(2,TypeNames).
                      ELSE IF INPUT FRAME lis IntType = 2 THEN 
                         IntTypeName = ENTRY(3,TypeNames).
                      ELSE DO:
                         BELL.
                         IntTypeName = ENTRY(4,TypeNames).
                         DISP IntTypeName.
                         NEXT.
                      END.
                      DISP IntTypeName.
                   END.
                END.
                APPLY LASTKEY.
             END. /* EDITING */
         END.
         ELSE PAUSE.

      LEAVE.
   END.
   IF llDoEvent AND NOT New_Int
   THEN RUN StarEventMakeModifyEvent(lhInterest).

   IF llDoEvent AND New_int 
   THEN RUN StarEventMakeCreateEvent(lhInterest).

   New_Int = FALSE.

END PROCEDURE.

