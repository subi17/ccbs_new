/* ----------------------------------------------------------------
  MODULE .......: SimMan.P
  TASK .........: UPDATE SIM Card Manufacturers
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 31-05-99
  CHANGED ......: 07-10-99 jp urights added
                  21.05.02/tk Event logging added
                  07.03.03/tk tokens
                  09.09.03jp brand
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable bnet

{commali.i}                
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'simman'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSimMan AS HANDLE NO-UNDO.
   lhSimMan = BUFFER SimMan:HANDLE.
   RUN StarEventInitialize(lhSimMan).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhSimMan).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Mancode  LIKE SimMan.Mancode  NO-UNDO.
DEF VAR ManName LIKE SimMan.ManName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR MaxOrder     AS i                      NO-UNDO  init 2.
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
    SimMan.Brand 
    SimMan.Mancode      /* COLUMN-LABEL FORMAT */
    SimMan.ManName     /* COLUMN-LABEL FORMAT */
    WITH width 80 OVERLAY SCROLL 1 15 DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " SIM Card Vendors/Manufacturers "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{brand.i}

form
    SimMan.Mancode     /* LABEL FORMAT */
    SimMan.ManName    /* LABEL FORMAT */
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* seek Manufacturer  BY  Mancode */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Man.Code..:"     mancode
    help "Enter ...."
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Manufacturer  BY ManName */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Man.name .:"    manname
    help "Enter ..."
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".
FIND FIRST SimMan
WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE SimMan THEN ASSIGN
   Memory       = recid(SimMan)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No SIM Manufacturers available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.   
   ELSE ASSIGN
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

   IF must-add THEN DO:  /* Add a SimMan  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.
ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.
        DO TRANSACTION:
           PROMPT-FOR SimMan.Mancode
           VALIDATE
              (SimMan.Mancode = "" OR
              NOT CAN-FIND(SimMan using  SimMan.Mancode),
              "Manufacturer " + string(INPUT SimMan.Mancode) +
              " already exists !").
           IF input SimMan.Mancode = "" THEN LEAVE ADD-ROW.
           CREATE SimMan.
           ASSIGN
           SimMan.Brand   = gcBrand 
           SimMan.Mancode = INPUT FRAME lis SimMan.Mancode.
           UPDATE SimMan.ManName.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSimMan).       

           ASSIGN
           Memory = recid(SimMan)
           xrecid = Memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST SimMan
      WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SimMan THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND SimMan WHERE recid(SimMan) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SimMan THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SimMan).
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
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SimMan.Mancode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SimMan.Mancode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SimMan.ManName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SimMan.ManName WITH FRAME sel.
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
        FIND SimMan WHERE recid(SimMan) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE SimMan THEN
              ASSIGN FIRSTrow = i Memory = recid(SimMan).
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
           FIND SimMan WHERE recid(SimMan) = rtab[FRAME-LINE] NO-LOCK.
           RUN local-find-prev.
           IF NOT AVAILABLE SimMan THEN DO:
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
                rtab[1] = recid(SimMan)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND SimMan WHERE recid(SimMan) = rtab[FRAME-DOWN] NO-LOCK .
           RUN local-find-NEXT.
           IF NOT AVAILABLE SimMan THEN DO:
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
              rtab[FRAME-DOWN] = recid(SimMan).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SimMan WHERE recid(SimMan) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE SimMan THEN DO:
           Memory = recid(SimMan).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE SimMan THEN Memory = recid(SimMan).
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
           FIND SimMan WHERE recid(SimMan) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       Mancode = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
        Disp lcBrand With FRAME f1.
       UPDATE  lcBrand WHEN gcAllBrand = TRUE
              Mancode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Mancode <> "" THEN DO:
          FIND FIRST SimMan WHERE 
                     SimMan.Mancode >= mancode AND 
                     simman.Brand   = gcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ManName = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
        Disp lcBrand With FRAME f2.
       UPDATE lcBrand WHEN gcAllBrand = TRUE
              ManName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ManName <> "" THEN DO:
          FIND FIRST SimMan USE-INDEX manname WHERE 
                     SimMan.ManName >= manname   AND 
                     simman.Brand = gcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(2) THEN NEXT Browse.

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
       FIND SimMan WHERE recid(SimMan) = rtab[FRAME-LINE] NO-LOCK.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       SimMan.Mancode SimMan.ManName  SimMan.Brand /* sd */.

       RUN local-find-NEXT.
       IF AVAILABLE SimMan THEN Memory = recid(SimMan).
       ELSE DO:
          /* read back the record that is TO be  removed */
          FIND SimMan WHERE recid(SimMan) = rtab[FRAME-LINE] NO-LOCK.
          /* THEN previous record */

          IF AVAILABLE SimMan THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(SimMan).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       FIND SimMan WHERE recid(SimMan) = rtab[FRAME-LINE]
       EXCLUSIVE-LOCK.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       SimMan.Mancode SimMan.ManName SimMan.Brand.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSimMan).

           DELETE SimMan.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST SimMan
           WHERE simman.Brand = gcBrand) THEN DO:
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
       /* change */
       FIND SimMan WHERE recid(SimMan) = rtab[FRAME-line(sel)]
       EXCLUSIVE-LOCK.


       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". run ufcolor.
       DISPLAY 
          SimMan.Brand
          SimMan.Mancode
          SimMan.ManName .

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSimMan).

          UPDATE SimMan.ManName.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSimMan).
       END.
       ELSE PAUSE.


       HIDE FRAME lis NO-PAUSE.
       DISPLAY SimMan.ManName SimMan.Brand
       WITH FRAME sel.
       xrecid = recid(SimMan).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SimMan) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SimMan) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST SimMan
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST SimMan USE-INDEX manName
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST SimMan
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST SimMan USE-INDEX manName
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT SimMan
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT SimMan USE-INDEX manName
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev SimMan
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev SimMan USE-INDEX manName
       WHERE simman.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       DISPLAY
       SimMan.Mancode SimMan.ManName SimMan.Brand
       WITH FRAME  sel.
END PROCEDURE.

