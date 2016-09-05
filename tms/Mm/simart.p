
/* ----------------------------------------------------------------------
  MODULE .......: simart.p
  TASK .........: UPDATE SIM Articles
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 01-06-99
  CHANGED ......: 07-10-99 jp urights added
                  21.05.02/tk Event logging added
                  05.09.02 jp validation
                  07.03.03 tk tokens
                  09.09.03 jp brand
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable simart

{Syst/commali.i}                  
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'simart'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSimArt AS HANDLE NO-UNDO.
   lhSimArt = BUFFER SimArt:HANDLE.
   RUN StarEventInitialize(lhSimArt).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSimArt).
   END.
END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR SimArt  LIKE SimArt.SimArt  NO-UNDO.
DEF VAR SAName LIKE SimArt.SAName NO-UNDO.
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
DEF VAR ok           AS log format "Yes/No"  NO-UNDO.

form
    SimArt.Brand   FORMAT "x(6)"
    SimArt.SimArt   /* COLUMN-LABEL FORMAT */
    SimArt.SAName  format "x(14)"
    SimArt.Balance
    SimArt.OrdPoint
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " SIM Articles "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    SimArt.SimArt     /* LABEL FORMAT */
    SimArt.SAName     /* LABEL FORMAT */
    VALIDATE(simart.SAName  ne "","Missing Sim Article Name!")
    SimArt.Balance
    SimArt.OrdPoint
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* Detailed Balance */

    SimArt.DetBal[1]  label "In Stock, Arrival tested ..."
    SimArt.DetBal[2]  label "In Stock, pre-pers. ........"
    SimArt.DetBal[3]  label "At Retailer Shops .........."
    SimArt.DetBal[4]  label "At Customers ..............."
    SimArt.DetBal[5]  label "In use ....................."
    SimArt.DetBal[6]  label "Discarded .................."
with centered overlay title "Detailed Balance of Article " 
    + SimArt.SimArt + " "
    ROW 5 side-labels FRAME dbal.


form /* seek SIM Article  BY  SimArt */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "SimArticle:" SimArt
    help "Enter Article Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek SIM Article  BY SAName */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP

    "ArticlName:" SAName
    help "Enter Article Name"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".


FIND FIRST SimArt
WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE SimArt THEN ASSIGN
   Memory       = recid(SimArt)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No SIM Articles available !" VIEW-AS ALERT-BOX.
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 37 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a SimArt  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSACTION:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR SimArt.SimArt
           VALIDATE
              (SimArt.SimArt = "" OR
              NOT CAN-FIND(SimArt using  SimArt.SimArt),
              "SIM Article " + string(INPUT SimArt.SimArt) +
              " already exists !").
           IF INPUT SimArt.SimArt = "" THEN LEAVE ADD-ROW.
           CREATE SimArt.
           ASSIGN
           SimArt.Brand  = lcBrand 
           SimArt.SimArt = INPUT FRAME lis SimArt.SimArt.

           RUN local-UPDATE-record.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSimArt).

           ASSIGN
           Memory = recid(SimArt)
           xrecid = Memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST SimArt
      WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SimArt THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND SimArt WHERE recid(SimArt) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SimArt THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SimArt).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 208 ufk[4]= 202
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SimArt.SimArt {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SimArt.SimArt WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SimArt.SAName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SimArt.SAName WITH FRAME sel.
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
        FIND SimArt WHERE recid(SimArt) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE SimArt THEN
              ASSIGN FIRSTrow = i Memory = recid(SimArt).
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
           IF NOT AVAILABLE SimArt THEN DO:
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
                rtab[1] = recid(SimArt)
                Memory  = rtab[1].
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
           IF NOT AVAILABLE SimArt THEN DO:
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
              rtab[FRAME-DOWN] = recid(SimArt).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SimArt WHERE recid(SimArt) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE SimArt THEN DO:
           Memory = recid(SimArt).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE SimArt THEN Memory = recid(SimArt).
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
           FIND SimArt WHERE recid(SimArt) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       SimArt = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       Disp lcBrand With FRAME f1.
       UPDATE 
           lcBrand WHEN gcAllBrand = TRUE  SimArt WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF SimArt <> "" THEN DO:
          FIND FIRST SimArt WHERE 
                     SimArt.SimArt >= SimArt  AND
                     SimArt.Brand = lcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       SAName = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       Disp lcBrand With FRAME f2.

       UPDATE  lcBrand WHEN gcAllBrand = TRUE
               SAName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF SAName <> "" THEN DO:
          FIND FIRST SimArt USE-INDEX saname WHERE 
                     SimArt.SAName >= SAName   AND
                     SimArt.Brand = lcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(2) THEN NEXT Browse.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANSACTION:  /* DET. BAL */
       ufkey = TRUE. 
       FIND SimArt WHERE recid(SimArt) = rtab[FRAME-LINE] /* NO-LOCK. */.

       RUN Mm/stobal3.p(Simart.Brand,SimArt.SimArt).
       NEXT loop.
     END.  

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANSACTION:  /* DET. BAL */
       ufkey = TRUE. ufk = 0. ehto = 3. RUN Syst/ufkey.p.
       FIND SimArt WHERE recid(SimArt) = rtab[FRAME-LINE] NO-LOCK. 
       PAUSE 0.
       DISP SimArt.DetBal[1 FOR 6] WITH FRAME dbal.
       message "Press ENTER to continue !".
       PAUSE no-message.
       HIDE FRAME dbal.
       NEXT LOOP.
     END.


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
       SimArt.SimArt SimArt.SAName SimArt.Brand.

       RUN local-find-NEXT.
       IF AVAILABLE SimArt THEN Memory = recid(SimArt).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE SimArt THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(SimArt).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       SimArt.SimArt SimArt.SAName SimArt.Brand.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSimArt).

           DELETE SimArt.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST SimArt
           WHERE SimArt.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     
     /* "ENTER" OPTION DISABLED: This is not important at the moment.*/
     /* In future if needed, add optimistic-locking to provide fluent usage. */
     
     /*
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSimArt).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p.
       CLEAR FRAME lis NO-PAUSE.
       DISPLAY SimArt.SimArt.
       RUN local-UPDATE-record.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSimArt).

       HIDE FRAME lis NO-PAUSE.
       RUN local-disp-row.
       xrecid = recid(SimArt).
     END.
     */

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SimArt) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SimArt) must-print = TRUE.
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
      FIND SimArt WHERE recid(SimArt) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND SimArt WHERE recid(SimArt) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST SimArt
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST SimArt USE-INDEX SAName
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST SimArt
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST SimArt USE-INDEX SAName
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT SimArt
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT SimArt USE-INDEX SAName
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev SimArt
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev SimArt USE-INDEX SAName
       WHERE SimArt.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       SimArt.Brand
       SimArt.SimArt 
       SimArt.SAName
       SimArt.Balance
       SimArt.OrdPoint
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   RUN local-find-others.
   DISP
       SimArt.SAName
       SimArt.Balance
       SimArt.OrdPoint
   WITH FRAME lis.

   IF lcRight = "RW" THEN
      UPDATE
          SimArt.SAName
          SimArt.Balance
          SimArt.OrdPoint WITH FRAME lis.
   ELSE PAUSE.    
END PROCEDURE.

