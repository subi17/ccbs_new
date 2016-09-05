/* ----------------------------------------------------------------------
  MODULE .......: AccPeriod
  TASK .........: UPDATEs table AccPeriod
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.09.02
  CHANGED ......: 13.09.02/aam tuning
                  05.03.03/tk  tokens
                  04.09.03/aam brand
  Version ......: m15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable AccPeriod

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'accperiod'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhAccPeriod AS HANDLE NO-UNDO.
   lhAccPeriod = BUFFER AccPeriod:HANDLE.
   RUN StarEventInitialize(lhAccPeriod).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhAccPeriod).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR AccPeriod    LIKE AccPeriod.Period     NO-UNDO.
DEF VAR FromDate     LIKE AccPeriod.FromDate   NO-UNDO.
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
DEF VAR oldestdate   AS DATE FORMAT "99.99.99" No-UNDO . 

DEF VAR liDefBeg     AS INT NO-UNDO.
DEF VAR liDefEnd     AS INT NO-UNDO.



form
    AccPeriod.Brand
    AccPeriod.Period       /* COLUMN-LABEL FORMAT */
    AccPeriod.FromDate     /* COLUMN-LABEL FORMAT */
    AccPeriod.ToDate
    AccPeriod.PerLocked

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " ACCOUNTING PERIODS "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    AccPeriod.Brand     COLON 12
    AccPeriod.Period    COLON 12  /* LABEL FORMAT */
    AccPeriod.FromDate  COLON 12   /* LABEL FORMAT */
    AccPeriod.ToDate    COLON 12
    AccPeriod.PerLocked COLON 12
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  AccPeriod */
    "Brand :" lcBrand skip
    "Period:" AccPeriod
    HELP "Enter Period "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Period "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  FromDate */
    "Brand:" lcBrand skip
    "Date :" FromDate
    HELP "Enter period's begin date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Period ,  By Date ,By 3, By 4".


/* set the reasonable period limit */
ASSIGN liDefBeg = (YEAR(TODAY) - 5) * 100 + 1
       liDefEnd = (YEAR(TODAY) + 5) * 100 + 12.

RUN local-Find-First.
IF AVAILABLE AccPeriod THEN ASSIGN
   Memory       = recid(AccPeriod)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No account periods available !" VIEW-AS ALERT-BOX.
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a AccPeriod  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ AccPeriod.Brand.

           PROMPT-FOR AccPeriod.Period
                      AccPeriod.FromDate
                      AccPeriod.ToDate.

           IF INPUT FRAME lis AccPeriod.Period = 0 THEN 
           LEAVE add-row.

           IF CAN-FIND(AccPeriod WHERE
                       AccPeriod.Brand  = lcBrand AND
                       AccPeriod.Period = INPUT FRAME lis AccPeriod.Period)
           THEN DO:
              MESSAGE 
              "Accounting period" INPUT FRAME lis AccPeriod.Period
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           IF INPUT FRAME lis AccPeriod.FromDate = ? OR 
              INPUT FRAME lis AccPeriod.ToDate   = ?
           THEN DO:
              MESSAGE "Dates are mandatory."
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           IF INPUT FRAME lis AccPeriod.ToDate < 
              INPUT FRAME lis AccPeriod.FromDate
           THEN DO:
              MESSAGE "Period's end date cannot be earlier than"
                      "beginning date."
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           /* overlapping time period */
           IF 
           CAN-FIND(FIRST AccPeriod WHERE 
              AccPeriod.Brand     = lcBrand AND
              AccPeriod.FromDate <= INPUT FRAME lis AccPeriod.FromDate AND
              AccPeriod.ToDate   >= INPUT FRAME lis AccPeriod.FromDate) 
           OR
           CAN-FIND(FIRST AccPeriod WHERE
              AccPeriod.Brand     = lcBrand AND
              AccPeriod.FromDate <= INPUT FRAME lis AccPeriod.ToDate   AND
              AccPeriod.ToDate   >= INPUT FRAME lis AccPeriod.ToDate)
           THEN DO:
              MESSAGE "A period has already been defined for this time period"
              VIEW-AS ALERT-BOX
              ERROR. 
              NEXT. 
           END.

           /* is given period sane */
           IF YEAR(INPUT FRAME lis AccPeriod.FromDate) < YEAR(TODAY) - 5 OR
              YEAR(INPUT FRAME lis AccPeriod.FromDate) > YEAR(TODAY) + 5 OR
              YEAR(INPUT FRAME lis AccPeriod.ToDate)   < YEAR(TODAY) - 5 OR
              YEAR(INPUT FRAME lis AccPeriod.ToDate)   > YEAR(TODAY) + 5 
           THEN DO:
              Ok = FALSE.
              MESSAGE "Given period is over 5 years from now,"
                      "shall the period nevertheless be created ?"
              VIEW-AS ALERT-BOX
              QUESTION
              BUTTONS YES-NO
              SET Ok.
              IF NOT Ok THEN NEXT. 
           END.

           CREATE AccPeriod.
           ASSIGN
           AccPeriod.Brand    = lcBrand
           AccPeriod.Period   = INPUT FRAME lis AccPeriod.Period
           AccPeriod.FromDate = INPUT FRAME lis AccPeriod.FromDate
           AccPeriod.ToDate   = INPUT FRAME lis AccPeriod.ToDate.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhAccPeriod).

           ASSIGN
           Memory = recid(AccPeriod)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST AccPeriod
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE AccPeriod THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND AccPeriod WHERE recid(AccPeriod) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE AccPeriod THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(AccPeriod).
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
        ufk[1]= 771 ufk[2]= 28 ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW AccPeriod.Period {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) AccPeriod.Period WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW AccPeriod.FromDate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) AccPeriod.FromDate WITH FRAME sel.
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
        FIND AccPeriod WHERE recid(AccPeriod) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE AccPeriod THEN
              ASSIGN FIRSTrow = i Memory = recid(AccPeriod).
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
           IF NOT AVAILABLE AccPeriod THEN DO:
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
                rtab[1] = recid(AccPeriod)
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
           IF NOT AVAILABLE AccPeriod THEN DO:
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
              rtab[FRAME-DOWN] = recid(AccPeriod).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND AccPeriod WHERE recid(AccPeriod) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE AccPeriod THEN DO:
           Memory = recid(AccPeriod).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE AccPeriod THEN Memory = recid(AccPeriod).
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
           FIND AccPeriod WHERE recid(AccPeriod) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           AccPeriod WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF AccPeriod ENTERED THEN DO:
          FIND FIRST AccPeriod WHERE 
                     AccPeriod.Brand = lcBrand AND
                     AccPeriod.Period >= AccPeriod
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       SET lcBrand WHEN gcAllBrand
           FromDate WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF FromDate ENTERED THEN DO:
          FIND FIRST AccPeriod WHERE 
                     AccPeriod.Brand     = lcBrand AND
                     AccPeriod.FromDate >= FromDate
          USE-INDEX FromDate  NO-LOCK NO-ERROR.

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
       AccPeriod.Period AccPeriod.FromDate AccPeriod.ToDate 
       AccPeriod.PerLocked.

       RUN local-find-NEXT.
       IF AVAILABLE AccPeriod THEN Memory = recid(AccPeriod).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE AccPeriod THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(AccPeriod).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       AccPeriod.Period AccPeriod.FromDate AccPeriod.ToDate
       AccPeriod.PerLocked.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhAccPeriod).

           DELETE AccPeriod.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST AccPeriod) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhAccPeriod).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY AccPeriod.Period.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhAccPeriod).

       RUN local-disp-row.
       xrecid = recid(AccPeriod).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(AccPeriod) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(AccPeriod) must-print = TRUE.
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
      FIND AccPeriod WHERE recid(AccPeriod) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND AccPeriod WHERE recid(AccPeriod) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST AccPeriod 
          WHERE AccPeriod.Brand = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST AccPeriod 
          WHERE AccPeriod.Brand = lcBrand
          USE-INDEX FromDate NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST AccPeriod
          WHERE AccPeriod.Brand = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST AccPeriod 
          WHERE AccPeriod.Brand = lcBrand
          USE-INDEX FromDate NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT AccPeriod
          WHERE AccPeriod.Brand = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT AccPeriod 
          WHERE AccPeriod.Brand = lcBrand
          USE-INDEX FromDate NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV AccPeriod
          WHERE AccPeriod.Brand = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV AccPeriod 
          WHERE AccPeriod.Brand = lcBrand
          USE-INDEX FromDate NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       AccPeriod.Brand
       AccPeriod.Period 
       AccPeriod.FromDate
       AccPeriod.ToDate
       AccPeriod.PerLocked

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP AccPeriod.Brand
           AccPeriod.Period
           AccPeriod.FromDate
           AccPeriod.ToDate
      WITH FRAME lis.

      UPDATE
          AccPeriod.PerLocked
      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

