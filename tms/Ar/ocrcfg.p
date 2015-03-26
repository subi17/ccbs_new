/* ----------------------------------------------------------------------
  MODULE .......: PaymCfg.P
  TASK .........: PaymCfg
  APPLICATION ..: nn
  AUTHOR .......: aam
  CREATED ......: 09.04.2001
  CHANGED ......: 21.05.2002/tk Eventlogging added
                  05.03.2003/tk tokens
                  15.09.2003/aam brand
                  29.12.04/aam longer format for PaymFile
                  08.03.07/aam longer format for AccNum
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable PaymCfg

{commali.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'paymcfg'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPaymCfg AS HANDLE NO-UNDO.
   lhPaymCfg = BUFFER PaymCfg:HANDLE.
   RUN StarEventInitialize(lhPaymCfg).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhPaymCfg).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR PaymCfg  LIKE PaymCfg.PaymCfg  NO-UNDO.
DEF VAR Origin  LIKE PaymCfg.Origin NO-UNDO.
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
    PaymCfg.Brand  format "x(4)" COLUMN-LABEL "Bran"
    PaymCfg.PaymCfg      /* COLUMN-LABEL FORMAT */
    PaymCfg.Origin format "x(15)"    /* COLUMN-LABEL FORMAT */
    PaymCfg.PaymFile format "x(17)" 
    PaymCfg.ConvMod format "x(10)"  
    PaymCfg.PaymAccNum FORMAT ">>>>>>>9"
            /* COLUMN-LABEL FORMAT */
    Account.AccName column-label "Name" format "x(10)"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " PAYMENT FILE CONFIGURATION "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    PaymCfg.PaymCfg     /* LABEL FORMAT */
    PaymCfg.Origin    /* LABEL FORMAT */
    PaymCfg.PaymFile FORMAT "X(60)"
    PaymCfg.ConvMod
    PaymCfg.PaymAccNum FORMAT ">>>>>>>9"
    Account.AccName label "Account Name"
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

{brand.i}

form /* seek PaymCfg  BY  PaymCfg */
    "Brand:" lcBrand skip
    "Code :" PaymCfg
    HELP "Enter Code of PaymCfg"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek PaymCfg  BY Origin */
    "Brand:" lcBrand skip
    "Name :" Origin
    HELP "Enter Name of PaymCfg"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".


RUN local-find-first.
IF AVAILABLE PaymCfg THEN ASSIGN
   Memory       = recid(PaymCfg)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Payment Configuration records available !" VIEW-AS ALERT-BOX.
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a PaymCfg  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR PaymCfg.PaymCfg
           VALIDATE
              (PaymCfg.PaymCfg NOT ENTERED OR
              NOT CAN-FIND(PaymCfg using  PaymCfg.PaymCfg WHERE
                            PaymCfg.Brand = lcBrand),
              "PaymCfg " + string(INPUT PaymCfg.PaymCfg) +
              " already exists !").
           IF INPUT FRAME lis PaymCfg.PaymCfg NOT ENTERED THEN 
           LEAVE add-row.
           CREATE PaymCfg.
           ASSIGN
           PaymCfg.Brand   = lcBrand
           PaymCfg.PaymCfg = INPUT FRAME lis PaymCfg.PaymCfg.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPaymCfg).

           ASSIGN
           Memory = recid(PaymCfg)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PaymCfg THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PaymCfg WHERE recid(PaymCfg) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PaymCfg THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PaymCfg).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW PaymCfg.PaymCfg ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PaymCfg.PaymCfg WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW PaymCfg.Origin ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PaymCfg.Origin WITH FRAME sel.
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
        FIND PaymCfg WHERE recid(PaymCfg) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PaymCfg THEN
              ASSIGN FIRSTrow = i Memory = recid(PaymCfg).
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
           IF NOT AVAILABLE PaymCfg THEN DO:
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
                rtab[1] = recid(PaymCfg)
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
           IF NOT AVAILABLE PaymCfg THEN DO:
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
              rtab[FRAME-DOWN] = recid(PaymCfg).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PaymCfg WHERE recid(PaymCfg) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PaymCfg THEN DO:
           Memory = recid(PaymCfg).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PaymCfg THEN Memory = recid(PaymCfg).
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
           FIND PaymCfg WHERE recid(PaymCfg) = Memory NO-LOCK.
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
              PaymCfg WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF PaymCfg NE "" THEN DO:
          FIND FIRST PaymCfg WHERE 
             PaymCfg.PaymCfg >= PaymCfg AND
             PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              Origin WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF Origin NE "" THEN DO:
          FIND FIRST PaymCfg WHERE 
             PaymCfg.Brand = lcBrand AND
             PaymCfg.Origin >= Origin
          USE-INDEX Origin NO-LOCK NO-ERROR.

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
          PaymCfg.PaymCfg 
          PaymCfg.Origin 
          PaymCfg.PaymFile
          PaymCfg.ConvMod 
          PaymCfg.PaymAccNum
          Account.AccName.

       RUN local-find-NEXT.
       IF AVAILABLE PaymCfg THEN Memory = recid(PaymCfg).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PaymCfg THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(PaymCfg).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          PaymCfg.PaymCfg 
          PaymCfg.Origin 
          PaymCfg.PaymFile
          PaymCfg.ConvMod 
          PaymCfg.PaymAccNum
          Account.AccName.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPaymCfg).

           DELETE PaymCfg.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST PaymCfg
           WHERE PaymCfg.Brand = lcBrand) THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPaymCfg).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PaymCfg.PaymCfg.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPaymCfg).

       RUN local-disp-row.
       xrecid = recid(PaymCfg).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PaymCfg) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PaymCfg) must-print = TRUE.
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
      FIND PaymCfg WHERE recid(PaymCfg) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PaymCfg WHERE recid(PaymCfg) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PaymCfg
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST PaymCfg USE-INDEX Origin
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PaymCfg
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST PaymCfg USE-INDEX Origin
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PaymCfg
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT PaymCfg USE-INDEX Origin
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PaymCfg
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV PaymCfg USE-INDEX Origin
       WHERE PaymCfg.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       PaymCfg.Brand
       PaymCfg.PaymCfg
       PaymCfg.Origin
       PaymCfg.PaymFile
       PaymCfg.ConvMod
       PaymCfg.PaymAccNum
       Account.AccName WHEN AVAILABLE Account
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
    FIND Account WHERE
         Account.Brand  = PaymCfg.Brand AND
         Account.AccNum = PaymCfg.PaymAccNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP 
          Account.AccName WHEN AVAILABLE Account
          PaymCfg.Origin
          PaymCfg.PaymFile
          PaymCfg.ConvMod
          PaymCfg.PaymAccNum
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:

         UPDATE
             PaymCfg.Origin
             PaymCfg.PaymFile
             PaymCfg.ConvMod
             PaymCfg.PaymAccNum
         WITH FRAME lis
         EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "PaymAccNum" THEN DO:
                   IF INPUT FRAME lis PaymCfg.PaymAccNum = 0 THEN DO:
                      DISP "" @ Account.AccName WITH FRAME lis.
                   END.

                   ELSE DO:    
                      FIND Account WHERE 
                           Account.Brand = PaymCfg.Brand AND
                           Account.AccNum = INPUT FRAME lis PaymCfg.PaymAccNum
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL Account THEN DO:
                         BELL.
                         MESSAGE "Unknown Account !".
                         NEXT.
                      END.
                      ELSE IF Account.AccType NE 4 THEN DO:
                         BELL. 
                         MESSAGE "Account's RepType is not 4 (payment).".
                         NEXT.
                      END.
                      DISP Account.AccName.
                   END.   
                END.
             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

