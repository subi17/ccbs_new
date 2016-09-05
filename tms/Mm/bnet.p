/* ----------------------------------------------------------------------
  MODULE .......: bnet
  TASK .........: Updates table bnet
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 05.09.03 jp brand
  VERSION ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable bnet

{Syst/commali.i}

def /* new */ shared var siirto AS char.

DEF VAR BnetCode      like bnet.BnetCode        NO-UNDO.
DEF VAR BnetName      like bnet.BnetName        NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

form
    bnet.Brand 
    bnet.BnetCode     /* column-label format */
    bnet.BnetName
    bnet.BnetValue     /* column-label format */

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  BNET for Mobile Operators and Service Providers  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    bnet.BnetCode     /* label format */
    bnet.BnetName 
    bnet.BnetValue    /* label format */
            /* label format */

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  BnetCode */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "B-Net Code:" BnetCode
    HELP "Enter Code of BNET "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek  BnetName */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "B-Net Name:"  BnetName
    HELP "Enter Name of the BNET"                                             
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.

form
    bnet.memo

    with overlay row 3 centered
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc)
    " MEMO: " + bnet.BnetName + " " WITH no-labels 1 columns
    frame f4.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST bnet
WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE bnet THEN ASSIGN
   memory       = recid(bnet)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = true.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a bnet  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR bnet.BnetCode
           validate
              (bnet.BnetCode NOT ENTERED or
              NOT CAN-FIND(bnet using  bnet.BnetCode WHERE
                           bnet.brand = gcBrand ),
              "Billing Type " + string(INPUT bnet.BnetCode) +
              " already exists !").
           IF INPUT FRAME lis bnet.BnetCode = "" THEN 
           LEAVE add-row.
           create bnet.
           ASSIGN
           bnet.Brand    = lcBrand 
           bnet.BnetCode = INPUT FRAME lis bnet.BnetCode.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(bnet)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST bnet
      WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bnet THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND bnet WHERE recid(bnet) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE bnet THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(bnet).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0  ufk[4]= 927
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row bnet.BnetCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) bnet.BnetCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row bnet.BnetName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) bnet.BnetName WITH FRAME sel.
      END.

      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND bnet WHERE recid(bnet) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE bnet THEN
              ASSIGN FIRSTrow = i memory = recid(bnet).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE bnet THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(bnet)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE bnet THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(bnet).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND bnet WHERE recid(bnet) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE bnet THEN DO:
           memory = recid(bnet).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE bnet THEN memory = recid(bnet).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND bnet WHERE recid(bnet) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME f1.
       Disp lcBrand With FRAME f1.
       SET   lcBrand WHEN gcAllBrand = TRUE  BnetCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF BnetCode ENTERED THEN DO:
          FIND FIRST bnet WHERE bnet.BnetCode >= BnetCode
           AND BNet.Brand = lcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME F2.
       Disp lcBrand With FRAME f2.

       SET  lcBrand WHEN gcAllBrand = TRUE BnetName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF BnetName ENTERED THEN DO:
          FIND FIRST bnet USE-INDEX BnetName  
          WHERE bnet.BnetName >= BnetName AND 
           BNet.Brand = lcBrand NO-LOCK NO-ERROR.

           IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Update Memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANS on ENDkey undo, NEXT LOOP:
        {Syst/uright2.i}.

        cfc = "puyr". RUN Syst/ufcolor.p.
        ehto = 9. 
        RUN Syst/ufkey.p. ufkey = true.
        run local-find-this(true).
        UPDATE bnet.memo WITH FRAME f4.
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}                           
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       bnet.BnetCode bnet.BnetValue Bnet.Brand.

       RUN local-find-NEXT.
       IF AVAILABLE bnet THEN memory = recid(bnet).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE bnet THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(bnet).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       bnet.BnetCode bnet.BnetValue bnet.Brand.
       IF ok THEN DO:

           DELETE bnet.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST bnet
           WHERE BNet.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY bnet.BnetCode.

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(bnet).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(bnet) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(bnet) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find bnet WHERE recid(bnet) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find bnet WHERE recid(bnet) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST bnet
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST bnet USE-INDEX BnetName
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST bnet
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST bnet USE-INDEX BnetName
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT bnet
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT bnet USE-INDEX BnetName
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV bnet
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV bnet USE-INDEX BnetName
       WHERE BNet.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Bnet.Brand 
       bnet.BnetCode 
       bnet.BnetName
       bnet.BnetValue

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          bnet.BnetName 
          bnet.BnetValue


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.
