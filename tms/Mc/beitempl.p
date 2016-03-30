/* -------------------------------------------------------------------------
  MODULE .......: BEITEMPL.P
  TASK .........: SHOW Billing Event Items of one Price event/one Price list
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-09-99
  CHANGED ......: 04-10-99 jp urights added
                  15.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT PARAMETER FeeModel LIKE FeeModel.FeeModel NO-UNDO.
DEF INPUT PARAMETER PriceList LIKE PriceList.PriceList  NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR BillCode LIKE FMItem.BillCode NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
    FMItem.BillCode     /* COLUMN-LABEL FORMAT */
    BillItem.BIName    format "x(24)"
    FMItem.BillMethod
    FMItem.Interval
    FMItem.Amount

WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " B-Event " + FeeModel + ": " + FeeModel.FeeName + " / Price List " + PriceList
    + " "
    FRAME sel.

form
    FMItem.BillCode  
    BillItem.BIName
    FMItem.BillMethod
    FMItem.Interval
    FMItem.Amount
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.


form /* seek Billing Event Item  BY BillCode */
    BillCode
    HELP "Enter BillCode Code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND BillCode "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FIND FIRST FeeModel WHERE 
           FeeModel.Brand    = gcBrand AND
           FeeModel.FeeModel = FeeModel NO-LOCK.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Price List,By BillCode  ,By 3, By 4".

RUN local-find-first.

IF AVAILABLE FMItem THEN ASSIGN
   memory       = recid(FMItem)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:

   MESSAGE "No FeeModel items available."
   VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a FMItem  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR 
              FMItem.PriceList
              FMItem.BillCode
           EDITING:
              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 IF FRAME-FIELD = "pl-code" THEN DO:
                    IF INPUT FMItem.PriceList = "" THEN 
                    UNDO add-row, LEAVE add-row.

                    FIND PriceList WHERE 
                         PriceList.Brand     = gcBrand AND
                         PriceList.PriceList = INPUT FMItem.PriceList
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL PriceList THEN DO:
                       BELL.
                       MESSAGE "Unknown Price List !".
                       NEXT.
                    END.
                    DISP PriceList.PLName.
                 END.   

                 ELSE IF FRAME-FIELD = "tu-nro" THEN DO:
                    IF INPUT FMItem.BillCode = "" THEN DO:
                       NEXT-PROMPT FMItem.PriceList.
                       NEXT.
                    END.
                    FIND BillItem WHERE 
                         BillItem.Brand    = gcBrand AND
                         BillItem.BillCode = INPUT FMItem.BillCode                                 NO-LOCK NO-ERROR.
                    IF NOT AVAIL BillItem THEN DO:
                       BELL.
                       MESSAGE "Unknown BillCode !".
                       NEXT.
                    END.
                 END.   
              END.
              APPLY LASTKEY.
           END.   

           IF CAN-FIND(FIRST FMItem WHERE
                       FMITem.Brand     = gcBrand AND
                       FMItem.FeeModel  = FeeModel        AND
                       FMItem.PriceList = PriceList.PriceList  AND
                       FMItem.BillCode  = BillItem.BillCode)
           THEN DO:
              BELL.
              MESSAGE
              "There is already an item with"    SKIP
              "Price List" PriceList.PriceList "AND"   SKIP
              "product code" BillItem.BillCode "!"    
              VIEW-AS ALERT-BOX ERROR.
              UNDO add-row, NEXT add-row.                       
           END.

           CREATE FMItem.
           ASSIGN
           FMItem.Brand     = gcBrand 
           FMItem.FeeModel  = FeeModel
           FMItem.PriceList = INPUT FRAME lis FMItem.PriceList
           FMItem.BillCode  = INPUT FRAME lis FMItem.BillCode.
           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(FMItem)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */

      RUN local-find-first.
      IF NOT AVAILABLE FMItem THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FMItem WHERE recid(FMItem) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FMItem THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FMItem).
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
        ufk[1]= 703  ufk[2]= 0   ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"5,6"'}.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FMItem.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FMItem.BillCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW FMItem.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FMItem.BillCode WITH FRAME sel.
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
        FIND FMItem WHERE recid(FMItem) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FMItem THEN
              ASSIGN FIRSTrow = i memory = recid(FMItem).
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
           IF NOT AVAILABLE FMItem THEN DO:
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
                rtab[1] = recid(FMItem)
                memory  = rtab[1].
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
           IF NOT AVAILABLE FMItem THEN DO:
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
              rtab[FRAME-DOWN] = recid(FMItem).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND FMItem WHERE recid(FMItem) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FMItem THEN DO:
           memory = recid(FMItem).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FMItem THEN memory = recid(FMItem).
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
           memory = rtab[FRAME-DOWN].
           FIND FMItem WHERE recid(FMItem) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY col 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F1.
       SET BillCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF BillCode ENTERED THEN DO:
          FIND FIRST FMItem WHERE 
                     FMItem.Brand     = gcBrand   AND
                     FMItem.PriceList = PriceList AND 
                     FMItem.BillCode >= BillCode  AND
                     FMItem.FeeModel = FeeModel.FeeModel  
          USE-INDEX BillCode 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FMItem THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some beitem/tu-nro was found */
          ASSIGN order = 1 memory = recid(FMItem) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(FMItem) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(FMItem) must-print = TRUE.
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
      FIND FMItem WHERE recid(FMItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FMItem WHERE recid(FMItem) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST FMItem WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel  = FeeModel.FeeModel NO-LOCK NO-ERROR.

       ELSE IF order = 2 THEN FIND FIRST FMItem USE-INDEX BillCode WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel = FeeModel.FeeModel NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST FMItem WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel  = FeeModel.FeeModel NO-LOCK NO-ERROR.

       ELSE IF order = 2 THEN FIND LAST FMItem USE-INDEX BillCode WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel = FeeModel.FeeModel NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT FMItem WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel  = FeeModel.FeeModel NO-LOCK NO-ERROR.

       ELSE IF order = 2 THEN FIND NEXT FMItem USE-INDEX BillCode WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel = FeeModel.FeeModel NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV FMItem WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel  = FeeModel.FeeModel NO-LOCK NO-ERROR.

       ELSE IF order = 2 THEN FIND PREV FMItem USE-INDEX BillCode WHERE 
          FMItem.Brand     = gcBrand   AND
          FMItem.PriceList = PriceList AND 
          FMItem.FeeModel = FeeModel.FeeModel NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       FMItem.BillCode
       BillItem.BIName      WHEN AVAIL BillItem
       FMItem.BillMethod
       FMItem.Interval
       FMItem.Amount
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

       FIND BillItem WHERE 
            BillItem.Brand     = gcBrand AND
            BillItem.BillCode  = FMItem.BillCode  NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP

      PriceList.PLName   WHEN AVAIL PriceList
      BillItem.BIName   WHEN AVAIL BillItem
      WITH FRAME lis.

      UPDATE
          FMItem.BillMethod
          FMItem.Interval
          FMItem.Amount


      WITH FRAME lis EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "bi-intv" THEN DO:
                   IF INPUT FRAME lis FMItem.BillMethod /* single */ THEN DO:
                      IF INPUT FRAME lis FMItem.Interval NE 0 THEN DO:
                         BELL.
                         MESSAGE 
                         "This is a SINGLE fee - no Interval allowed !".
                         NEXT.
                      END.   
                   END.
                   ELSE DO:  /* periodical */
                      IF INPUT FRAME lis FMItem.Interval = 0 THEN DO:
                         BELL.
                         MESSAGE 
                         "This is a PERIODICAL fee - Interval must be given !".
                         NEXT.
                      END.   

                   END.
                END.

                ELSE IF  FRAME-FIELD = "bi-itype" AND
                INPUT FRAME lis FMItem.BillMethod = TRUE THEN
                DISP 0 @ FMItem.Interval.


             END.
             APPLY LASTKEY.
          END. /* EDITING */

      LEAVE.
   END.
END PROCEDURE.

