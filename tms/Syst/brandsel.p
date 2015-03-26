/* ----------------------------------------------------------------------
  MODULE .......: brandsel.p
  TASK .........: Brand selection
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.05.02
  CHANGED ......: 1.9.03 jp /TMS+
                  03.10.03/aam use fChgBrand
                  03.03.05 kl return correctly
                  28.01.08 kl run ufkey, not .p

  VERSION ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Brand

{commali.i}

DEF VAR xBrand     like Brand.Brand  NO-UNDO.
DEF VAR xBRName     like Brand.BRName NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 1.
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
    Brand.Brand     column-label "Brand"
    Brand.BRName     column-label "Name"
WITH ROW FrmRow width 80 overlay FrmDown  down
    TITLE " " + " CHOOSE A BRAND "
         + string(pvm,"99-99-99") + " "
    FRAME sel.

{brand.i}

form
    Brand.Brand     /* label format */
    Brand.BRName    /* label format */
WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Brand  by  Brand */
    xBrand
    HELP "Enter Code of Brand"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    NO-labels overlay FRAME f1.

form /* seek Brand  by xBRName */
    xBRName
    HELP "Enter Name of Brand"
    WITH row 4 col 2 TITLE " FIND NAME "
    NO-labels overlay FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".

find TmsUser where  TmsUser.UserCode = katun no-lock no-error.

IF NOT AVAIL TMSUSER THEN DO:
   MESSAGE 
   "Unknown UserCode " katun SKIP
   VIEW-AS ALERT-BOX.
   NEXT.
END.

FIND FIRST Brand WHERE 
    LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR 
    INDEX("*",TmsUser.Brand) > 0 
NO-LOCK NO-ERROR.
IF AVAILABLE Brand THEN ASSIGN
   memory       = recid(Brand)
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
       PUT SCREEN row FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a Brand  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR Brand.Brand
           validate
              (Brand.Brand NOT ENTERED or
              NOT CAN-FIND(Brand using  Brand.Brand),
              "Brand " + string(INPUT Brand.Brand) +
              " already exists !").
           IF INPUT FRAME lis Brand.Brand NOT ENTERED THEN 
           LEAVE add-row.
           create Brand.
           ASSIGN
           Brand.Brand = INPUT FRAME lis Brand.Brand.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(Brand)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST Brand WHERE
          LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
              INDEX("*",TmsUser.Brand) > 0
      NO-LOCK NO-ERROR.

      IF NOT AVAILABLE Brand THEN DO:
        MESSAGE "No brands are available."
        VIEW-AS ALERT-BOX.
        RETURN.
      END.

      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND Brand WHERE recid(Brand) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Brand THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(Brand).
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
        ufk[1]= 35  ufk[2]= 0 /* 30 */ ufk[3]= 0 ufk[4]= 0
        ufk[5]= 11  ufk[6]= 0 /* 4  */ ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row Brand.Brand ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Brand.Brand WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row Brand.BRName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Brand.BRName WITH FRAME sel.
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
        FIND Brand WHERE recid(Brand) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE Brand THEN
              ASSIGN FIRSTrow = i memory = recid(Brand).
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
           IF NOT AVAILABLE Brand THEN DO:
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
                rtab[1] = recid(Brand)
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
           IF NOT AVAILABLE Brand THEN DO:
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
              rtab[FRAME-down] = recid(Brand).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Brand WHERE recid(Brand) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Brand THEN DO:
           memory = recid(Brand).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE Brand THEN memory = recid(Brand).
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
           FIND Brand WHERE recid(Brand) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET xBrand WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF xBrand ENTERED THEN DO:
          FIND FIRST Brand WHERE Brand.Brand >= xBrand AND
              (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
               INDEX("*",TmsUser.Brand) > 0)
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Brand THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some Brand/Brand was found */
          ASSIGN order = 1 memory = recid(Brand) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO:  /* choose */

       RUN local-find-this(false).

       IF NOT AVAILABLE brand THEN DO:
          MESSAGE "Brand data is not available. Connection can not be made."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT.
       END.

       fChgBrand(Brand.Brand,?). 

       LEAVE LOOP. 

     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(Brand) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(Brand) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
        LEAVE LOOP.
     END.

  END.  /* BROWSE */

END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

/* start TMS */
IF LOOKUP(nap,"8,f8") = 0 THEN DO:
   RUN nn_brand.
   RETURN RETURN-VALUE.
END.
ELSE RETURN "LEAVE".

PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find Brand WHERE recid(Brand) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find Brand WHERE recid(Brand) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST Brand WHERE
                             (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
                             INDEX("*",TmsUser.Brand) > 0)
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Brand USE-INDEX BRName WHERE
                        (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
                                INDEX("*",TmsUser.Brand) > 0)

       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST Brand WHERE
                            (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
          INDEX("*",TmsUser.Brand) > 0)
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Brand USE-INDEX BRName WHERE (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
           INDEX("*",TmsUser.Brand) > 0)
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT Brand WHERE  (LOOKUP(Brand.Brand,TMSUser.Brand) >  0 OR
           INDEX("*",TmsUser.Brand) > 0)
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Brand USE-INDEX BRName WHERE                     (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
                     INDEX("*",TmsUser.Brand) > 0)
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV Brand WHERE             (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
           INDEX("*",TmsUser.Brand) > 0)
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV Brand USE-INDEX BRName WHERE            (LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR
           INDEX("*",TmsUser.Brand) > 0)
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Brand.Brand
       Brand.BRName
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
          Brand.BRName
      WITH FRAME lis
      EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
      LEAVE.
   END.
END PROCEDURE.
