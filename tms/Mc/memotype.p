/* ----------------------------------------------------------------------
  MODULE .......: memotype.p
  TASK .........: Update TMSCodes
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 07.04.03
  CHANGED ......: 14.07.04 tk f4 removed
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER   iiCustNum AS INT NO-UNDO.

DEF VAR TMSCodes     LIKE TMSCodes.Tablename        NO-UNDO.
DEF VAR CodeGroup    LIKE TMSCodes.CodeGroup        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
   TMSCodes.CodeValue    FORMAT "X(30)" Column-label "Type of memo" 
   TMSCodes.CodeName FORMAT "x(40)"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  BILLING ITEMS MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    TMSCodes.CodeValue    /* LABEL FORMAT */
            /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  TMSCodes */
    TMSCodes
    HELP "Enter Code of Billing RepType "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  CodeValue */
    CodeGroup
    HELP "Enter Name of the Billing RepType"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form
    TMSCodes.Memo

    WITH OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Memo: " + TMSCodes.CodeValue + " " WITH NO-LABELS 1 columns
    FRAME f4.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST TMSCodes
WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
IF AVAILABLE TMSCodes THEN ASSIGN
   Memory       = recid(TMSCodes)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.
PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TMSCodes WHERE recid(TMSCodes) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMSCodes THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMSCodes).
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
        ufk[1]= 35  ufk[2]= 30 
        ufk[3]= 2353
        ufk[4]= 0 /*927*/
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TMSCodes.CodeValue {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMSCodes.CodeValue WITH FRAME sel.
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
        FIND TMSCodes WHERE recid(TMSCodes) = Memory NO-LOCK NO-ERROR.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TMSCodes THEN
              ASSIGN FIRSTrow = i Memory = recid(TMSCodes).
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
           IF NOT AVAILABLE TMSCodes THEN DO:
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
                rtab[1] = recid(TMSCodes)
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
           IF NOT AVAILABLE TMSCodes THEN DO:
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
              rtab[FRAME-DOWN] = recid(TMSCodes).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TMSCodes WHERE recid(TMSCodes) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMSCodes THEN DO:
           Memory = recid(TMSCodes).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TMSCodes THEN Memory = recid(TMSCodes).
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
           FIND TMSCodes WHERE recid(TMSCodes) = Memory NO-LOCK NO-ERROR.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET TMSCodes WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF TMSCodes ENTERED THEN DO:
          FIND FIRST TMSCodes WHERE 
                     TMSCodes.CodeValue >= TMSCodes AND 
                     TmsCodes.CodeGroup = "MEmoType" 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TMSCodes THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some TMSCodes/TMSCodes was found */
          ASSIGN order = 1 Memory = recid(TMSCodes) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET CodeGroup WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CodeGroup ENTERED THEN DO:
          FIND FIRST TMSCodes USE-INDEX codegroup WHERE 
                     TMSCodes.CodeGroup >= CodeGroup AND 
                     TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
          IF NOT AVAILABLE TMSCodes THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some TMSCodes/CodeValue was found */
          ASSIGN order = 2 Memory = recid(TMSCodes) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.p.
        ehto = 9. 
        RUN Syst/ufkey.p. ufkey = TRUE.
        RUN local-find-this(FALSE).
        RUN Mc/memob.p(input iiCustNum, TMSCodes.codevalue).
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       IF LOCKED TmsCodes THEN DO:
          MESSAGE "TmsCodes record is locked by some other user." VIEW-AS ALERT-BOX.
          RUN local-find-this(false).
          LEAVE.          
       END. /* IF NOT AVAILABLE TmsCodes THEN DO: */

       IF NOT AVAILABLE TmsCodes THEN DO:
          MESSAGE "TmsCodes record is not available." VIEW-AS ALERT-BOX.
          LEAVE.
       END. /* IF NOT AVAILABLE TmsCodes THEN DO: */

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY TMSCodes.CodeValue.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(TMSCodes).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TMSCodes) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TMSCodes) must-print = TRUE.
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
      FIND TMSCodes WHERE recid(TMSCodes) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    ELSE
       FIND TMSCodes WHERE recid(TMSCodes) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST TMSCodes
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST TMSCodes USE-INDEX CodeGroup
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST TMSCodes
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST TMSCodes USE-INDEX CodeGroup
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT TMSCodes
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT TMSCodes USE-INDEX CodeGroup
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV TMSCodes
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV TMSCodes USE-INDEX CodeGroup
       WHERE TmsCodes.CodeGroup = "MEmoType" NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       TMSCodes.CodeValue 
       TMSCodes.CodeName 
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          TMSCodes.CodeValue
          TMSCodes.CodeName           
      WITH FRAME lis.

      UPDATE

         TMSCodes.CodeName          
      WITH FRAME lis. 
      LEAVE.
   END.
END PROCEDURE.

