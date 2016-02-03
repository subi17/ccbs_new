/* ----------------------------------------------------------------------
  MODULE .......: EPMember2.p
  TASK .........: UPDATE Product's memberships in one Ext BillCode Group
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-09-99
  CHANGED ......: 04-10-99 jp urights added
                  16.09.03 jp Brand 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 

DEF INPUT PARAMETER BillCode LIKE BillItem.BillCode NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR EpGroup LIKE EPMember.EpGroup NO-UNDO.

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
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

form
    EPMember.EpGroup    /* COLUMN-LABEL FORMAT */
    EPGroup.EpName    
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Memerships of BillCode. " + BillCode + ": " + BillItem.BIName + " "
    FRAME sel.

form
    EPMember.EpGroup 
    EPGroup.EpName

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Billing Event Item  BY BillCode */
    EPGroup
    HELP "Enter Group Code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND GROUP "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FIND BillItem WHERE 
     BillItem.Brand    = gcBrand   AND 
     BillItem.BillCode = BillCode NO-LOCK.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Price List,By BillCode  ,By 3, By 4".


FIND FIRST EPMember WHERE 
           EPMember.BillCode = BillItem.BillCode AND EPMember.Brand = gcBrand 
NO-LOCK NO-ERROR.
IF AVAILABLE EPMember THEN ASSIGN
   Memory       = recid(EPMember)
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a EPMember  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR EPMember.EpGroup
           EDITING:
              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 IF FRAME-FIELD = "EpGroup" THEN DO:
                    IF INPUT FRAME lis EPMember.EpGroup = "" 
                    THEN UNDO add-row, LEAVE.

                    FIND EPGroup WHERE 
                         EPGroup.EpGroup = INPUT EPMember.EpGroup       
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL EPGroup THEN DO:
                       BELL.
                       MESSAGE "Unknown Group !".
                       NEXT.
                    END.
                    DISP EPGroup.EpName.
                 END.   
              END.
              APPLY LASTKEY.
           END.   

           IF CAN-FIND(EPMember WHERE
                       EPMember.EpGroup = EpGroup        AND
                       EPMember.BillCode  = BillItem.BillCode)
           THEN DO:
              BELL.
              MESSAGE
              "This BillCode is already a member" SKIP
              "in this External BillCode Group !" SKIP
              VIEW-AS ALERT-BOX ERROR
              TITLE " BillCode EXISTS ALREADY ".
              UNDO add-row, NEXT add-row.                       
           END.

           CREATE EPMember.
           ASSIGN
           EPMember.Brand     = gcBrand 
           EPMember.BillCode  = BillCode
           EPMember.EpGroup   = INPUT FRAME lis EPMember.EpGroup.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(EPMember)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST EPMember
      WHERE EPMember.BillCode = BillItem.BillCode AND 
            EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE EPMember THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND EPMember WHERE recid(EPMember) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE EPMember THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(EPMember).
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
        ufk[1]= 973  ufk[2]= 0 ufk[3]= 1131 ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"5,6"'}.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.

      IF order = 1 THEN DO:
        CHOOSE ROW EPMember.EpGroup ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) EPMember.EpGroup WITH FRAME sel.
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
        FIND EPMember WHERE recid(EPMember) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE EPMember THEN
              ASSIGN FIRSTrow = i Memory = recid(EPMember).
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
           IF NOT AVAILABLE EPMember THEN DO:
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
                rtab[1] = recid(EPMember)
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
           IF NOT AVAILABLE EPMember THEN DO:
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
              rtab[FRAME-DOWN] = recid(EPMember).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND EPMember WHERE recid(EPMember) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE EPMember THEN DO:
           Memory = recid(EPMember).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE EPMember THEN Memory = recid(EPMember).
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
           FIND EPMember WHERE recid(EPMember) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY col 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F1.
       SET EpGroup WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF EpGroup ENTERED THEN DO:
          FIND FIRST EPMember WHERE 
                     EPMember.BillCode   = BillCode AND
                     EPMember.EpGroup >= EpGroup  
          USE-INDEX BillCode 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE EPMember THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some EPMember/BillItem was found */
          ASSIGN order = 1 Memory = recid(EPMember) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Group contains */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        /* get CURRENT record WITH NO-LOCK status */
        RUN local-find-this(FALSE).

        run epmember1 (INPUT EPMember.EpGroup).
        ufkey = TRUE.
        NEXT loop.
     END.






     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}.
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       EPMember.EpGroup EPGroup.EpName.

       RUN local-find-NEXT.
       IF AVAILABLE EPMember THEN Memory = recid(EPMember).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE EPMember THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(EPMember).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       EPMember.EpGroup EPGroup.EpName.
       IF ok THEN DO:

           DELETE EPMember.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST EPMember
           WHERE EPMember.BillCode = BillCode) THEN DO:
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
       {Syst/uright2.i}
       /* change */

MESSAGE
"You can not CHANGE anything here - just " SKIP
"add a new record or"                      SKIP
"delete an existing one"
VIEW-AS ALERT-BOX INFORMATION.



       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY
          EPMember.EpGroup.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(EPMember).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(EPMember) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(EPMember) must-print = TRUE.
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
      FIND EPMember WHERE recid(EPMember) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND EPMember WHERE recid(EPMember) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND 
             EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND 
             EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND 
             EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND 
             EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND 
             EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND 
             EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV EPMember USE-INDEX BillCode
       WHERE EPMember.BillCode = BillItem.BillCode AND EPMember.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       EPMember.EpGroup
       EPGroup.EpName      WHEN AVAIL EPGroup
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND EPGroup WHERE EPGroup.EpGroup = EPMember.EpGroup NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      EPGroup.EpName  WHEN AVAIL BillItem
      WITH FRAME lis.
      UPDATE

      WITH FRAME lis EDITING:
             READKEY.
             APPLY LASTKEY.
          END. /* EDITING */

      LEAVE.
   END.
END PROCEDURE.

