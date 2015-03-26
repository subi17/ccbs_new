/* ----------------------------------------------------------------------
  MODULE .......: ServEl1.P
  TASK .........: Elements (single Services) of a Service Package
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 18-06-99
  CHANGED ......: 29.06.99 pt 
                  07.10.99 jp urights added
                  04.09.03 jp brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}

DEF INPUT PARAMETER lcBrand AS C NO-UNDO.
DEF INPUT PARAMETER ServPac LIKE ServPac.ServPac NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ServCom LIKE ServEl.ServCom  NO-UNDO.
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
    ServEl.ServCom      /* COLUMN-LABEL FORMAT */
    ServEl.SeValue   column-label "V"
    ServCom.ScName
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " Components of Service Package '" + ServPac + "' "
    FRAME sel.

form
    ServEl.ServCom     /* LABEL FORMAT */
    ServCom.ScName
    ServCom.ScLocalName 
    ServEl.SeValue
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek ServEl  BY  ServCom */
    ServCom
    HELP "Enter Code of Service"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FIND ServPac WHERE 
     ServPac.Brand   = lcBrand AND 
     ServPac.ServPac = ServPac.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By 2,By 3, By 4".


FIND FIRST ServEl WHERE
ServEl.ServPac = ServPac AND Servel.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE ServEl THEN ASSIGN
   Memory       = recid(ServEl)
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

   IF must-add THEN DO:  /* Add a ServEl  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR ServEl.ServCom WITH FRAME lis EDITING:
              READKEY.
              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 if frame-field = "ServCom" THEN DO:
                    if input ServEl.ServCom = "" THEN LEAVE add-row.
                    FIND ServCom where 
                         ServCom.Brand   = gcBrand AND 
                         ServCom.ServCom = INPUT ServEl.ServCom
                    no-lock no-error.
                    IF NOT AVAIL ServCom THEN DO:
                       bell. message "Unknown Service Component !".
                       NEXT.
                    END.
                    DISP ServCom.ScName ServCom.ScLocalName.
                    FIND ServEl where
                         ServEl.Brand   = gcBrand              AND 
                         ServEl.ServCom = INPUT ServEl.ServCom AND
                         ServEl.ServPac = ServPac AND Servel.Brand = lcBrand
                    NO-LOCK NO-ERROR.
                    IF AVAIL ServEl THEN DO:     
                       BELL. MESSAGE
                       "Service '" + INPUT FRAME lis ServEl.ServCom +
                       "' already exists in package '" + ServPac + "' !".
                       NEXT.
                    END.
                 END.
              END.         
              APPLY LASTKEY.
           END.      


           CREATE ServEl.
           ASSIGN
           ServEl.ServPac = ServPac  
           Servel.Brand   = lcBrand
           ServEl.ServCom = INPUT FRAME lis ServEl.ServCom.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(ServEl)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ServEl WHERE
                 ServEl.ServPac = ServPac AND 
                 Servel.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServEl THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ServEl THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServEl).
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
        ufk[1]= 35  ufk[2]= 0 ufk[3]= 251 ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {uright1.i '"5,6"'}
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ServEl.ServCom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServEl.ServCom WITH FRAME sel.
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
        FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServEl THEN
              ASSIGN FIRSTrow = i Memory = recid(ServEl).
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
           IF NOT AVAILABLE ServEl THEN DO:
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
                rtab[1] = recid(ServEl)
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
           IF NOT AVAILABLE ServEl THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServEl).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServEl THEN DO:
           Memory = recid(ServEl).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServEl THEN Memory = recid(ServEl).
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
           FIND ServEl WHERE recid(ServEl) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET ServCom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ServCom ENTERED THEN DO:
          FIND FIRST ServEl WHERE 
                     ServEl.ServCom >= ServCom AND 
                     ServEl.ServPac = ServPac  AND 
                     Servel.Brand = lcBrand NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ServEl THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ServEl/ServCom was found */
          ASSIGN order = 1 Memory = recid(ServEl) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS:  /* Within ServPac */
       RUN local-find-this(FALSE).                                        
       run servel2(ServEl.ServCom).
       ufkey = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ServEl.ServCom. 

       RUN local-find-NEXT.
       IF AVAILABLE ServEl THEN Memory = recid(ServEl).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServEl THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ServEl).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ServEl.ServCom.
       IF ok THEN DO:

           DELETE ServEl.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServEl
           WHERE ServEl.ServPac = ServPac AND Servel.Brand = lcBrand) THEN DO:
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
       {uright2.i}
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ServEl.ServCom.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ServEl).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ServEl) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ServEl) must-print = TRUE.
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
      FIND ServEl WHERE recid(ServEl) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServEl WHERE recid(ServEl) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ServEl
       WHERE ServEl.ServPac = ServPac AND Servel.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ServEl
       WHERE ServEl.ServPac = ServPac AND Servel.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ServEl
       WHERE ServEl.ServPac = ServPac AND Servel.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ServEl
       WHERE ServEl.ServPac = ServPac AND Servel.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ServEl.ServCom
       ServEl.SeValue
       ServCom.ScName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND ServCom where
            ServCom.ServCom = ServEl.ServCom AND 
            ServCom.Brand   = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP 
        ServCom.ScName  WHEN AVAIL ServCom
        ServCom.ScLocalName WHEN AVAIL ServCom
      WITH FRAME lis.
      UPDATE
      ServEl.SeValue
      WITH FRAME lis EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "SeValue" THEN DO:
                   IF INPUT ServEl.SeValue < ServCom.scvaluerange[1] OR
                      INPUT ServEl.SeValue > ServCom.scvaluerange[2] THEN DO:
                      BELL.
                      message "Value MUST be within range" ServCom.scvaluerange[1]
                      "-" ServCom.scvaluerange[2] "!".
                      NEXT.
                   END.
                END.   
             END.
             APPLY LASTKEY.
          END. /* EDITING */
         LEAVE.
   END.
END PROCEDURE.

