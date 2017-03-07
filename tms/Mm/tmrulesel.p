/* ----------------------------------------------------------------------
  MODULE .......: TMRule.p
  TASK .........: TMRule selection for tmcounter view.
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 05/2008
  CHANGED ......: 
  Version ......: xfera 
  ---------------------------------------------------------------------- */

{Syst/commali.i}.

DEFINE INPUT PARAM piMsSeq     AS INT NO-UNDO. 
DEFINE INPUT PARAM piCustNum   AS INT NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 4.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 11.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE orders       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE ufkey        AS LOGICAL                 NO-UNDO  init TRUE.
DEFINE VARIABLE delrow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE pr-order     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE Memory       AS RECID                   NO-UNDO.
DEFINE VARIABLE RowNo        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE must-print   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE must-add     AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE ac-hdr       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24         NO-UNDO.
DEFINE VARIABLE i            AS INTEGER                 NO-UNDO.
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.

FORM
    TMRule.Name COLUMN-LABEL "Ticket management rules"
WITH ROW FrmRow CENTER width 42 OVERLAY FrmDown DOWN SCROLL 1
    COLOR VALUE(cfc)   
    FRAME sel.

orders = "TMC".

RUN local-find-first.
IF AVAILABLE TMRule THEN ASSIGN
   Memory       = recid(TMRule)
   must-print   = TRUE 
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No TMRule available" VIEW-AS ALERT-BOX.
   RETURN.
END.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

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
        FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMRule THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMRule).
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
        ufk = 0
        ufk[1]= 968  
        ufk[2]= 1521 WHEN piCustnum > 0  
        ufk[3]= 0
        ufk[5]= 0
        ufk[6]= 0 
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TMRule.Name {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMRule.Name WITH FRAME sel.
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
        FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TMRule THEN
              ASSIGN FIRSTrow = i Memory = recid(TMRule).
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
      
      nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE TMRule THEN DO:
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
                rtab[1] = recid(TMRule)
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
           IF NOT AVAILABLE TMRule THEN DO:
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
              rtab[FRAME-DOWN] = recid(TMRule).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMRule THEN DO:
           Memory = recid(TMRule).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TMRule THEN Memory = recid(TMRule).
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
           FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 THEN DO:
        RUN local-find-this(false).
        HIDE FRAME sel no-pause.
        VIEW FRAME sel.
        RUN Mm/tmlimit.p(TMRule.TMRuleSeq,piMsSeq,piCustNum,1).
        ufkey = true.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO:
        RUN local-find-this(false).
        HIDE FRAME sel no-pause.
        RUN Mm/tmcounter.p(TMRule.TMRuleSeq,piMsSeq,piCustNum).
        VIEW FRAME sel.
        ufkey = true.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TMRule) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TMRule) must-print = TRUE.
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
      FIND TMRule WHERE recid(TMRule) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TMRule WHERE recid(TMRule) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   FIND FIRST TMRule WHERE
         TMRule.Brand     = gcBrand AND
         TMRule.FromDate <= TODAY AND
         TMRule.ToDate   >= TODAY
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
   
   FIND LAST TMRule WHERE
         TMRule.Brand = gcBrand AND
         TMRule.FromDate <= TODAY AND
         TMRule.ToDate   >= TODAY
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   
   FIND NEXT TMRule WHERE
         TMRule.Brand = gcBrand AND
         TMRule.FromDate <= TODAY AND
         TMRule.ToDate   >= TODAY
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   FIND PREV TMRule WHERE
         TMRule.Brand = gcBrand AND
         TMRule.FromDate <= TODAY AND
         TMRule.ToDate   >= TODAY
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      TMRule.Name
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.
