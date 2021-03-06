/* ----------------------------------------------------------------------
  MODULE .......: TOKENCH.P
  TASK .........: update tokens for user group
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 20.02.03
  CHANGED ......: 05.03.03 tk check tokens
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'usergrp'}
{Func/fuserright.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT-OUTPUT PARAMETER pTokenList AS CHAR NO-UNDO.

DEF VAR tokencode      LIKE token.tokencode        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR crundo       AS LOG                    NO-UNDO INIT FALSE.
DEF VAR lSave        AS LOG format "Yes/No"    NO-UNDO.
DEF VAR llAdminUser  AS LOG                    NO-UNDO.

DEF TEMP-TABLE wtoken NO-UNDO
    field tokencode like token.tokencode
    field tokenname like token.tokenname
    field chosen    as   log format "*/"
    FIELD AdminToken AS LOG.

form
    wtoken.tokencode  /* COLUMN-LABEL FORMAT */
    wtoken.tokenname
    wtoken.chosen
WITH ROW FrmRow centered OVERLAY 12 DOWN
    COLOR VALUE(Syst.Var:cfc)
    title COLOR VALUE(Syst.Var:ctc) " CHOOSE TOKENS " 
    FRAME sel.

form /* seek Token by Code */
    tokencode
    help "Enter Token Code"
    WITH row 4 col 2 title COLOR VALUE(Syst.Var:ctc) " FIND CODE "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".


FIND FIRST token
 NO-LOCK NO-ERROR.
IF NOT AVAILABLE token THEN  DO:
   MESSAGE "No tokens available !" view-as alert-box.
   return.
END.

for each token no-lock:
   create wtoken.
   assign
     wtoken.tokencode = token.tokencode
     wtoken.tokenname = token.tokenname
     wtoken.chosen    = true when lookup(token.tokencode,ptokenlist) > 0
     wToken.AdminToken = Token.AdminToken.
end.

llAdminUser = fIsAdminUser(Syst.Var:katun).

FIND FIRST wtoken no-lock.
ASSIGN
   memory       = recid(wtoken)
   must-print   = TRUE
   must-add     = FALSE.


LOOP:
REPEAT WITH FRAME sel:

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND wtoken WHERE recid(wtoken) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE wtoken THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(wtoken).
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
        Syst.Var:ufk[1]= 35 Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0 Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 11 ELSE 0) 
        Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW wtoken.tokencode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) wtoken.tokencode WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND wtoken WHERE recid(wtoken) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE wtoken THEN
              ASSIGN FIRSTrow = i memory = recid(wtoken).
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

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE wtoken THEN DO:
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
                rtab[1] = recid(wtoken)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE wtoken THEN DO:
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
              rtab[FRAME-DOWN] = recid(wtoken).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND wtoken WHERE recid(wtoken) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE wtoken THEN DO:
           memory = recid(wtoken).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE wtoken THEN memory = recid(wtoken).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND wtoken WHERE recid(wtoken) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       tokencode = "".
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE tokencode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF tokencode <> "" THEN DO:
          FIND FIRST wtoken WHERE wtoken.tokencode >= tokencode
           NO-LOCK NO-ERROR.
          IF NOT AVAILABLE wtoken THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some token/vc-code was found */
          ASSIGN order = 1 memory = recid(wtoken) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return,5,f5") > 0 AND lcRight = "RW" THEN DO:
       RUN local-find-this(TRUE).
       IF wToken.AdminToken AND NOT llAdminUser THEN DO:
          MESSAGE "This is an admin level token. Modification not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
       wtoken.chosen = NOT(wtoken.chosen).
       RUN local-disp-row.
       xrecid = recid(wtoken).
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(wtoken) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(wtoken) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN DO:

        IF lcRight = "RW" THEN DO:
           lSave = TRUE.
           MESSAGE "Save changes ? " UPDATE lSave.
        END.
        ELSE lSave = FALSE.
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

IF lSave THEN DO:
   pTokenList = "".

   FOR EACH wtoken WHERE wtoken.chosen = TRUE NO-LOCK:
      pTokenList = pTokenList + "," + wtoken.tokencode.
   END.

   if substr(pTokenList,1,1) = "," then
      pTokenList = substr(pTokenList,2).

END.  

PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND wtoken WHERE recid(wtoken) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND wtoken WHERE recid(wtoken) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   FIND FIRST wtoken NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   FIND LAST wtoken NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   FIND NEXT wtoken NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
   FIND prev wtoken NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   DISPLAY
      wtoken.tokencode
      wtoken.tokenname
      wtoken.chosen
   WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

