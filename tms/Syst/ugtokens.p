/* ----------------------------------------------------------------------
  MODULE .......: UGTOKENS.P
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

DEF INPUT PARAMETER icUserGroup AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER pShoTokens AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER pModTokens AS CHAR NO-UNDO.

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
DEF VAR cright       AS CHAR                   NO-UNDO.
DEF VAR llAdminUser  AS LOG                    NO-UNDO.

DEF TEMP-TABLE wtoken
    field tokencode like token.tokencode
    field tokenname like token.tokenname
    FIELD AdminToken AS LOG 
    field right     as   int.

form
    wtoken.tokencode  /* COLUMN-LABEL FORMAT */
    wtoken.tokenname
    cright            label "Right"
WITH ROW FrmRow centered OVERLAY 12 DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " CHOOSE TOKENS, GROUP " + icUserGroup + " " 
    FRAME sel.

form /* seek Token by Code */
    tokencode
    help "Enter Token Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".

llAdminUser = fIsAdminUser(katun).

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
     wToken.AdminToken = Token.AdminToken
     wtoken.right     = 1 when lookup(token.tokencode,pShoTokens) > 0
     wtoken.right     = 2 when lookup(token.tokencode,pModTokens) > 0.
end.

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
        ufk[1]= 35 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 11 ELSE 0) 
        ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW wtoken.tokencode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) wtoken.tokencode WITH FRAME sel.
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

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
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
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
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
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
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
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
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
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       tokencode = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
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

     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 AND lcRight = "RW" THEN DO:
       RUN local-find-this(TRUE).
       
       IF wToken.AdminToken AND NOT llAdminUser THEN DO:
          MESSAGE "This is an admin level token, modification not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
       
       wtoken.right = wtoken.right + 1.
       
       IF wToken.Right = 2 AND wToken.AdminToken THEN DO:
          FOR EACH TMSUser NO-LOCK WHERE
                   TMSUser.UserGroup = icUserGroup:
             IF NOT TMSUser.AdminUser THEN DO:
                MESSAGE "Group contains users that are not admin"
                        "users. MODIFY right can not be assigned."
                VIEW-AS ALERT-BOX ERROR.
                wToken.Right = 0.
                LEAVE.
             END.
          END.
       END.
       
       if wtoken.right = 3 then wtoken.right = 0.
       RUN local-disp-row.
       xrecid = recid(wtoken).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(wtoken) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(wtoken) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:

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
si-recid = xrecid.

IF lSave THEN DO:
   ASSIGN
      pShoTokens = ""
      pModTokens = "".

   FOR EACH wtoken WHERE wtoken.right > 0 NO-LOCK:
      pShoTokens = pShoTokens + "," + wtoken.tokencode.
   END.

   FOR EACH wtoken WHERE wtoken.right = 2 NO-LOCK:
      pModTokens = pModTokens + "," + wtoken.tokencode.
   END.

   if substr(pShoTokens,1,1) = "," then
      pShoTokens = substr(pShoTokens,2).
   if substr(pModTokens,1,1) = "," then
      pModTokens = substr(pModTokens,2).


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
      "NONE"   when wtoken.right = 0 @ cright
      "SHOW"   when wtoken.right = 1 @ cright
      "MODIFY" when wtoken.right = 2 @ cright 
   WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

