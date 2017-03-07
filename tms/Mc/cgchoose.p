/* ----------------------------------------------------------------------
  MODULE .......: CGCHOOSE.p
  TASK .........: select customer groups; returns comma separated list
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 17.03.03
  CHANGED ......: 16.09.03/aam 
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT-OUTPUT PARAMETER pGroupList AS CHAR NO-UNDO.

DEF VAR CustGroup    LIKE CustGroup.CustGroup  NO-UNDO.
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

DEF TEMP-TABLE wCustGroup
    field CustGroup like CustGroup.CustGroup
    field CGName    like CustGroup.CGName
    field chosen     as  log format "*/".


form
    wCustGroup.CustGroup  /* COLUMN-LABEL FORMAT */
    wCustGroup.CGName
    wCustGroup.chosen     label "Ch" 
WITH ROW FrmRow centered OVERLAY 12 DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " CHOOSE TOKENS (" + gcBrand + ") " 
    FRAME sel.

form /* seek Token by Code */
    CustGroup
    help "Enter Token Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".


FIND FIRST token
 NO-LOCK NO-ERROR.
IF NOT AVAILABLE token THEN  DO:
   MESSAGE "No tokens available !" view-as alert-box.
   return.
END.

for each CustGroup no-lock WHERE
         CustGroup.Brand = gcBrand:
   create wCustGroup.
   assign
     wCustGroup.CustGroup = CustGroup.CustGroup
     wCustGroup.CGName    = CustGroup.CGName
     wCustGroup.chosen    = true when 
                            lookup(CustGroup.CustGroup,pGroupList) > 0.
end.

FIND FIRST wCustGroup no-lock.
ASSIGN
   memory       = recid(wCustGroup)
   must-print   = TRUE
   must-add     = FALSE.


LOOP:
REPEAT WITH FRAME sel:

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND wCustGroup WHERE recid(wCustGroup) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE wCustGroup THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(wCustGroup).
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
        ufk[5]= 11  
        ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW wCustGroup.CustGroup {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) wCustGroup.CustGroup WITH FRAME sel.
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
        FIND wCustGroup WHERE recid(wCustGroup) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE wCustGroup THEN
              ASSIGN FIRSTrow = i memory = recid(wCustGroup).
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
           IF NOT AVAILABLE wCustGroup THEN DO:
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
                rtab[1] = recid(wCustGroup)
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
           IF NOT AVAILABLE wCustGroup THEN DO:
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
              rtab[FRAME-DOWN] = recid(wCustGroup).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND wCustGroup WHERE recid(wCustGroup) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE wCustGroup THEN DO:
           memory = recid(wCustGroup).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE wCustGroup THEN memory = recid(wCustGroup).
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
           FIND wCustGroup WHERE recid(wCustGroup) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       tokencode = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE tokencode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF tokencode <> "" THEN DO:
          FIND FIRST wCustGroup WHERE wCustGroup.CustGroup >= tokencode
           NO-LOCK NO-ERROR.
          IF NOT AVAILABLE wCustGroup THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some token/vc-code was found */
          ASSIGN order = 1 memory = recid(wCustGroup) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN DO:
       RUN local-find-this(TRUE).
       wCustGroup.chosen = NOT(wCustGroup.chosen).
       RUN local-disp-row.
       xrecid = recid(wCustGroup).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(wCustGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(wCustGroup) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:

        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

pGroupList = "".

FOR EACH wCustGroup WHERE wCustGroup.chosen = TRUE NO-LOCK:
   pGroupList = pGroupList + "," + wCustGroup.CustGroup.
END.

if substr(pGroupList,1,1) = "," then
   pGroupList = substr(pGroupList,2).


PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND wCustGroup WHERE recid(wCustGroup) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND wCustGroup WHERE recid(wCustGroup) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   FIND FIRST wCustGroup NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   FIND LAST wCustGroup NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   FIND NEXT wCustGroup NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
   FIND prev wCustGroup NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   DISPLAY
      wCustGroup.CustGroup
      wCustGroup.CGName
      wCustGroup.chosen
   WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

