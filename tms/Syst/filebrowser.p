/* ----------------------------------------------------------------------
  MODULE .......: FILEBROWSER.P
  TASK .........: Browse Files in browser
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 16.03.2007
  CHANGED ......: 
                  
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}

DEFINE TEMP-TABLE ttFiles NO-UNDO
   FIELD FileName AS CHARACTER FORMAT "X(78)".

DEFINE INPUT PARAMETER lcDirectory AS CHARACTER NO-UNDO.

DEFINE VARIABLE xrecid       AS RECID           NO-UNDO  INIT ?.
DEFINE VARIABLE FIRSTrow     AS INT             NO-UNDO  INIT 0.
DEFINE VARIABLE FrmRow       AS INT             NO-UNDO  INIT 5.
DEFINE VARIABLE FrmDown      AS INT             NO-UNDO  INIT 15.
DEFINE VARIABLE order        AS INT             NO-UNDO  INIT 1.
DEFINE VARIABLE ufkey        AS LOG             NO-UNDO  INIT TRUE.
DEFINE VARIABLE Memory       AS RECID           NO-UNDO.
DEFINE VARIABLE RowNo        AS INT             NO-UNDO.
DEFINE VARIABLE must-print   AS LOG             NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24 NO-UNDO.
DEFINE VARIABLE i            AS INT             NO-UNDO.
DEFINE VARIABLE lcFile       AS CHARACTER       NO-UNDO.

RUN pFindFirst. 

INPUT THROUGH VALUE("ls " + lcDirectory + " | xargs -n 1 basename").
REPEAT:
   CREATE ttFiles.
   IMPORT UNFORMATTED ttFiles.FileName.
END.

FORM
   ttFiles.FileName
WITH
   ROW 1 TITLE " Files in " + lcDirectory + " " CENTERED OVERLAY FrmDown DOWN COLOR VALUE(cfc)
FRAME sel.

cfc = "sel".

RUN Syst/ufcolor.

ccc = cfc.

VIEW FRAME sel.

RUN pFindFirst.

ASSIGN
   must-print = TRUE
   memory     = RECID(ttFiles).

LOOP:
REPEAT WITH FRAME sel:

   PrintPage:
   DO :
      IF must-print THEN DO:

        UP FRAME-LINE - 1.

        FIND ttFiles WHERE recid(ttFiles) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        REPEAT WITH FRAME sel:

           IF AVAILABLE ttFiles THEN DO:
              RUN pDispRow.
              rtab[FRAME-LINE] = recid(ttFiles).
              RUN pFindNext.
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
      
        ASSIGN
           FIRSTrow   = 0
           must-print = FALSE.

        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */

      END. /* must-print = TRUE */

   END. /* PrintPage */

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
           ufk    = 0
           ufk[8] = 8 
           ufk[9] = 1
           ehto   = 3
           ufkey  = FALSE.
         
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.

      IF order = 1 THEN DO:

        CHOOSE ROW ttFiles.FileName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
      
        COLOR DISPLAY VALUE(ccc) ttFiles.FileName WITH FRAME sel.

      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-LINE] = ? THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN pFindThis.
           RUN pFindPrev.
           IF NOT AVAILABLE ttFiles THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN pDispRow.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ttFiles)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN pFindThis.
           RUN pFindNext.
           IF NOT AVAILABLE ttFiles THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN pDispRow.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(ttFiles).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttFiles WHERE recid(ttFiles) = Memory NO-LOCK NO-ERROR.
        RUN pFindPrev.
        IF AVAILABLE ttFiles THEN DO:
           Memory = recid(ttFiles).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN pFindPrev.
              IF AVAILABLE ttFiles THEN Memory = recid(ttFiles).
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
           FIND ttFiles WHERE recid(ttFiles) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN pFindFirst.
        ASSIGN Memory = recid(ttFiles) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN pFindLast.
        ASSIGN Memory = recid(ttFiles) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,F5,RETURN") > 0 THEN DO:
        RUN pFindThis.
        RETURN ttFiles.FileName.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN 
        RETURN "".
  END.  /* BROWSE */

END.  /* LOOP */

FINALLY:
   HIDE FRAME sel NO-PAUSE.
   si-recid = xrecid.
END.

PROCEDURE pFindThis:

   FIND FIRST ttFiles WHERE
        RECID(ttFiles) = rtab[frame-line(sel)] 
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE pFindFirst:

   FIND FIRST ttFiles NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE pFindLast:

   FIND LAST ttFiles NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE pFindNext:

   FIND NEXT ttFiles NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE pFindPrev:

   FIND PREV ttFiles NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE pDispRow:

   DISPLAY
      ttFiles.FileName
   WITH FRAME sel.

END PROCEDURE.


