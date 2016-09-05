/* ----------------------------------------------------------------------
  MODULE .......: viewtable.p
  TASK .........: Show fields and values for any given table
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 11.02.13
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commali.i}        
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}
{Syst/eventval.i}

DEFINE INPUT PARAMETER ihTable  AS HANDLE NO-UNDO. 

IF ihTable EQ ? THEN RETURN.

DEF TEMP-TABLE ttTableField NO-UNDO
   FIELD ttName  AS  CHAR FORMAT "X(25)"
   FIELD ttValue AS  CHAR  FORMAT "X(50)" 
INDEX ttName IS PRIMARY ttName 
INDEX ttValue ttvalue.    

DEFINE VARIABLE lhField       AS HANDLE    NO-UNDO.
DEFINE VARIABLE liFields      AS INT       NO-UNDO.

DO liFields = 1 TO ihTable:NUM-FIELDS:

  lhField = ihTable:BUFFER-FIELD(liFields).  

  CREATE ttTableField.
  ASSIGN
     ttTableField.ttName  = lhfield:name
     ttTableField.ttValue =
        (IF lhfield:format = "99999999.99999" THEN
         STRING(lhfield:buffer-value,"99999999.99999")
      ELSE lhfield:buffer-value).

END.

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF VAR ttTableField      LIKE ttName        NO-UNDO.
DEF VAR ttValue      LIKE ttTableField.ttValue        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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
    ttName                 COLUMN-LABEL "NAME"
    ttTableField.ttValue   COLUMN-LABEL "VALUE"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  "  + ihTable:NAME + " DETAIL  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ttName               LABEL  "NAME:"
    ttTableField.ttValue LABEL  "VALUE" FORMAT "X(60)" 

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  ttTableField */
    ttTableField
    HELP "Enter Name Of the Column "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  ttValue */
    ttValue
    HELP "Enter Value "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND VALUE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.


FIND FIRST ttTableField
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE ttTableField THEN ASSIGN
   Memory       = recid(ttTableField)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a ttTableField  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttTableField WHERE recid(ttTableField) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttTableField THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttTableField).
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
        ufk[1]= 30  ufk[2]= 9064 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttName WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ttTableField.ttValue {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttTableField.ttValue WITH FRAME sel.
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
        FIND ttTableField WHERE recid(ttTableField) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttTableField THEN
              ASSIGN FIRSTrow = i Memory = recid(ttTableField).
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
           IF NOT AVAILABLE ttTableField THEN DO:
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
                rtab[1] = recid(ttTableField)
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
           IF NOT AVAILABLE ttTableField THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttTableField).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttTableField WHERE recid(ttTableField) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttTableField THEN DO:
           Memory = recid(ttTableField).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttTableField THEN Memory = recid(ttTableField).
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
           FIND ttTableField WHERE recid(ttTableField) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET ttTableField WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ttTableField ENTERED THEN DO:
          FIND FIRST ttTableField WHERE ttName >= ttTableField
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttTableField THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttTableField/ttTableField was found */
          ASSIGN order = 1 Memory = recid(ttTableField) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET ttValue WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ttValue ENTERED THEN DO:
          FIND FIRST ttTableField WHERE ttTableField.ttValue >= ttValue
          USE-INDEX ttValue /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttTableField THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttTableField/ttValue was found */
          ASSIGN order = 2 Memory = recid(ttTableField) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */


     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).


       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttName.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.


       RUN local-disp-row.
       xrecid = recid(ttTableField).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttTableField) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttTableField) must-print = TRUE.
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
      FIND ttTableField WHERE recid(ttTableField) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttTableField WHERE recid(ttTableField) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttTableField
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ttTableField USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttTableField
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ttTableField USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttTableField
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ttTableField USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttTableField
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ttTableField USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttTableField.ttName 
       ttTableField.ttValue

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          ttTableField.ttValue
          ttTableField.ttValue
      WITH FRAME lis.

      MESSAGE "PRESS ENTER TO CONTINUE". PAUSE   NO-MESSAGE.
      LEAVE.
   END.
END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttTableField.
END.
