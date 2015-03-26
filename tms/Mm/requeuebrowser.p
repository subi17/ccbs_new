/* -----------------------------------------------
  MODULE .......: requeuebrowser.p
  FUNCTION .....: Request queue browser 
  APPLICATION ..: TMS
  AUTHOR .......: JT 
  CREATED ......: 22.08.07
  Version ......: TMS Master
  ------------------------------------------------------ */

{commali.i}

DEF TEMP-TABLE ttMenu 
 FIELD Type       AS INT
 FIELD MenuText   AS CHAR
 INDEX Type Type. 

DEFINE TEMP-TABLE ttStatMenu
 FIELD FrameIndex  AS INTEGER
 FIELD StatVal     AS INTEGER
 FIELD StatusText  AS CHARACTER
 INDEX FrameIndex FrameIndex.

DEF VAR lcCodeName   AS CHAR NO-UNDO.
DEF VAR liCodeValue  AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS ROWID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS ROWID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR llQueRights  AS LOG                    NO-UNDO. /* User rights OK ?  */
DEF VAR icTitle      AS CHAR                   NO-UNDO.
DEF VAR lcRowText    AS CHAR                   NO-UNDO.
DEF VAR lcSpace      AS CHAR                   NO-UNDO.

icTitle = "All Requests".



FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName  = "requem"    AND
         TMSCodes.CodeGroup  = "SubsLMenu" AND
         TMSCodes.CodeValue  = "1": /* 1 = VISIBLE, TESTING */
   
   IF LENGTH(ENTRY(1,TMSCodes.FieldName,",")) > 1 THEN lcSpace = " ".
   ELSE lcSpace = "  ".
   
   lcRowText = ENTRY(1,TMSCodes.FieldName,",") + lcSpace + TMSCodes.CodeName.
   
   CREATE ttMenu.
   ASSIGN ttMenu.MenuText   = "   " + lcRowText + "   "
          ttMenu.Type       = INT(ENTRY(1,TMSCodes.FieldName,","))
          FrmDown           = FrmDown + 1.
             
   
END.

IF FrmDown > 14 THEN FrmDown = 14.

form
    ttMenu.MenuText FORMAT "x(40)"
   
WITH ROW FrmRow OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + icTitle + " " 
    CENTERED
    FRAME sel.

form /* seek  CodeValue */
    "Code value:" liCodeValue
    HELP "Enter code value"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE VALUE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek CodeName */
    "Code Name:" lcCodeName
    HELP "Enter code name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE NAME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

IF icTitle = "" THEN icTitle = "Requests".

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

/* column-labels for parameters */

ASSIGN ttMenu.MenuText:LABEL IN FRAME sel = "All subscriber queues".


DEFINE VARIABLE lcRight AS CHAR INIT "R".

RUN local-find-first.
llQueRights = FALSE.
       
IF AVAILABLE ttMenu THEN ASSIGN
   Memory       = ROWID(ttMenu)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

def var lite AS INTEGER NO-UNDO.

LOOP:
REPEAT WITH FRAME sel:
                           
   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :
   
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttMenu WHERE ROWID(ttMenu) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose ROWID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttMenu THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = ROWID(ttMenu).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        
        UP FRAME-LINE - 1.
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
           ufk    = 0
           ufk[1] = 0
           ufk[2] = 0
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.
      
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttMenu.Menutext ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttMenu.MenuText WITH FRAME sel.
      END.
      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttMenu THEN DO:
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
                rtab[1] = ROWID(ttMenu)
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
           IF NOT AVAILABLE ttMenu THEN DO:
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
              rtab[FRAME-DOWN] = ROWID(ttMenu).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.        
        END.
        ELSE DO:
           DOWN 1.
        END.
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttMenu WHERE ROWID(ttMenu) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttMenu THEN DO:
           Memory = ROWID(ttMenu).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttMenu THEN Memory = ROWID(ttMenu).
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
           FIND ttMenu WHERE ROWID(ttMenu) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO:
        RUN local-find-this(FALSE).

        IF AVAILABLE ttMenu THEN DO:
           
           RUN reqstatmenu.p(ttMenu.Type). 
           ufkey = TRUE.
           NEXT LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttMenu) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttMenu) must-print = TRUE.
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
      FIND ttMenu WHERE ROWID(ttMenu) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttMenu WHERE ROWID(ttMenu) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF lcRight = "R" THEN DO:
      FIND FIRST ttMenu NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF lcRight = "R" THEN DO:
      FIND LAST ttMenu NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF lcRight = "R" THEN DO:
     FIND NEXT ttMenu NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF lcRight = "R" THEN DO:
      FIND PREV ttMenu NO-LOCK NO-ERROR.
   END.                  

END PROCEDURE.

PROCEDURE local-disp-row:

    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       
       ttMenu.MenuText
    WITH FRAME sel.

END PROCEDURE.



