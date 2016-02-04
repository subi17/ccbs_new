/* -----------------------------------------------
  MODULE .......: maintmenu.p
  FUNCTION .....: Request queue browser 
  APPLICATION ..: TMS
  AUTHOR .......: JT 
  CREATED ......: 22.08.07
  Version ......: TMS Master
  ------------------------------------------------------ */

{Syst/commali.i}


DEFINE TEMP-TABLE ttMaintAction
 FIELD FrameIndex  AS INTEGER
 FIELD Action      AS CHARACTER
 FIELD Module      AS CHARACTER
 INDEX FrameIndex FrameIndex.

DEFINE TEMP-TABLE ttMaintMenu
 FIELD MaintType AS CHAR
 FIELD FramePos  AS CHAR.


DEF VAR lcCodeName   AS CHAR NO-UNDO.
DEF VAR liCodeValue  AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR icCoName2    AS CHAR                   NO-UNDO.
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
DEF VAR lcTemp       AS CHAR                   NO-UNDO.
DEF VAR liBarr       AS INT                    NO-UNDO. 
DEF VAR icTitle      AS CHAR                   NO-UNDO.
DEF VAR lcRowText    AS CHAR                   NO-UNDO.
DEF VAR lcSpace      AS CHAR                   NO-UNDO.
DEF VAR lcLine       AS CHAR                   NO-UNDO.
DEF VAR lcDesc       AS CHAR EXTENT 4          NO-UNDO.
DEF VAR lcExtMsg     AS CHAR                   NO-UNDO.


DEFINE TEMP-TABLE ttMenuItem
FIELD MaintType   AS CHAR
FIELD FramePos    AS CHAR /* Left, TopRight */
FIELD Action      AS CHAR
FIELD Descr       AS CHAR
FIELD Module      AS CHAR
INDEX Orderr MaintType Action.

DEF VAR menuc AS CHAR EXTENT 9 NO-UNDO.

FORM

   WITH ROW 1 17 DOWN WIDTH 80
   COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) " " + "ADMIN LEVEL MAINTENANCE MENU" + " "
   CENTERED

FRAME frMain.

FORM
    menuc[1] FORMAT "x(56)" SKIP
    menuc[2] FORMAT "x(56)" SKIP
    menuc[3] FORMAT "x(56)" SKIP
    menuc[4] FORMAT "x(56)" SKIP
    menuc[5] FORMAT "x(56)" SKIP
    menuc[6] FORMAT "x(56)" SKIP
    menuc[7] FORMAT "x(56)" SKIP
    menuc[8] FORMAT "x(56)" SKIP
    menuc[9] FORMAT "x(56)" SKIP
    WITH ROW 2 COL 22 OVERLAY 
    COLOR VALUE(ccc)
    TITLE "   MAINTENANCE ACTION   "
    NO-LABEL
FRAME frTop.


FORM
    lcDesc[1] FORMAT "x(56)" SKIP
    lcDesc[2] FORMAT "x(56)" SKIP
    lcDesc[3] FORMAT "x(56)" SKIP
    lcDesc[4] FORMAT "x(56)" SKIP

    WITH ROW 13 COL 22 OVERLAY 
    COLOR VALUE(cfc)
    TITLE "      INFO      "  
    NO-LABEL
FRAME frDesc.

FORM

    ttMaintMenu.MaintType FORMAT "x(12)"
    WITH ROW 2 OVERLAY 15 DOWN COL 2 WIDTH 20
    TITLE COLOR VALUE(ctc) " " + "MAINTENANCE TYPE" + " "
    COLOR VALUE(cfc)
    NO-LABEL

FRAME frLeft.


VIEW FRAME frMain. PAUSE 0. 
VIEW FRAME frTop.  PAUSE 0.
VIEW FRAME frDesc. PAUSE 0.
VIEW FRAME frLeft. PAUSE 0.

FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.CodeGroup = "UI_Maint":
   CREATE ttMenuItem.
   ASSIGN ttMenuItem.MaintType = TMSCodes.TableName
          ttMenuItem.FramePos  = TMSCodes.CodeValue
          ttMenuItem.Descr     = TMSCodes.ConfigValue
          ttMenuItem.Action    = TMSCodes.CodeName
          ttMenuItem.Module    = TMSCodes.FieldName.
END.


RUN pInitMenuBrowser.

/* Currently no tokenlib checking */
DEFINE VARIABLE lcRight AS CHAR INIT "R".

RUN local-find-first.

IF AVAILABLE ttMaintMenu THEN ASSIGN
   Memory       = ROWID(ttMaintMenu)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

RUN pInitDesc(ttMaintMenu.MaintType,
              ttMaintMenu.FramePos,
              "").
RUN pActionMenu(FALSE,
                ttMaintMenu.MaintType,
                "Top").
LOOP:

REPEAT WITH FRAME frLeft:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :
   
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttMaintMenu WHERE ROWID(ttMaintMenu) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose ROWID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME frLeft:
           IF AVAILABLE ttMaintMenu THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = ROWID(ttMaintMenu).
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
   REPEAT WITH FRAME frLeft ON ENDKEY UNDO, RETURN:
      IF ufkey THEN DO:
         ASSIGN
           ufk    = 0
           ufk[1] = 0
           ufk[2] = 0
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.
      
         RUN Syst/ufkey.
      END.
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttMaintMenu.MaintType
         GO-ON (home f1 f2 f3 f4 END CURSOR-DOWN
                CURSOR-UP CURSOR-LEFT CURSOR-RIGHT " " TAB F8)
         WITH FRAME frLeft. 
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
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME frLeft:
         IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttMaintMenu THEN DO:
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
                rtab[1] = ROWID(ttMaintMenu)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
        RUN local-find-this(FALSE).
        RUN pActionMenu(FALSE,
                        ttMaintMenu.MaintType,
                        "Top").
        RUN pInitDesc(ttMaintMenu.MaintType,
                      ttMaintMenu.FramePos,
                      "").
      END. /* PREVious ROW */
      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME frLeft:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttMaintMenu THEN DO:
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
              rtab[FRAME-DOWN] = ROWID(ttMaintMenu).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.        
        END.
        ELSE DO:
           DOWN 1.
           
           IF rtab[FRAME-LINE] = ? THEN UP 1.
        END.    
        RUN local-find-this(FALSE).
        RUN pActionMenu(FALSE,
                        ttMaintMenu.MaintType,
                        "Top").
        RUN pInitDesc(ttMaintMenu.MaintType,
                      ttMaintMenu.FramePos,
                      "").

      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttMaintMenu WHERE ROWID(ttMaintMenu) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttMaintMenu THEN DO:
           Memory = ROWID(ttMaintMenu).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttMaintMenu THEN Memory = ROWID(ttMaintMenu).
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
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME frLeft:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND ttMaintMenu WHERE ROWID(ttMaintMenu) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO:
        RUN local-find-this(FALSE).

        IF AVAILABLE ttMaintMenu THEN DO:
           RUN pActionMenu(TRUE, /* run as display */
                           ttMaintMenu.MaintType,
                           "Top").
           
           RUN pInitDesc(ttMaintMenu.MaintType,
                         "Left",
                         "").
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttMaintMenu) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttMaintMenu) must-print = TRUE.
        NEXT LOOP.
     END.
     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME frLeft NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttMaintMenu WHERE ROWID(ttMaintMenu) = rtab[frame-line(frLeft)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttMaintMenu WHERE ROWID(ttMaintMenu) = rtab[frame-line(frLeft)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF lcRight = "R" THEN DO:
      FIND FIRST ttMaintMenu NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF lcRight = "R" THEN DO:
      FIND LAST ttMaintMenu NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF lcRight = "R" THEN DO:
     FIND NEXT ttMaintMenu NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF lcRight = "R" THEN DO:
      FIND PREV ttMaintMenu NO-LOCK NO-ERROR.
   END.                  

END PROCEDURE.

PROCEDURE local-disp-row:

    CLEAR FRAME frLeft NO-PAUSE.
    DISPLAY 
       
    ttMaintMenu.MaintType
    WITH FRAME frLeft.

END PROCEDURE.

PROCEDURE pInitDesc.
   DEFINE INPUT PARAMETER icType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icPos  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icAct  AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO. 
   /* Reset descriptions */

   DO liLoop = 1 TO 4:
      lcDesc[liLoop] = "".
   END.


   FOR FIRST ttMenuItem WHERE
             ttMenuItem.MaintType = icType AND
             ttMenuItem.FramePos  = icPos  AND
             ttMenuItem.Action    = icAct:
         
      DO liLoop = 1 TO NUM-ENTRIES(ttMenuitem.Descr,","):
         lcDesc[liLoop] = ENTRY(liLoop,ttMenuItem.Descr,",").
      END.
   END.
   
   DISPLAY lcDesc[1] SKIP 
           lcDesc[2] SKIP 
           lcDesc[3] SKIP
           lcDesc[4] SKIP

   WITH FRAME frDesc NO-LABELS. 

END.

PROCEDURE pInitMenuBrowser.

   FOR EACH ttMenuItem NO-LOCK BREAK BY ttMenuItem.MaintType:
      IF FIRST-OF (ttMenuItem.MaintType) THEN DO:
         CREATE ttMaintMenu.
         ASSIGN ttMaintMenu.MaintType = ttMenuItem.MaintType
                ttMaintMenu.FramePos  = "Left".   
      END.
   END.
END.

PROCEDURE pActionMenu.

   DEFINE INPUT PARAMETER llMenu      AS LOGICAL   NO-UNDO.
   DEFINE INPUT PARAMETER icMaintType AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icFramePos  AS CHARACTER NO-UNDO.

   DEF VAR liRowCount AS INTEGER NO-UNDO INIT 0.
   
   /* Reset menu */
   DO liRowCount = 1 TO 8:
      menuc[liRowCount] = "".
   END.
   liRowCount = 0.


   DEF VAR lcRkey     AS CHARACTER NO-UNDO.
   FOR EACH ttMenuItem NO-LOCK WHERE
            ttMenuItem.MaintType = icMaintType AND
            ttMenuItem.FramePos  = "Top":
      liRowCount = liRowCount + 1.
      CREATE ttMaintAction.
      ASSIGN ttmaintAction.FrameIndex = liRowCount 
             ttMaintAction.Action = ttMenuItem.Action
             ttMaintAction.Module = ttMenuItem.Module
             menuc[liRowCount] = ttMenuItem.Action.
   END.
   DISPLAY menuc[1] FORMAT "x(56)" SKIP
           menuc[2] FORMAT "x(56)" SKIP
           menuc[3] FORMAT "x(56)" SKIP
           menuc[4] FORMAT "x(56)" SKIP
           menuc[5] FORMAT "x(56)" SKIP
           menuc[6] FORMAT "x(56)" SKIP
           menuc[7] FORMAT "x(56)" SKIP
           menuc[8] FORMAT "x(56)" SKIP

   WITH OVERLAY FRAME frTop NO-LABELS.
   
   IF NOT llMenu THEN DO:
      EMPTY TEMP-TABLE ttMaintAction.
      LEAVE.
   END.
   DO WHILE TRUE:
   
      CHOOSE FIELD menuc AUTO-RETURN GO-ON (F8) 
      WITH FRAME frTop
      ROW 2 WITH 22 COL.
      LASTKEY = -1. /* Reset lastkey */
      lcRKey = KEYLABEL(LASTKEY). 
      IF LOOKUP(lcRKey,"RETURN")  > 0  THEN DO:
         FIND ttMaintAction WHERE
              ttMaintAction.FrameIndex = FRAME-INDEX
         NO-LOCK NO-ERROR.
         IF AVAILABLE ttMaintAction THEN DO:
            RUN pInitDesc(icMaintType,
                          "Top",
                          ttMaintAction.Action).
            RUN VALUE(ttMaintAction.Module).            
            EMPTY TEMP-TABLE ttMaintAction.
            RUN Syst/ufkey.
            LEAVE.
            
         END.
      END.
      
      IF LOOKUP(lcRkey,"F8") > 0 THEN DO:
         EMPTY TEMP-TABLE ttMaintAction.
         APPLY LASTKEY.
         LEAVE.
      END.
   /* Clean up menu after use / d~isplay */
   END.   

END PROCEDURE.
      
