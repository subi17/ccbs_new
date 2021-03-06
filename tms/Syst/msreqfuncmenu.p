/* -----------------------------------------------
  MODULE .......: requeuebrowser.p
  FUNCTION .....: Request queue browser 
  APPLICATION ..: TMS
  AUTHOR .......: JT 
  CREATED ......: 22.08.07
  Version ......: TMS Master
  ------------------------------------------------------ */

{Syst/commali.i}

DEFINE INPUT PARAMETER iiMsRequest AS INTEGER NO-UNDO.

FIND MsRequest WHERE
     MsRequest.MsRequest = iiMsRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest THEN DO:
   MESSAGE "Request not found (handled by automation)!"
   VIEW-AS ALERT-BOX.
   LEAVE.
END.

DEFINE TEMP-TABLE ttMenu
 FIELD Module   AS CHARACTER
 FIELD MenuText AS CHARACTER
 FIELD CParam   AS CHARACTER
 FIELD IParam   AS INTEGER.

DEF VAR lcCodeName   AS CHAR NO-UNDO.
DEF VAR liCodeValue  AS INT  NO-UNDO.
DEF VAR liStatNow    AS INT  NO-UNDO.
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
DEF VAR lcStatuses   AS CHAR                   NO-UNDO.
DEF VAR liFill       AS INTEGER                NO-UNDO.

icTitle = "Request functions".

/* Get status from MsRequest */
liStatNow = MsRequest.ReqStatus.

RUN pInitMenu.

form
    ttMenu.MenuText FORMAT "x(50)"
   
WITH ROW 2 OVERLAY FrmDown DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + icTitle + " " 
    CENTERED
    FRAME sel.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

/* Empty label */
ASSIGN ttMenu.MenuText:LABEL IN FRAME sel = "".

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
           Syst.Var:ufk    = 0 
           Syst.Var:ufk[1] = 0 
           Syst.Var:ufk[2] = 0
           Syst.Var:ufk[5] = 11 
           Syst.Var:ufk[8] = 8 
           Syst.Var:ehto   = 3 
           ufkey  = FALSE.
      
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttMenu.Menutext {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) ttMenu.MenuText WITH FRAME sel.
      END.
      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
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
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
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
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
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
     
     ELSE IF LOOKUP(Syst.Var:nap,"5,f5,enter,return") > 0 THEN DO:
        RUN local-find-this(FALSE).
        
        IF AVAILABLE ttMenu THEN DO:
           
           RUN VALUE(ttMenu.Module)(iiMsRequest,
                                    liStatNow,
                                    ttMenu.IParam,
                                    ttMenu.Cparam).
           
           HIDE FRAME sel.
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttMenu) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttMenu) must-print = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.


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

PROCEDURE pInitMenu.
   DEF VAR liLoop AS INTEGER NO-UNDO.

   /* Get list of available functions for this type and status from FuncGroup */
   FIND MsReqStatFunc NO-LOCK WHERE
        MsReqStatFunc.ReqType   = MsRequest.ReqType AND
        MsReqStatFunc.ReqStatus = MsRequest.ReqStatus
   NO-ERROR.
   /* Create menu from FuncGroup list, first entry is 0, not a real function,
      just indicator that this MsRequest with status can be visible in request
      queue menus */

   DO liLoop = 2 TO NUM-ENTRIES(MsReqStatFunc.FuncGroup,","):
      FIND MsReqFuncItem NO-LOCK WHERE
           MsReqFuncItem.ItemId = ENTRY(liLoop,MsReqStatFunc.FuncGroup,",")
      NO-ERROR.

      CREATE ttMenu.
      ASSIGN ttMenu.Module   = MsReqFuncItem.Module
             ttMenu.MenuText = MsReqFuncItem.ItemDesc
             ttMenu.CParam   = MsReqFuncItem.CParam
             ttMenu.IParam   = MsReqFuncItem.IParam.
             FrmDown         = FrmDown + 1.
      IF FrmDown > 15 THEN FrmDown = 15.
   END.


END.

