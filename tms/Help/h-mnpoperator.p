/* -----------------------------------------------
  MODULE .......: h-MNPOperator
  FUNCTION .....: MNPOperator help
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 07/2010
  Version ......: yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/fcustdata.i}

DEF SHARED VAR siirto AS CHAR.

DEF VAR lcOperCode  AS CHAR NO-UNDO.
DEF VAR lcMNPOperator  AS CHAR NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 2.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.

form
    MNPOperator.OperCode   
    MNPOperator.OperName
WITH ROW FrmRow CENTERED OVERLAY FrmDown DOWN
    COLOR VALUE(Syst.CUICommon:cfc)   
    TITLE COLOR VALUE(Syst.CUICommon:ctc) "  MNP Operators  " 
    FRAME sel.

form /* seek  MNPOperator */
    "MNPOperator:" lcMNPOperator
    HELP "Enter MNPOperator"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND MNPOperator "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  */
    "Name:" lcOperCode
    HELP "Enter MNPOperator name"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND Name "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f2.


Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE MNPOperator THEN ASSIGN
   Memory       = recid(MNPOperator)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :
   
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MNPOperator THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MNPOperator).
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
           ufk    = 0
           ufk[1] = 35
           ufk[2] = 30
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW MNPOperator.OperName {Syst/uchoose.i} NO-ERROR 
            WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.CUICommon:ccc) MNPOperator.OperName WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW MNPOperator.OperCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.CUICommon:ccc) MNPOperator.OperCode WITH FRAME sel.
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

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MNPOperator THEN
              ASSIGN FIRSTrow = i Memory = recid(MNPOperator).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE MNPOperator THEN DO:
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
                rtab[1] = recid(MNPOperator)
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
           IF NOT AVAILABLE MNPOperator THEN DO:
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
              rtab[FRAME-DOWN] = recid(MNPOperator).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MNPOperator THEN DO:
           Memory = recid(MNPOperator).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MNPOperator THEN Memory = recid(MNPOperator).
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
           FIND MNPOperator WHERE recid(MNPOperator) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME f1.
        lcMNPOperator = "".
        UPDATE lcMNPOperator WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.

        IF lcMNPOperator > "" THEN DO:
           FIND FIRST MNPOperator WHERE 
                      MNPOperator.OperName >= lcMNPOperator
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE MNPOperator THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(MNPOperator)
              must-print = TRUE
              order      = 1.
              
           NEXT LOOP.
        END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME f2.
        lcOperCode = "".
        UPDATE lcOperCode WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.

        IF lcOperCode > "" THEN DO:
           FIND FIRST MNPOperator WHERE 
                      MNPOperator.OperCode >= lcOperCode
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE MNPOperator THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(MNPOperator)
              must-print = TRUE
              order      = 2.
              
           NEXT LOOP.
        END.
     END. /* Search-2 */


     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO: 
        RUN local-find-this(FALSE).

        IF AVAILABLE MNPOperator THEN DO:
           siirto = MNPOperator.OperName.
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MNPOperator) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MNPOperator) must-print = TRUE.
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
      FIND MNPOperator WHERE recid(MNPOperator) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MNPOperator WHERE recid(MNPOperator) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperName NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperCode NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperName NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperCode NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperName NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperCode NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN 
      FIND PREV MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperName NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV MNPOperator WHERE
                MNPOperator.Brand EQ Syst.CUICommon:gcBrand AND
                MNPOperator.Active EQ True
                USE-INDEX OperCode NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       MNPOperator.OperName    
       MNPOperator.OperCode
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.


