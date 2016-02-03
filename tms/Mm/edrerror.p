/* ----------------------------------------------------------------------
  MODULE .......: edrerror.p
  TASK .........: Updates table EDRError
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 12.2.2013
  VERSION ......: Yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i} 

def /* new */ shared var siirto AS char.

DEFINE TEMP-TABLE ttEDRError LIKE MobError.

FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "PrepEDR" AND
         TMSCodes.FieldName = "ErrorCode" :
   CREATE ttEDRError.
   ASSIGN
      ttEDRError.moberror = INT(TMSCodes.CodeValue)
      ttEDRError.MeName = TMSCodes.CodeName.
END.

DEF VAR ErrorCode    like ttEDRError.mobError        NO-UNDO.
DEF VAR me-name      like ttEDRError.MEName         NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR ErrQty       AS CHAR FORMAT "x(6)"     NO-UNDO.
DEF VAR iqty         AS INT                    NO-UNDO.
DEF VAR ldaFromDate  AS DATE NO-UNDO.
DEF VAR ldaToDate    AS DATE NO-UNDO.
DEF VAR liMaxQty     AS INT  NO-UNDO.

form
    ttEDRError.MobError     column-label "Code"
    ttEDRError.MEName     /* column-label format */
    ErrQty

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  EDR ERROR CODES  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ttEDRError.MobError     /* label format */
    ttEDRError.MEName    /* label format */
            /* label format */

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.
       
FORM
   ttEDRError.MobError
   ldaFromDate 
      FORMAT "99-99-9999" 
      LABEL "From"
      HELP "From"
   ldaToDate 
      FORMAT "99-99-9999"
      LABEL "To"
      HELP "To"
   liMaxQty 
      FORMAT ">>>>>>>9"
      LABEL "Max Quantity"
      HELP "Max number of cdrs"
WITH OVERLAY ROW 8 CENTERED 1 COLUMNS SIDE-LABELS TITLE " Error EDRs"
   FRAME fLimits.

form /* seek  ErrorCode */
    ErrorCode
    HELP "Enter Code of ERROR Type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek  me-name */
    me-name
    HELP "Enter Name of the ERROR Type"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST ttEDRError
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE ttEDRError THEN ASSIGN
   memory       = recid(ttEDRError)
   must-print   = true.
ELSE ASSIGN
   memory       = ?
   must-print   = false.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND ttEDRError WHERE recid(ttEDRError) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttEDRError THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(ttEDRError).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0  ufk[4]= 0 
        ufk[5]= 1961 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row ttEDRError.MobError ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttEDRError.MobError WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row ttEDRError.MEName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttEDRError.MEName WITH FRAME sel.
      END.

      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND ttEDRError WHERE recid(ttEDRError) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttEDRError THEN
              ASSIGN FIRSTrow = i memory = recid(ttEDRError).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttEDRError THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ttEDRError)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttEDRError THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(ttEDRError).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ttEDRError WHERE recid(ttEDRError) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttEDRError THEN DO:
           memory = recid(ttEDRError).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttEDRError THEN memory = recid(ttEDRError).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND ttEDRError WHERE recid(ttEDRError) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET ErrorCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ErrorCode ENTERED THEN DO:
          FIND FIRST ttEDRError WHERE ttEDRError.MobError >= ErrorCode
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttEDRError THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttEDRError/ErrorCode was found */
          ASSIGN order = 1 memory = recid(ttEDRError) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME F2.
       SET me-name WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF me-name ENTERED THEN DO:
          FIND FIRST ttEDRError WHERE ttEDRError.MEName >= me-name
          USE-INDEX mename /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttEDRError THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttEDRError/me-name was found */
          ASSIGN order = 2 memory = recid(ttEDRError) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO : 
       
       RUN pBrowseErrors.
       ufkey = true.
       run ufkey.p.

     END. 

     IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ttEDRError) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ttEDRError) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
HIDE FRAME fLimits NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

   def INPUT parameter exlock as lo NO-undo.

   IF exlock THEN
     find ttEDRError WHERE recid(ttEDRError) = rtab[frame-line(sel)] 
     EXCLUSIVE-LOCK.
   ELSE
      find ttEDRError WHERE recid(ttEDRError) = rtab[frame-line(sel)] 
      NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttEDRError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ttEDRError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttEDRError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ttEDRError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttEDRError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ttEDRError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttEDRError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ttEDRError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      ttEDRError.MobError 
      ttEDRError.MEName
      ErrQty
   WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   ASSIGN
      iqty    = 0 .

   FOR EACH prepedr USE-index ErrorCode where 
            prepedr.ErrorCode = ttEDRError.MobError no-lock.
      iqty = iqty + 1.

      if iqty mod 100 = 0 then leave.      
   END.       
      
   if iqty >= 100 then ErrQty = ">100".
   ELSE                ErrQty = String(iqty).

END PROCEDURE.

PROCEDURE pBrowseErrors:
       
    RUN local-find-this (false).
    
    ASSIGN
       ldaFromDate = 1/1/6
       ldaToDate   = TODAY
       liMaxQty    = 99999999.

    PAUSE 0.
    DISP ttEDRError.MobError WITH FRAME fLimits NO-ERROR. 
    ehto = 9. run ufkey.
    
    UPDATE 
       ldaFromDate 
       ldaToDate 
       liMaxQty 
    WITH FRAME fLimits.

   RUN edrbrowse.p(INPUT "edr,Qty:" + STRING(liMaxQty),
                 INPUT  ldaFromDate,
                 INPUT  ldaToDate,
                 INPUT  0,
                 INPUT  "",
                 INPUT  "",
                 INPUT  0,
                 INPUT  0,
                 INPUT  "",
                 INPUT  "",
                 INPUT  "",
                 INPUT  ttEDRError.MobError,
                 INPUT  0).

END PROCEDURE. 
