/* ----------------------------------------------------------------------
  MODULE .......: errorprepaid
  TASK .........: Updates table MobError
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 27.08.03 
  CHANGED ......: 
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{commali.i} 

SESSION:SYSTEM-ALERT-BOXES = TRUE.

{eventval.i}
if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {lib/eventlog.i}

    DEF VAR lhMobError AS HANDLE NO-UNDO.
    lhMobError = BUFFER MobError:HANDLE.
    RUN StarEventInitialize(lhMobError).

    ON F12 ANYWHERE DO:
        run eventview2.p(lhMobError).
    END.
END.

def /* new */ shared var siirto AS char.

DEF VAR ErrorCode    like MobError.mobError        NO-UNDO.
DEF VAR me-name      like MobError.MEName         NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR LcCheck      AS LOG format "CHECK/"    NO-UNDO.
DEF VAR ErrQty       AS CHAR FORMAT "x(6)"     NO-UNDO.
DEF VAR iqty         AS INT                    NO-UNDO.

FORM
    MobError.MobError     column-label "Code"
    MobError.MEName     /* column-label format */
    LcCheck           Column-label "Check"
    ErrQty
WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  ERROR ITEMS MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

FORM
   MobError.MobError  /* label format */
   MobError.MEName    /* label format */
WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

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

form
    MobError.memo

    with overlay row 3 centered
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc)
    " MEMO: " + MobError.MEName + " " WITH no-labels 1 columns
    frame f4.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST MobError
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE MobError THEN ASSIGN
   memory       = recid(MobError)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = true.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MobError  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR MobError.MobError
           validate
              (MobError.MobError NOT ENTERED or
              NOT CAN-FIND(MobError using  MobError.MobError),
              "ERROR Type " + string(INPUT MobError.MobError) +
              " already exists !").
           IF INPUT FRAME lis MobError.MobError = "" THEN 
           LEAVE add-row.
           create MobError.
           ASSIGN
           MobError.MobError = INPUT FRAME lis MobError.MobError.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMobError).

           ASSIGN
           memory = recid(MobError)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST MobError
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MobError THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND MobError WHERE recid(MobError) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MobError THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(MobError).
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

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0  ufk[4]= 927 
        ufk[5]= 1961 ufk[6]= 1962 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row MobError.MobError ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MobError.MobError WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row MobError.MEName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MobError.MEName WITH FRAME sel.
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
        FIND MobError WHERE recid(MobError) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE MobError THEN
              ASSIGN FIRSTrow = i memory = recid(MobError).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
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
           IF NOT AVAILABLE MobError THEN DO:
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
                rtab[1] = recid(MobError)
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
           IF NOT AVAILABLE MobError THEN DO:
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
              rtab[FRAME-down] = recid(MobError).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND MobError WHERE recid(MobError) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MobError THEN DO:
           memory = recid(MobError).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE MobError THEN memory = recid(MobError).
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
           FIND MobError WHERE recid(MobError) = memory NO-LOCK.
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
          FIND FIRST MobError WHERE MobError.MobError >= ErrorCode
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MobError THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MobError/ErrorCode was found */
          ASSIGN order = 1 memory = recid(MobError) must-print = true.
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
          FIND FIRST MobError WHERE MobError.MEName >= me-name
          USE-INDEX mename /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MobError THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MobError/me-name was found */
          ASSIGN order = 2 memory = recid(MobError) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Update Memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANS on ENDkey undo, NEXT LOOP:


        cfc = "puyr". RUN ufcolor.
        ehto = 9. 
        RUN ufkey. ufkey = true.
        run local-find-this(true).
        UPDATE MobError.memo WITH FRAME f4.
        HIDE FRAME f4 NO-PAUSE.
     END.



     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO: 
       {uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       MESSAGE 
       "Do You Want Rerate all "   SKIP
       "'" MobError.MEName "' - Calls?"                                         
       VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.

       IF ok THEN 
       run error_preprate.p(INPUT MobError.MobError).
       ufkey = true.
       run ufkey.p.

     END. 

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO : 
       {uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       run errorprep.p(INPUT MobError.MobError).
       ufkey = true.
       run ufkey.p.

     END. 

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MobError.MobError.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobError).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobError).

       RUN local-disp-row.
       xrecid = recid(MobError).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(MobError) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(MobError) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find MobError WHERE recid(MobError) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find MobError WHERE recid(MobError) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MobError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST MobError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MobError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST MobError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MobError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT MobError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MobError
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV MobError USE-INDEX MEName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MobError.MobError 
       MobError.MEName
       lcCheck
       ErrQty

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   ASSIGN
      lcCheck = false
      iqty    = 0 .
   FOR EACH PrepCDR USE-index ErrorCode where 
            PrepCDR.ErrorCode = MobError.MobError no-lock.
      iqty = iqty + 1.

      if iqty mod 100 = 0 then leave.      
   END.         

    if iqty = 100 then ErrQty = ">100".
    ELSE                ErrQty = String(iqty).

    IF iqty > 0 AND 
       LOOKUP(STRING(MobError.MobError),"1001,1002,1003,1004,2001,3001") > 0 THEN
    lccheck = TRUE.

END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          MobError.MEName


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.
