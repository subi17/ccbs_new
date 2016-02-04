/* ----------------------------------------------------------------------
  MODULE .......: errorprep.P
  TASK .........: Browse ERRONEUS mobile calls
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-09-99
  CHANGED ......: 18.11.99 pt disp "not analysed" @ username
                  24.03.00 jpo f5- roaming view
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}

DEF INPUT PARAMETER  errorcode AS INT             NO-UNDO.
def /* new */ shared var siirto AS char.

DEF VAR DateSt  like PrepCDR.DateSt               NO-UNDO.
DEF VAR CustNum  like PrepCDR.CustNum             NO-UNDO.
DEF VAR CLI  like PrepCDR.CLI                     NO-UNDO.

DEF  TEMP-TABLE ttCall NO-UNDO LIKE  MobCDR
   FIELD CDRTable AS CHAR
   INDEX date     Datest TimeStart
   INDEX BillCode BillCode
   INDEX CustNum  CustNum Datest TimeStart
   INDEX Cli      Cli     Datest TimeStart gsmbnr spocmt
   INDEX gsmbnr   gsmbnr  Datest TimeStart.

DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 1.
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
DEF VAR qmark        AS DA                     NO-UNDO  INIT ?.
DEF VAR stime        AS C                      NO-UNDO.
DEF VAR username     AS C                      NO-UNDO.
DEF VAR SL_prefix    AS C                      NO-UNDO.
DEF VAR mi-no        AS C                      NO-UNDO.
DEf var roamview     AS i                      NO-UNDO.

{Func/cparam.i DefMSISDNPr  return} SL_prefix = tmsparam.CharVal.

form
    PrepCDR.ErrorCode
    MobError.MEName   COLUMN-LABEL "Error/Expl"  FORMAT "x(25)"
    PrepCDR.DateSt  
    PrepCDR.CustNum     COLUMN-LABEL "A-Cust"      FORMAT "zzzzzzzz9"
    PrepCDR.CLI                                FORMAT "x(12)"
    PrepCDR.GsmBnr                                FORMAT "x(15)"

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Erroneus Mobile Calls "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    PrepCDR.DateSt     /* label format */
    PrepCDR.CustNum    /* label format */
            /* label format */

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Mobile Call  by  DateSt */
    ErrorCode
    HELP "Error Code"
    DateSt
    HELP "Enter first desired call date where that code should occur"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE/DATE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.



cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Error Code,By A-Customer,BY MSISDN No.,By 4".


FIND FIRST PrepCDR
WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
IF AVAILABLE PrepCDR THEN ASSIGN
   memory       = recid(PrepCDR)
   must-print   = true
   must-add     = false.
ELSE DO:
   MESSAGE 
   "There are NOT Errorneous calls"  SKIP
   "for Errorcode " errorcode
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a PrepCDR  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR PrepCDR.DateSt
           validate
              (PrepCDR.DateSt NOT ENTERED or
              NOT CAN-FIND(PrepCDR using  PrepCDR.DateSt),
              "Mobile Call " + string(INPUT PrepCDR.DateSt) +
              " already exists !").
           IF INPUT FRAME lis PrepCDR.DateSt NOT ENTERED THEN 
           LEAVE add-row.
           create PrepCDR.
           ASSIGN
           PrepCDR.DateSt = INPUT FRAME lis PrepCDR.DateSt.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(PrepCDR)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST PrepCDR
      WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PrepCDR THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND PrepCDR WHERE recid(PrepCDR) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PrepCDR THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(PrepCDR).
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
        ufk[1]= 35  ufk[2]= 0   ufk[3]= 1102 ufk[4]= 1101
        ufk[5]= 265 ufk[6]= 0   ufk[7]= 0    ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row PrepCDR.ErrorCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PrepCDR.ErrorCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row PrepCDR.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PrepCDR.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        choose row PrepCDR.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PrepCDR.CLI WITH FRAME sel.
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
        FIND PrepCDR WHERE recid(PrepCDR) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE PrepCDR THEN
              ASSIGN FIRSTrow = i memory = recid(PrepCDR).
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
           IF NOT AVAILABLE PrepCDR THEN DO:
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
                rtab[1] = recid(PrepCDR)
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
           IF NOT AVAILABLE PrepCDR THEN DO:
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
              rtab[FRAME-down] = recid(PrepCDR).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND PrepCDR WHERE recid(PrepCDR) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PrepCDR THEN DO:
           memory = recid(PrepCDR).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE PrepCDR THEN memory = recid(PrepCDR).
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
           FIND PrepCDR WHERE recid(PrepCDR) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME f1.
       DateSt = 1/1/1999.
       UPDATE ErrorCode DateSt WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ErrorCode > 0 THEN DO:
          FIND FIRST PrepCDR WHERE 
                     PrepCDR.ErrorCode = ErrorCode AND
                     PrepCDR.DateSt >= DateSt       
          USE-INDEX ErrorCode NO-LOCK NO-ERROR.
          IF NOT AVAILABLE PrepCDR THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some PrepCDR/DateSt was found */
          ASSIGN order = 1 memory = recid(PrepCDR) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"3,F3") > 0 THEN DO:
        RUN local-find-this(FALSE).
        /* build an international style MSISDN No. from National CLI */
        mi-no = PrepCDR.CLI.
        RUN Mm/msowner2(mi-no).
        ufkey = TRUE.
        NEXT.
     END.   

     ELSE IF LOOKUP(nap,"4,F4") > 0 THEN DO:
        RUN local-find-this(FALSE).
        FIND mobError WHERE Moberror.MobError = PrepCDR.ErrorCode NO-LOCK.
        MESSAGE MobError.MEName.
        PAUSE 2 NO-MESSAGE.
        NEXT.
     END.   


     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN DO:  /* VIEW */
       RUN local-find-this(FALSE).

       CREATE ttCall.
       BUFFER-COPY PrepCDR TO ttCall.
       RUN Mm/viewmbd.p(INPUT TABLE ttcall,
                    ttcall.datest,
                    ttcall.timest,
                    ttCall.cli,
                    ttcall.dtlseq ).
                                
        DELETE ttcall.
        ufkey = TRUE.
        NEXT loop.
     END.    

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PrepCDR.DateSt.

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(PrepCDR).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(PrepCDR) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(PrepCDR) must-print = true.
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
      find PrepCDR WHERE recid(PrepCDR) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find PrepCDR WHERE recid(PrepCDR) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PrepCDR      USE-INDEX ErrorCode
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST PrepCDR USE-INDEx CustNum
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST PrepCDR USE-INDEX CLI
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PrepCDR      USE-INDEX ErrorCode
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST PrepCDR USE-INDEx CustNum
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST PrepCDR USE-INDEX CLI
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PrepCDR      USE-INDEX ErrorCode
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT PrepCDR USE-INDEx CustNum
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT PrepCDR USE-INDEX CLI
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PrepCDR      USE-INDEX ErrorCode
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV PrepCDR USE-INDEx CustNum
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV PrepCDR USE-INDEX CLI
       WHERE PrepCDR.ErrorCode = ErrorCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       PrepCDR.ErrorCode
       MobError.MEName      WHEN AVAIL moberror       
       PrepCDR.DateSt
       PrepCDR.CustNum


       PrepCDR.CLI
       PrepCDR.GsmBnr
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND mobsub WHERE mobsub.cli = PrepCDR.CLI NO-LOCK NO-ERROR.
       FIND Customer WHERE Customer.CustNum = PrepCDR.CustNum NO-LOCK NO-ERROR.
       FIND moberror WHERE 
            moberror.MobError = PrepCDR.ErrorCode NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          PrepCDR.CustNum


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.
