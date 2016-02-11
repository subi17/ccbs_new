/* ----------------------------------------------------------------------
  MODULE .......: prepcdr.P
  TASK .........: Browse ERRONEUS mobile calls
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-09-99
  CHANGED ......: 18.11.99 pt disp "not analysed" @ username
                  24.03.00 jpo f5- Roaming view
                  21.03.03 jp xbsub
                  25.03.03 jp xbsub -> func.i
                  26.01.06 jt UserName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER Customer)
                  25.01.07 kl beta for invoicable mobcdrs

  Version ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}
{Func/func.p}

DEFINE TEMP-TABLE ttCall LIKE MobCDR.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR DateSt  LIKE PrepCDR.DateSt               NO-UNDO.
DEF VAR CustNum  LIKE PrepCDR.CustNum               NO-UNDO.
DEF VAR CLI  LIKE PrepCDR.CLI               NO-UNDO.
DEF VAR Rc           AS INT               NO-uNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR qmark        AS DA                     NO-UNDO  INIT ?.
DEF VAR stime        AS C                      NO-UNDO.
DEF VAR UserName     AS C                      NO-UNDO.
DEF VAR SL_prefix    AS C                      NO-UNDO.
DEF VAR roamview     AS i                      NO-UNDO.
DEF VAR lcTime       AS C                      NO-UNDO.

{Func/tmsparam.i DefMSISDNPr  return} SL_prefix = TMSParam.CharVal.

form
    PrepCDR.DateSt  
    lcTime             COLUMN-LABEL "Time"       FORMAT "X(5)"
    PrepCDR.CustNum     COLUMN-LABEL "A-Cust"     FORMAT "zzzzzzzz9"
    Customer.CustName COLUMN-LABEL "Cust. Name"  FORMAT "x(9)"
    UserName        COLUMN-LABEL "User"          FORMAT "x(9)"
    PrepCDR.CLI                                   FORMAT "x(11)"
    PrepCDR.GsmBnr                                FORMAT "x(8)"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Erroneus Mobile calls "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    PrepCDR.DateSt     /* LABEL FORMAT */
    PrepCDR.CustNum    /* LABEL FORMAT */
            /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Mobile Call  BY  DateSt */
    DateSt
    HELP "Enter first desired call Date where that code should occur"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE/DATE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.



cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Error Code,By A-Customer,BY MSISDN No.,By 4".


FIND FIRST PrepCDR WHERE
           PrepCDR.ErrorCode = 0
NO-LOCK NO-ERROR.

IF AVAILABLE PrepCDR THEN ASSIGN
   Memory       = recid(PrepCDR)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE 
   "There are NO Billable ISValue"  SKIP
   "in the call database"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PrepCDR WHERE recid(PrepCDR) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PrepCDR THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PrepCDR).
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
        ufk[1]= 35  ufk[2]= 0   ufk[3]= 1102 ufk[4]= 0
        ufk[5]= 265 ufk[6]= 0   ufk[7]= 0    ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW PrepCDR.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PrepCDR.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW PrepCDR.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PrepCDR.CLI WITH FRAME sel.
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
        FIND PrepCDR WHERE recid(PrepCDR) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PrepCDR THEN
              ASSIGN FIRSTrow = i Memory = recid(PrepCDR).
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
           IF NOT AVAILABLE PrepCDR THEN DO:
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
                rtab[1] = recid(PrepCDR)
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
           IF NOT AVAILABLE PrepCDR THEN DO:
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
              rtab[FRAME-DOWN] = recid(PrepCDR).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PrepCDR WHERE recid(PrepCDR) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PrepCDR THEN DO:
           Memory = recid(PrepCDR).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PrepCDR THEN Memory = recid(PrepCDR).
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
           FIND PrepCDR WHERE recid(PrepCDR) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DateSt = 12/1/2006.
       UPDATE DateSt WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       FIND FIRST PrepCDR WHERE 
                  PrepCDR.DateSt = DateSt       
       USE-INDEX ErrorCode NO-LOCK NO-ERROR.
       IF NOT AVAILABLE PrepCDR THEN DO:
          BELL.
          MESSAGE "NOT FOUND !".
          PAUSE 1 NO-MESSAGE.
          NEXT BROWSE.
       END.
       /* some PrepCDR/DateSt was found */
       ASSIGN order = 1 Memory = recid(PrepCDR) must-print = TRUE.
       NEXT LOOP.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"3,F3") > 0 THEN DO:
        RUN local-find-this(FALSE).
        /* build an international style MSISDN No. from National CLI */
        CLI = SL_Prefix + PrepCDR.CLI.
        find first mobsub where
                   mobsub.cli = PrepCDR.CLI
        no-lock no-error.
        if avail mobsub then RUN Mm/msowner(mobsub.msseq).
        ufkey = TRUE.
        NEXT.
     END.   

     ELSE IF LOOKUP(nap,"4,F4") > 0 THEN DO:
        NEXT.
     END.   


     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN DO:  /* VIEW */
        RUN local-find-this(FALSE).
        CREATE ttCall.
        BUFFER-COPY PrepCDR TO ttCall.
        RUN Mm/viewmbd(INPUT TABLE ttcall,
                          ttcall.datest, 
                          ttcall.timest,
                          ttCall.cli,
                          ttcall.dtlseq ).
        EMPTY TEMP-TABLE ttCall.
        ufkey = TRUE.
        NEXT loop.
     END.    


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PrepCDR.DateSt.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(PrepCDR).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PrepCDR) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PrepCDR) must-print = TRUE.
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
      FIND PrepCDR WHERE recid(PrepCDR) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PrepCDR WHERE recid(PrepCDR) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       
   IF order = 1 THEN FIND FIRST PrepCDR WHERE
                                PrepCDR.ErrorCode = 0
                     NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND FIRST PrepCDR WHERE
                                     PrepCDR.ErrorCode = 0
                          USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND FIRST PrepCDR WHERE
                                     PrepCDR.ErrorCode = 0
                          USE-INDEX CLI NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
       
   IF order = 1 THEN FIND LAST PrepCDR WHERE
                               PrepCDR.ErrorCode = 0
                     NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND LAST PrepCDR WHERE
                                    PrepCDR.ErrorCode = 0
                          USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND LAST PrepCDR WHERE
                                     PrepCDR.ErrorCode = 0
                          USE-INDEX CLI NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
       
   IF order = 1 THEN FIND NEXT PrepCDR WHERE
                               PrepCDR.ErrorCode = 0
                     NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND NEXT PrepCDR WHERE
                                    PrepCDR.ErrorCode = 0
                          USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND NEXT PrepCDR WHERE
                                    PrepCDR.ErrorCode = 0
                          USE-INDEX CLI NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
       
   IF order = 1 THEN FIND PREV PrepCDR WHERE
                               PrepCDR.ErrorCode = 0
                     NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND PREV PrepCDR WHERE
                                    PrepCDR.ErrorCode = 0
                          USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND PREV PrepCDR WHERE
                                    PrepCDR.ErrorCode = 0
                          USE-INDEX CLI NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       PrepCDR.DateSt
       STRING(PrepCDR.TimeSt,"HH:MM") @ lcTime
       PrepCDR.CustNum
       Customer.CustName    WHEN AVAIL Customer
       username
       PrepCDR.CLI
       DYNAMIC-FUNCTION("fHideBSub" IN ghFunc1,
          PrepCDR.gsmbnr,
          PrepCDR.custnum,
          PrepCDR.bdest,
          PrepCDR.BType,
          PrepCDR.bpref,
          true) @ PrepCDR.GsmBnr
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND MobSub WHERE MobSub.MsSeq = PrepCDR.MsSeq NO-LOCK NO-ERROR.
       IF AVAIL MobSub THEN DO:
          FIND Customer OF MobSub NO-LOCK NO-ERROR.      
          UserName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, 
                                       BUFFER Customer). 
       END.
       ELSE DO:
          UserName = "!! UNKNOWN !!".
       END.
       
       FIND Customer WHERE Customer.CustNum = PrepCDR.CustNum NO-LOCK NO-ERROR.
       FIND MobError WHERE 
            MobError.MobError = PrepCDR.ErrorCode NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-UPDATE-record:
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

