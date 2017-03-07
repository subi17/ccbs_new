/* ----------------------------------------------------------------------
  MODULE .......: prepaidbr.p
  TASK .........: Browse all prepaid tickets
  APPLICATION ..: 
  AUTHOR .......: 
  CREATED ......: 
  CHANGED ......: 
  Version ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}
{Func/func.p}
{Func/callquery.i}

   
DEF /* NEW */ shared VAR siirto AS CHAR.


DEF VAR DateSt   LIKE Mobcdr.DateSt               NO-UNDO.
DEF VAR timest   LIKE Mobcdr.timest               NO-UNDO.
DEF VAR timest1  LIKE Mobcdr.TimeStart            NO-UNDO.
DEF VAR CustNum  LIKE Mobcdr.CustNum              NO-UNDO.
DEF VAR CLI      LIKE Mobcdr.CLI                  NO-UNDO.
DEF VAR gsmbnr   LIKE Mobcdr.Gsmbnr               NO-UNDO.

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
DEF VAR def-ccode    AS C                      NO-UNDO.
DEF VAR Billed     AS LO                     NO-UNDO.
DEF VAR SL_prefix  AS C  NO-UNDO.

{Func/tmsparam.i SL_prefix      return}.  SL_prefix = TMSParam.CharVal.
{Func/tmsparam.i DefCCode       return}.  def-ccode = TMSParam.CharVal.

form
    prepcdr.DateSt  
    STime               COLUMN-LABEL "Time"        FORMAT "x(5)"
    prepcdr.BillCode     COLUMN-LABEL "Pr"          FORMAT "x(4)"
    prepcdr.CustNum      COLUMN-LABEL "A-Cust"      FORMAT "zzzzzzzz9"
    Customer.CustName   COLUMN-LABEL "Cust. Name"  FORMAT "x(11)"
    UserName            COLUMN-LABEL "User"        FORMAT "x(9)"
    prepcdr.CLI                                     FORMAT "x(11)"
    prepcdr.GsmBnr                                  FORMAT "x(10)"
    space(0)
    Billed           COLUMN-LABEL "I"              FORMAT "*/"


WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Billable Mobile calls "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    prepcdr.DateSt     /* LABEL FORMAT */
    prepcdr.CustNum    /* LABEL FORMAT */
                    
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr
    SIDE-LABELS  1 columns    FRAME lis.

form /* seek Mobile Call  BY  DateSt */
    DateSt timest
    HELP "Enter CallDate and time"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date AND TIME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Mobile Call  BY CustNum */
    CustNum
    HELP "Enter A-Sub Customer No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND A-CUST "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek Mobile Call  BY A-sub. */

    CLI FORMAT "x(12)"
    HELP "Enter calling MSISDN No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek Mobile Call  BY A-sub. */
    gsmbnr FORMAT "x(11)"
    HELP "Enter called b-No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND B-NUMBER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = 
"By Call Date, By Product, By A-Customer, BY MSISDN No., By CallType ".


FIND FIRST prepcdr WHERE 
           prepcdr.errorcode = 0 
NO-LOCK NO-ERROR.
IF AVAILABLE prepcdr THEN ASSIGN
   Memory       = recid(prepcdr)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE 
   "There are NO Billable Calls"  SKIP
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

   IF must-add THEN DO:  /* Add a prepcdr  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR prepcdr.DateSt
           VALIDATE
              (prepcdr.DateSt NOT ENTERED OR
              NOT CAN-FIND(prepcdr using  prepcdr.DateSt),
              "Mobile Call " + string(INPUT prepcdr.DateSt) +
              " already exists !").
           IF INPUT FRAME lis prepcdr.DateSt NOT ENTERED THEN 
           LEAVE add-row.
           CREATE prepcdr.
           ASSIGN
           prepcdr.DateSt = INPUT FRAME lis prepcdr.DateSt.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(prepcdr)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST prepcdr WHERE errorcode = 0 
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE prepcdr THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND prepcdr WHERE recid(prepcdr) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE prepcdr THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(prepcdr).
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
        ufk[1]= 713 ufk[2]= 702 ufk[3]= 209 ufk[4]= 704
        ufk[5]= 265 ufk[6]= 0   ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW prepcdr.DateSt {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) prepcdr.DateSt WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW prepcdr.BillCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) prepcdr.BillCode WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW prepcdr.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) prepcdr.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW prepcdr.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) prepcdr.CLI WITH FRAME sel.
      END.
      ELSE IF order = 5 THEN DO:
        CHOOSE ROW prepcdr.GsmBnr  {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) prepcdr.GsmBnr WITH FRAME sel.
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
        FIND prepcdr WHERE recid(prepcdr) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE prepcdr THEN
              ASSIGN FIRSTrow = i Memory = recid(prepcdr).
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
           IF NOT AVAILABLE prepcdr THEN DO:
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
                rtab[1] = recid(prepcdr)
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
           IF NOT AVAILABLE prepcdr THEN DO:
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
              rtab[FRAME-DOWN] = recid(prepcdr).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND prepcdr WHERE recid(prepcdr) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE prepcdr THEN DO:
           Memory = recid(prepcdr).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE prepcdr THEN Memory = recid(prepcdr).
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
           FIND prepcdr WHERE recid(prepcdr) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET DateSt timest WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF DateSt NE qmark THEN DO:
       
       timest1 = prepcdr.timest.

          FIND FIRST  prepcdr WHERE 
                      prepcdr.DateSt    = DateSt   AND
                      prepcdr.TimeStart >= timest1    
          USE-INDEX Date  NO-LOCK  NO-ERROR.
          IF NOT AVAILABLE prepcdr THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END. 
          /* some prepcdr/DateSt was found */
          ASSIGN order = 1 Memory = recid(prepcdr) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET CustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CustNum ENTERED THEN DO:
          FIND FIRST prepcdr WHERE 
                     prepcdr.CustNum >= CustNum
          USE-INDEX CustNum  NO-LOCK NO-ERROR.
          IF NOT AVAILABLE prepcdr THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some prepcdr/CustNum was found */
          ASSIGN order = 2 Memory = recid(prepcdr) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F3.
       SET CLI WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF CLI NE "" THEN DO:

          /* substitute Country code in m_pref WITH 0 */
          FIND FIRST prepcdr WHERE 
                     prepcdr.datest ne ?              AND
                     prepcdr.CLI >= CLI
          USE-INDEX CLI NO-LOCK NO-ERROR.
          IF NOT AVAILABLE prepcdr THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some prepcdr/CLI was found */
          ASSIGN order = 3 Memory = recid(prepcdr) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */
     
     
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F4.
       SET gsmbnr WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.
       IF gsmbnr NE "" THEN DO:

          /* substitute Country code in m_pref WITH 0 */
          FIND FIRST prepcdr WHERE 
                     prepcdr.gsmbnr >= gsmbnr
          USE-INDEX gsmbnr NO-LOCK NO-ERROR.
          IF NOT AVAILABLE prepcdr THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some prepcdr/CLI was found */
          ASSIGN order = 4 Memory = recid(prepcdr) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-4 */

     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN DO:  /* VIEW */
        
        FIND FIRST prepcdr WHERE
             recid(prepcdr) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.

        RUN Mm/viewmcdr2.p (INPUT  RECID(PrepCDR),
                              "PrepCDR").
        ufkey = TRUE.
        NEXT loop.
     END.    

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY prepcdr.DateSt.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(prepcdr).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(prepcdr) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(prepcdr) must-print = TRUE.
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
      FIND prepcdr WHERE recid(prepcdr) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND prepcdr WHERE recid(prepcdr) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST prepcdr      USE-INDEX Date
       WHERE  prepcdr.errorcode = 0 NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST prepcdr      USE-INDEX Date
       WHERE prepcdr.errorcode = 0 NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT prepcdr      USE-INDEX Date
       WHERE prepcdr.errorcode = 0 NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV prepcdr      USE-INDEX Date
       WHERE prepcdr.errorcode = 0 NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       prepcdr.DateSt
       stime
       prepcdr.CustNum
       Customer.CustName    WHEN AVAIL Customer
       username
       prepcdr.CLI  
       prepcdr.BillCode
       DYNAMIC-FUNCTION("fHideBSub" IN ghFunc1,
          prepcdr.gsmbnr,
          prepcdr.custnum,
          prepcdr.bdest,
          prepcdr.BType,
          prepcdr.bpref,
          true) @ prepcdr.GsmBnr
       billed
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       Stime = STRING(prepcdr.TimeStart,"HH:MM").
       FIND MobSub WHERE MobSub.MsSeq = prepcdr.MsSeq NO-LOCK NO-ERROR.
       FIND Customer WHERE Customer.CustNum = prepcdr.CustNum NO-LOCK NO-ERROR.

       IF Avail Customer then username = DYNAMIC-FUNCTION("fDispCustName" IN                        ghFunc1,  BUFFER Customer) .
       FIND InvSeq where 
            InvSeq.InvSeq = prepcdr.InvSeq no-lock no-error.
       IF AVAIL InvSeq AND InvSeq.Billed = TRUE THEN Billed = TRUE.
       ELSE Billed = FALSE.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          prepcdr.CustNum


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

