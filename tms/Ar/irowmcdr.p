/* ----------------------------------------------------------------------
  MODULE .......: IROWMCDR
  TASK .........: Browse Billable mobile calls
  APPLICATION ..: TMS
  AUTHOR .......: pt
  CREATED ......: 05-11-99
  CHANGED ......: 17.11.99 pt only 1 CHOOSE order, USE-INDEX InvSeq
                  17.04.00 f5 - view Roaming calls
                  12.11.02 jp - better search criteria
                  05.03.03 aam  use-index invseq for first find
                  19.06.03 jp - hidden b-number
                  19.03.04 aam  input CLI, use temp-table,
                                renamed nnlrpu2 -> irowmcdr
                  30.03.04 kl Billed always true
                  22.09.04 jp   input icBPref
                  17.03.05 aam  ignore prefix "151",
                                find invoice instead of invseq with iiInvNum
                  15.12.05 aam  username removed (= custname)
                  24.01.06 jt   DYNAMIC-FUNCTION("fDispCustName"
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}

DEF /* NEW */ SHARED VAR siirto AS C.
DEF INPUT PARAMETER  iiInvNum   AS i    NO-UNDO.
DEF INPUT PARAMETER  pvm1       AS Date NO-UNDO.
DEF INPUT PARAMETER  pvm2       AS Date NO-UNDO.
DEF INPUT PARAMETER  icBillCode AS c    NO-UNDO.
DEF INPUT PARAMETER  icCLI      AS CHAR NO-UNDO.
DEF INPUT PARAMETER  icBPref    AS CHAR NO-UNDO.

DEF VAR DateSt       LIKE MobCDR.DateSt        NO-UNDO.
DEF VAR CustNum      LIKE MobCDR.CustNum       NO-UNDO.
DEF VAR CLI          LIKE MobCDR.CLI           NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR qmark        AS DA                     NO-UNDO  INIT ?.
DEF VAR stime        AS C                      NO-UNDO.
DEF VAR Billed     AS LOG                      NO-UNDO.
DEF VAR def-ccode    AS C                      NO-UNDO.
DEF VAR lcCustName   AS CHAR                   NO-UNDO.
{Func/tmsparam.i DefCCode   return} def-ccode = TMSParam.CharVal.

DEF TEMP-TABLE ttRow NO-UNDO
   FIELD MobCdr   AS INT
   FIELD DateSt   AS DATE
   FIELD TimeSt   AS INT
   INDEX DateSt DateSt TimeSt. 
   
form
    MobCDR.DateSt  
    STime             COLUMN-LABEL "Time"        FORMAT "x(5)"
    MobCDR.BillCode   COLUMN-LABEL "Pr"          FORMAT "x(4)"
    MobCDR.CustNum    COLUMN-LABEL "A-Cust"      FORMAT "zzzzzzzz9"
    lcCustName        COLUMN-LABEL "Cust. Name"  FORMAT "x(19)"
    MobCDR.CLI                                   FORMAT "x(12)"
    MobCDR.GsmBnr                                FORMAT "x(10)"
    Billed     COLUMN-LABEL "I"           FORMAT "*/"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Billable Mobile calls "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    MobCDR.DateSt     /* LABEL FORMAT */
    MobCDR.CustNum    /* LABEL FORMAT */
            /* LABEL FORMAT */

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Mobile Call  BY  DateSt */
    DateSt
    HELP "Enter CallDate"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


form /* seek Mobile Call  BY A-sub. */
    m_pref SPACE(0)
    CLI FORMAT "x(8)"
    HELP "Enter calling MSISDN No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Call Date,,BY MSISDN No.,By 4".

FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK NO-ERROR.

IF AVAILABLE Invoice THEN 
FOR EACH SubInvoice OF Invoice NO-LOCK WHERE
    (IF icCLI > "" THEN SubInvoice.CLI = icCLI ELSE TRUE),
    EACH MobCDR USE-INDEX InvSeq NO-LOCK WHERE   
         MobCDR.InvCust  = Invoice.CustNum AND 
         MobCDR.InvSeq   = SubInvoice.InvSeq  AND
         MobCDR.DateSt  >= pvm1           AND
         MobCDR.DateSt  <= pvm2           AND     
         MobCDR.BillCode = icBillCode:
      
   IF icCli > "" AND MobCDR.CLI NE icCLI THEN NEXT.
       
   IF icBPref = "" THEN DO:
      IF LOOKUP(MobCDR.BPref,",151") = 0 THEN NEXT. 
   END.
   ELSE IF MobCDR.BPref NE icBPref THEN NEXT. 

   CREATE ttRow.
   ASSIGN ttRow.MobCDR = RECID(MobCDR)
          ttRow.DateSt = MobCDR.DateSt
          ttRow.TimeSt = MobCDR.TimeSt
          i            = i + 1.
          
   IF i MOD 1000 = 0 THEN DO:
      PAUSE 0.
      DISPLAY i LABEL "Qty" FORMAT ">>>>>>>>9"
         WITH FRAME fQty OVERLAY ROW 10 CENTERED TITLE " Collecting ".
   END.
   
END.

HIDE FRAME fQty NO-PAUSE.

FIND FIRST ttRow NO-ERROR.
IF AVAILABLE ttRow THEN ASSIGN
   memory       = recid(ttRow)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE 
   "There are NO calls"  SKIP
   "behind this invoice row"
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
        FIND ttRow WHERE recid(ttRow) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttRow).
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
        ufk[1]= 713 ufk[2]= 0  ufk[3]= 0 ufk[4]= 0
        ufk[5]= 265 ufk[6]= 0  ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MobCDR.DateSt {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MobCDR.DateSt WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MobCDR.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MobCDR.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW MobCDR.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MobCDR.CLI WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND ttRow WHERE recid(ttRow) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttRow THEN
              ASSIGN FIRSTrow = i memory = recid(ttRow).
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
           IF NOT AVAILABLE ttRow THEN DO:
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
                rtab[1] = recid(ttRow)
                memory  = rtab[1].
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
           IF NOT AVAILABLE ttRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttRow).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ttRow WHERE recid(ttRow) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttRow THEN DO:
           memory = recid(ttRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttRow THEN memory = recid(ttRow).
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
           memory = rtab[FRAME-DOWN].
           FIND ttRow WHERE recid(ttRow) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET DateSt WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF DateSt NE ? THEN DO:
          FIND FIRST ttRow WHERE
                     ttRow.DateSt >= DateSt NO-ERROR. 

          IF NOT AVAILABLE ttRow THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some mcdr/DateSt was found */
          ASSIGN order = 1 memory = recid(ttRow) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */


     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN DO:  /* VIEW */
        RUN local-find-this(TRUE).
        RUN Mm/viewmbd(ttRow.MobCDR).

        ufkey = TRUE.
        NEXT loop.
     END.    

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ttRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ttRow) must-print = TRUE.
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
      FIND ttRow WHERE recid(ttRow) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttRow WHERE recid(ttRow) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   FIND FIRST ttRow NO-ERROR. 

END PROCEDURE.

PROCEDURE local-find-LAST:

   FIND LAST ttRow NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   FIND NEXT ttRow NO-ERROR.     

END PROCEDURE.

PROCEDURE local-find-PREV:

   FIND PREV ttRow NO-ERROR.     

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       lcCustName =  DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                      BUFFER Customer).
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MobCDR.DateSt
       stime
       MobCDR.CustNum
       lcCustName    WHEN AVAIL Customer
       MobCDR.CLI
       MobCDR.BillCode
       DYNAMIC-FUNCTION("fHideBSub" IN ghFunc1,
          mobcdr.gsmbnr,
          mobcdr.custnum,
          mobcdr.bdest,
          MobCDR.BType,
          mobcdr.BPref,
          true) @  MobCDR.GsmBnr
       Billed
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

       FIND MobCDR WHERE RECID(MobCDR) = ttRow.MobCDR NO-LOCK.
       
       Stime = STRING(MobCDR.TimeStart,"HH:MM").
       
       FIND Customer WHERE Customer.CustNum = MobCDR.CustNum NO-LOCK NO-ERROR.
       
       Billed = TRUE.
       
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          MobCDR.CustNum


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

