/* ----------------------------------------------------------------------
  MODULE .......: CustBal
  TASK .........: browse table CustBal
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 16.02.07
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CustBal'}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.

DEF VAR lcCLI        AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 6.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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

DEF VAR lcBalType    AS CHAR                   NO-UNDO.
DEF VAR ldAPTotal    AS DEC                    NO-UNDO.
DEF VAR ldOPTotal    AS DEC                    NO-UNDO.
DEF VAR ldDPTotal    AS DEC                    NO-UNDO.
DEF VAR ldINTTotal   AS DEC                    NO-UNDO.
DEF VAR ldREFTotal   AS DEC                    NO-UNDO.
DEF VAR ldCLTotal    AS DEC                    NO-UNDO.

DEF TEMP-TABLE ttCustBal NO-UNDO
   FIELD Order    AS INT 
   FIELD CLI      AS CHAR
   FIELD BalType  AS CHAR
   FIELD Amt      AS DEC 
   INDEX CLI Order CLI.
   
FORM
   ldOPTotal AT 2 
      LABEL "Overpayment ....."
      FORMAT "->>>>>>>9.99"
   ldREFTotal AT 35
      LABEL "Refund ........."
      FORMAT "->>>>>>>9.99"
      SKIP
   ldAPTotal AT 2 
      LABEL "Advance Payment ."
      FORMAT "->>>>>>>9.99"
   ldCLTotal AT 35
      LABEL "Doubtful Receiv."
      FORMAT "->>>>>>>9.99"
      SKIP
   ldINTTotal AT 2 
      LABEL "Unbilled Interest"
      FORMAT "->>>>>>>9.99"
   ldDPTotal AT 35
      LABEL "Deposit ........"
      FORMAT "->>>>>>>9.99"
      SKIP
WITH ROW 1 CENTERED OVERLAY SIDE-LABELS 
    TITLE " BALANCES OF CUSTOMER "  + STRING(iiCustNum) + " "
    FRAME fTotal.
   
form
    ttCustBal.CLI        COLUMN-LABEL "Target"  FORMAT "X(14)"
    ttCustBal.BalType    COLUMN-LABEL "Type"    FORMAT "X(5)"
    lcBalType            COLUMN-LABEL "Desc."   FORMAT "X(28)" 
    ttCustBal.Amt        COLUMN-LABEL "Balance" FORMAT "->>,>>>,>>9.99" 
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc)
       " DETAILED A/R BALANCES "
    FRAME sel.

form /* seek  CustBal */
    "MSISDN:" lcCLI FORMAT "X(12)"  
    HELP "Enter MSISDN"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FUNCTION fMakeTemp RETURNS LOGIC
   (icType AS CHAR,
    idAmt  AS DEC):
    
   CREATE ttCustBal.
   BUFFER-COPY CustBal TO ttCustBal.
   ASSIGN i                 = i + 1
          ttCustBal.Order   = i
          ttCustBal.BalType = icType
          ttCustBal.Amt     = idAmt.
     
   IF ttCustBal.CLI = "" THEN ttCustBal.CLI = "Customer". 
   
END FUNCTION.    
    

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN pGetBalances.

PAUSE 0.
DISP ldAPTotal 
     ldOPTotal
     ldDPTotal
     ldREFTotal
     ldINTTotal
     ldCLTotal
     WITH FRAME fTotal.
     
RUN local-find-first.

IF AVAILABLE ttCustBal THEN ASSIGN
   Memory       = recid(ttCustBal)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.


LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.
    
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttCustBal WHERE recid(ttCustBal) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttCustBal THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttCustBal).
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
        ufk[1] = 36
        ufk[4] = 1642
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttCustBal.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttCustBal.CLI WITH FRAME sel.
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
        FIND ttCustBal WHERE recid(ttCustBal) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttCustBal THEN
              ASSIGN FIRSTrow = i Memory = recid(ttCustBal).
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
           IF NOT AVAILABLE ttCustBal THEN DO:
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
                rtab[1] = recid(ttCustBal)
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
           IF NOT AVAILABLE ttCustBal THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttCustBal).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttCustBal WHERE recid(ttCustBal) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttCustBal THEN DO:
           Memory = recid(ttCustBal).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttCustBal THEN Memory = recid(ttCustBal).
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
           FIND ttCustBal WHERE recid(ttCustBal) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE lcCLI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF lcCLI > "" THEN DO:
       
          FIND FIRST ttCustBal WHERE 
                     ttCustBal.CLI >= lcCLI
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttCustBal THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* refund balances */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
        RUN local-find-this(FALSE).

        IF ttCustBal.BalType NE "AP" THEN DO:
           MESSAGE "This is not an advance payment. Function not allowed."
           VIEW-AS ALERT-BOX INFORMATION.
           NEXT.
        END.
        
        RUN Ar/refundadvp(iiCustNum,
                       ttCustBal.CLI).

        RUN pGetBalances.

        RUN local-find-first.
        memory = RECID(ttCustBal).
        
        ufkey = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttCustBal) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttCustBal) must-print = TRUE.
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
      FIND ttCustBal WHERE recid(ttCustBal) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttCustBal WHERE recid(ttCustBal) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN 
          FIND FIRST ttCustBal NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN 
          FIND LAST ttCustBal NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN 
          FIND NEXT ttCustBal NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN 
          FIND PREV ttCustBal NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ttCustBal.CLI
       ttCustBal.BalType
       lcBalType
       ttCustBal.Amt
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   
   lcBalType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "CustBal",
                                "BalType",
                                ttCustBal.BalType).
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT WITH FRAME fInvDet ON ENDKEY UNDO, LEAVE:
   
   END.
   
END PROCEDURE.

PROCEDURE pGetBalances:

   EMPTY TEMP-TABLE ttCustBal.

   i = 20.
    
   FOR EACH CustBal NO-LOCK WHERE
            CustBal.CustNum = iiCustNum
   BREAK BY CustBal.CLI:

      fMakeTemp("AP",
                CustBal.AdvPaym).
      fMakeTemp("OP",
                CustBal.OverPaym).
      fMakeTemp("REF",
                CustBal.Refund).
      fMakeTemp("CL",
                CustBal.CreditLoss).
      fMakeTemp("DP",
                CustBal.Deposit).
      fMakeTemp("INT",
                CustBal.Interest).
                 
      ACCUMULATE CustBal.OverPaym   (TOTAL)
                 CustBal.AdvPaym    (TOTAL)
                 CustBal.Deposit    (TOTAL)
                 CustBal.Interest   (TOTAL)
                 CustBal.Refund     (TOTAL)
                 CustBal.CreditLoss (TOTAL).
              
      IF LAST(CustBal.CLI) THEN DO:
   
         i = 0.
         
         ASSIGN ldAPTotal  = ACCUM TOTAL CustBal.AdvPaym
                ldOPTotal  = ACCUM TOTAL CustBal.OverPaym
                ldDPTotal  = ACCUM TOTAL CustBal.Deposit
                ldREFTotal = ACCUM TOTAL CustBal.Refund
                ldCLTotal  = ACCUM TOTAL CustBal.CreditLoss
                ldINTTotal = ACCUM TOTAL CustBal.Interest.
      END.
   END.
   
END PROCEDURE.


