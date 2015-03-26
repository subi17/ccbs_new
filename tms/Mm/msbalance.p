/* ----------------------------------------------------------------------
  MODULE .......: MSBalance
  TASK .........: browse table MSBalance
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 23.01.08
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */
{commali.i}
{timestamp.i}
{ftopup.i}

{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'MSBalance'}

DEF INPUT PARAMETER iiMsSeq   AS INT NO-UNDO.
DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 4.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
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

DEF VAR lcBalType  AS CHAR NO-UNDO.
DEF VAR ldBalance  AS DEC  NO-UNDO EXTENT 6.
DEF VAR liMsSeq    AS INT  NO-UNDO.
DEF VAR lcCLI      AS CHAR NO-UNDO.
DEF VAR lcLabel    AS CHAR NO-UNDO.
DEF VAR ldMinusAmt AS DEC  NO-UNDO.

DEF TEMP-TABLE ttBal NO-UNDO
   FIELD BalType AS CHAR
   FIELD BalName AS CHAR
   FIELD BalNbr  AS INT 
   FIELD Amount  AS DEC
   INDEX BalType BalType.

FORM
   ldBalance[1] AT 2 
      LABEL "Balance 1 ......."
      FORMAT "->>>>>>>9.99"
   ldBalance[4] AT 35
      LABEL "Balance 4 ......."
      FORMAT "->>>>>>>9.99"
      SKIP
   ldBalance[2] AT 2 
      LABEL "Balance 2 ......."
      FORMAT "->>>>>>>9.99"
   ldBalance[5] AT 35
      LABEL "Balance 5 ......."
      FORMAT "->>>>>>>9.99"
      SKIP
   ldBalance[3] AT 2 
      LABEL "Balance 3 ......."
      FORMAT "->>>>>>>9.99"
   ldBalance[6] AT 35
      LABEL "Balance 6 ......."
      FORMAT "->>>>>>>9.99"
      SKIP
WITH ROW 1 CENTERED OVERLAY SIDE-LABELS 
    TITLE " TOTAL BALANCES, CUSTOMER " + STRING(iiCustNum) + " " 
    FRAME fTotal.
   
form
    MsBalance.MsSeq      COLUMN-LABEL "Subs.ID" 
    lcCLI                COLUMN-LABEL "MSISDN"  FORMAT "X(10)" 
    MsBalance.BalType    COLUMN-LABEL "Type"    
    lcBalType            COLUMN-LABEL "Desc."   FORMAT "X(21)" 
    MsBalance.BalDate    
    MsBalance.Amount     COLUMN-LABEL "Balance" FORMAT "->>,>>>,>>9.99" 
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc)
       " SUBSCRIPTION BALANCES "
    FRAME sel.

FORM
    MsBalance.MsSeq      COLON 20
    lcCLI                COLON 20 
       LABEL "MSISDN"  
       FORMAT "X(10)" 
    MsBalance.BalType    COLON 20 
       lcBalType NO-LABEL FORMAT "X(21)" SKIP
    MsBalance.Amount     COLON 20 
       LABEL "Balance" 
       FORMAT "->>,>>>,>>9.99" 
    SKIP(1)
    ldMinusAmt COLON 20 
       LABEL "Minus Adjustment"
       HELP "Amount that is deducted from subscription's balance"
       FORMAT ">>,>>>,>>9.99"
WITH ROW 4 CENTERED OVERLAY SIDE-LABELS
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc)
       " MINUS ADJUSTMENT "
    FRAME fMinus.

form /* seek  MSBalance */
    "Subscription:" liMsSeq FORMAT ">>>>>>>9"
       HELP "Enter subscription id"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Subscription "
       COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FUNCTION fGetRequestID RETURNS INTEGER:

   DEF VAR liNextID AS INT NO-UNDO.
   
   DO WHILE TRUE:
      liNextID = NEXT-VALUE(PrePaidReq).
   
      IF NOT CAN-FIND(FIRST PrePaidRequest WHERE
                            PrePaidRequest.Brand     = gcBrand AND
                            PrepaidRequest.PPRequest = liNextID)
      THEN LEAVE.
   END.
   
   RETURN liNextID.
   
END.
 


IF iiMsSeq = 0 AND iiCustNum > 0 THEN DO:
   RUN pGetBalances.

   FOR EACH ttBal WHERE
            ttBal.BalNbr >= 1 AND ttBal.BalNbr <= 6:

      lcLabel = ttBal.BalName.
      IF LENGTH(lcLabel) < 18 THEN 
         lcLabel = lcLabel + " " + FILL(".",17 - LENGTH(lcLabel)).
      
      
      /* label-attribute requires a constant subscript */
      CASE ttBal.BalNbr:
      WHEN 1 THEN ldBalance[1]:LABEL IN FRAME fTotal = lcLabel.
      WHEN 2 THEN ldBalance[2]:LABEL IN FRAME fTotal = lcLabel.
      WHEN 3 THEN ldBalance[3]:LABEL IN FRAME fTotal = lcLabel.
      WHEN 4 THEN ldBalance[4]:LABEL IN FRAME fTotal = lcLabel.
      WHEN 5 THEN ldBalance[5]:LABEL IN FRAME fTotal = lcLabel.
      WHEN 6 THEN ldBalance[6]:LABEL IN FRAME fTotal = lcLabel.
      END CASE.
      
      ldBalance[ttBal.BalNbr] = ttBal.Amount.
   END.

   PAUSE 0.
   DISP ldBalance WITH FRAME fTotal.

   ASSIGN 
      FrmRow  = 6
      FrmDown = 10.
END.

ELSE IF iiMsSeq > 0 THEN DO:

   FOR FIRST MsOwner NO-LOCK WHERE
             MsOwner.MsSeq = iiMsSeq:
      lcCLI = MsOwner.CLI.
   END.
END.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE MsBalance THEN ASSIGN
   Memory       = recid(MsBalance)
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
        FIND MsBalance WHERE recid(MsBalance) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MsBalance THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MsBalance).
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
        ufk[1] = IF iiMsSeq = 0 THEN 1645 ELSE 0
        ufk[3] = 1086
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MsBalance.MsSeq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MsBalance.MsSeq WITH FRAME sel.
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
        FIND MsBalance WHERE recid(MsBalance) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MsBalance THEN
              ASSIGN FIRSTrow = i Memory = recid(MsBalance).
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
           IF NOT AVAILABLE MsBalance THEN DO:
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
                rtab[1] = recid(MsBalance)
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
           IF NOT AVAILABLE MsBalance THEN DO:
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
              rtab[FRAME-DOWN] = recid(MsBalance).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MsBalance WHERE recid(MsBalance) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MsBalance THEN DO:
           Memory = recid(MsBalance).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MsBalance THEN Memory = recid(MsBalance).
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
           FIND MsBalance WHERE recid(MsBalance) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE liMsSeq WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF liMsSeq > 0 THEN DO:
       
          FIND FIRST MsBalance WHERE 
                     MsBalance.CustNum = iiCustNum AND
                     MsBalance.MsSeq  >= liMsSeq
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE MsBalance THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* minus adjustment */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 THEN DO:  

        RUN local-find-this(FALSE).

        ufkey = TRUE.

        RUN pMinusAdjustment. 
        
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MsBalance) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MsBalance) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
HIDE FRAME fTotal NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND MsBalance WHERE recid(MsBalance) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MsBalance WHERE recid(MsBalance) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiMsSeq > 0 THEN DO:
      IF order = 1 THEN 
         FIND FIRST MsBalance WHERE MsBalance.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN 
         FIND FIRST MsBalance WHERE MsBalance.CustNum = iiCustNum 
         NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN 
         FIND FIRST MsBalance NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF iiMsSeq > 0 THEN DO:
      IF order = 1 THEN 
         FIND LAST MsBalance WHERE MsBalance.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN 
         FIND LAST MsBalance WHERE MsBalance.CustNum = iiCustNum 
         NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN 
         FIND LAST MsBalance NO-LOCK NO-ERROR.
   END.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF iiMsSeq > 0 THEN DO:
      IF order = 1 THEN 
         FIND NEXT MsBalance WHERE MsBalance.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN 
         FIND NEXT MsBalance WHERE MsBalance.CustNum = iiCustNum 
         NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN 
         FIND NEXT MsBalance NO-LOCK NO-ERROR.
   END.
           
END PROCEDURE.

PROCEDURE local-find-PREV:
 
   IF iiMsSeq > 0 THEN DO:
      IF order = 1 THEN 
         FIND PREV MsBalance WHERE MsBalance.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN 
         FIND PREV MsBalance WHERE MsBalance.CustNum = iiCustNum 
         NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN 
         FIND PREV MsBalance NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       MsBalance.MsSeq
       lcCLI
       MsBalance.BalType
       lcBalType
       MsBalance.BalDate
       MsBalance.Amount
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   
   lcBalType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "MSBalance",
                                "BalType",
                                MsBalance.BalType).

   IF iiMsSeq = 0 THEN DO:
      lcCLI = "".
      FOR FIRST MsOwner NO-LOCK WHERE
                MsOwner.MsSeq = MsBalance.MsSeq:
         lcCLI = MsOwner.CLI.
      END.
   END.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT WITH FRAME fInvDet ON ENDKEY UNDO, LEAVE:
   
   END.
   
END PROCEDURE.

PROCEDURE pGetBalances:

   DEF VAR liBalance AS INT  NO-UNDO.
   
   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "MsBalance" AND
            TMSCodes.FieldName = "BalType":
            
      CREATE ttBal.
      ASSIGN ttBal.BalType = TMSCodes.CodeValue
             ttBal.BalName = TMSCodes.CodeName.
             
      IF TMSCodes.ConfigValue > "" THEN 
         ttBal.BalNbr = INTEGER(ENTRY(1,TMSCodes.ConfigValue)) NO-ERROR.
         
      IF ttBal.BalNbr = 0 THEN ASSIGN
         liBalance     = liBalance + 1
         ttBal.BalNbr  = liBalance.
   END.
   
   FOR EACH MSBalance NO-LOCK WHERE
            MSBalance.CustNum = iiCustNum:

      FIND FIRST ttBal WHERE ttBal.BalType = MsBalance.BalType NO-ERROR.
      IF AVAILABLE ttBal THEN 
         ttBal.Amount = ttBal.Amount + MsBalance.Amount.
   END.
   
END PROCEDURE.

PROCEDURE pMinusAdjustment:

   DEF VAR lcTaxZone  AS CHAR NO-UNDO. 
   DEF VAR ldTopupBal AS DEC  NO-UNDO.
   DEF VAR llPending  AS LOG  NO-UNDO.

   /* for atm-payments must be made from ppreqbr */
   IF MsBalance.BalType = "TOP" THEN DO:
      MESSAGE "Minus adjustment for atm payments must be made" 
              "from the original events"
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.

   IF MsBalance.Amount <= 0 THEN DO:
      MESSAGE "No balance to be handled"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
        
   RUN local-find-others.

   PAUSE 0.
   VIEW FRAME fMinus.
        
   llPending = FALSE.
   
   FOR EACH PrepaidRequest NO-LOCK WHERE
            PrepaidRequest.Brand    = gcBrand AND
            PrepaidRequest.MsSeq    = MsBalance.MsSeq AND
            PrepaidRequest.PPStatus = 0:
      llPending = TRUE.
      LEAVE.
   END.
                   
   IF llPending THEN DO:
      MESSAGE "There are pending topup requests on this subscription."
              "They must be handled before minus adjustment is made."
      VIEW-AS ALERT-BOX INFORMATION.

      HIDE FRAME fMinus NO-PAUSE.
      RETURN.
   END.
  
   ldMinusAmt = 0.
                 
   REPEAT WITH FRAME fMinus:
       
     /* no need to handle tax; all balances except "top" are handled
        without tax -> also minus adjustment from them without tax */
              
      DISPLAY 
         MsBalance.MsSeq
         lcCLI
         MsBalance.BalType
         lcBalType
         MsBalance.Amount
      WITH FRAME fMinus. 

      UPDATE ldMinusAmt WITH FRAME fMinus.
            
      ASSIGN 
         ufk    = 0
         ufk[1] = 7
         ufk[5] = 1089
         ufk[8] = 8
         ehto   = 0.
      RUN ufkey.
           
      IF toimi = 5 THEN DO:
            
         IF ldMinusAmt = 0 THEN DO:
            MESSAGE "Nothing to do."
            VIEW-AS ALERT-BOX INFORMATION.
            NEXT.
         END.
         
         /* check balance once more */
         FIND CURRENT MsBalance NO-LOCK.
             
         IF ldMinusAmt > MsBalance.Amount THEN DO:
            MESSAGE "Minus adjustment cannot be more than current"
                    "balance."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         ldtopupbal = 40.

         /* if this occurs, then there is either something wrong with
            the balances, or a new event has occurred while this
            process has been going on */
         IF ldMinusAmt > ldTopupBal THEN DO:
            MESSAGE "Current balance on prepaid platform is"
                    ldTopupBal "eur. Minus adjustment cannot be more"
                    "than that."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
                     
         FIND MobSub WHERE MobSub.MsSeq = MsBalance.MsSeq NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MobSub THEN DO:
            MESSAGE "An active subscription could not be found"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.

         IF MobSub.PayType = FALSE THEN DO:
            MESSAGE "Type of subscription is not prepaid."
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         lcTaxZone = "".
         FOR FIRST Customer NO-LOCK WHERE 
                   Customer.CustNum = MobSub.InvCust,
             FIRST Region NO-LOCK WHERE 
                   Region.Region = Customer.Region:
            lcTaxZone = Region.TaxZone.       
         END.
         
         /* user limit? */

         i = fAddTopUp(MsBalance.MsSeq,
                       lcCLI,
                       "AdjustmentTRequest",
                       "CC",
                       "AdjustmentTRequest",
                       "999",
                       lcTaxZone,
                       -1 * ldMinusAmt * 100,
                       0).  
                 
         MESSAGE "Request for minus adjustment has been created with ID" i
         VIEW-AS ALERT-BOX 
         TITLE " Done ".

         LEAVE.
      END.
           
      ELSE IF toimi = 8 THEN LEAVE. 
   END.

   HIDE FRAME fMinus NO-PAUSE.


END PROCEDURE.

