/* ----------------------------------------------------------------------------
  MODULE .......: BALREP
  FUNCTION .....: Current balance data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 23.06.04
  MODIFIED .....: 
                  16.06.06/aam ClaimState instead of ClaimQty
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fcustbal.i}
{Func/finvbal.i}
{Syst/utumaa.i}

DEF INPUT PARAMETER icInvGrp1   AS CHAR  NO-UNDO.
DEF INPUT PARAMETER icInvGrp2   AS CHAR  NO-UNDO.
DEF INPUT PARAMETER iiCustNum1  AS INT   NO-UNDO.
DEF INPUT PARAMETER iiCustNum2  AS INT   NO-UNDO.
DEF INPUT PARAMETER iiInvType1  AS INT   NO-UNDO.
DEF INPUT PARAMETER iiInvType2  AS INT   NO-UNDO.
DEF INPUT PARAMETER idClaimQty1 AS DEC   NO-UNDO.
DEF INPUT PARAMETER idClaimQty2 AS DEC   NO-UNDO.
DEF INPUT PARAMETER iiPaymPlan  AS INT   NO-UNDO.
DEF INPUT PARAMETER idtPaymDate AS DATE  NO-UNDO. 
DEF INPUT PARAMETER icFile      AS CHAR  NO-UNDO. 

DEF VAR viiva1 AS CHAR NO-UNDO FORMAT "x(112)".
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR sl     AS INT  NO-UNDO.
DEF VAR rl     AS INT  NO-UNDO.
DEF VAR lev    AS INT  NO-UNDO INIT 112.

DEF VAR lcSessionNum AS CHAR NO-UNDO.
DEF VAR lcDateHeader AS CHAR NO-UNDO.
DEF VAR liQty        AS INT  NO-UNDO.
DEF VAR liPrint      AS INT  NO-UNDO. 
DEF VAR ldDebt       AS DEC  NO-UNDO. 
DEF VAR ldDue        AS DEC  NO-UNDO. 
DEF VAR ldBal        AS DEC  NO-UNDO.
DEF VAR liInvQty     AS INT  NO-UNDO.
DEF VAR liDueQty     AS INT  NO-UNDO. 
DEF VAR ldDeposit    AS DEC  NO-UNDO.
DEF VAR ldAdvPaym    AS DEC  NO-UNDO.
DEF VAR ldOverPaym   AS DEC  NO-UNDO. 
DEF VAR liCnt        AS INT  NO-UNDO. 
DEF VAR llFound      AS LOG  NO-UNDO. 

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum  AS INT
   FIELD CustName AS CHAR
   FIELD Debt     AS DEC
   FIELD InvQty   AS INT
   FIELD DueDebt  AS DEC
   FIELD DueQty   AS INT
   FIELD Deposit  AS DEC
   FIELD OverPaym AS DEC
   FIELD AdvPaym  AS DEC
   FIELD TotDebt  AS DEC
   FIELD Paid     AS DEC
   FIELD PaymQty  AS INT
   FIELD Latest   AS DATE
   INDEX CustNum CustNum 
   INDEX TotDebt TotDebt DESC CustNum.
   
ASSIGN 
    lcSessionNum           = SESSION:NUMERIC-FORMAT
    SESSION:NUMERIC-FORMAT = "European"
    viiva1                 = fill("=",lev)
    viiva2                 = fill("=",lev)
    viiva3                 = fill("-",lev)
    viiva4                 = fill("-",lev).

form header
   viiva1 AT 1 SKIP
   ynimi at 1 format "x(35)" 
      "CURRENT A/R BALANCES" AT 45
      "Page" AT 103 
      sl format "ZZZZ9" SKIP
   lcDateHeader AT 45 FORMAT "X(40)"
      pvm format "99.99.9999" AT 103 SKIP
   viiva2 AT 1 SKIP
   "Customer"  TO 8
   "Name"      AT 10
   "Inv.Debt"  TO 42
   "Deposit"   TO 53
   "Overpaym"  TO 64
   "Adv.Paym"  TO 75
   "Tot.Debt"  TO 88
   "Payments"  TO 99
   "Qty"       TO 103
   "Latest"    AT 105 
   SKIP
   viiva3 AT 1 SKIP
   WITH WIDTH 112 NO-LABEL NO-BOX FRAME fPage.

FUNCTION fCheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF icFile > "" THEN RETURN FALSE.
    
    IF rl >= skayt1 - iAddLine THEN DO:

        IF sl > 0 THEN DO:
           {Syst/uprfeed.i rl}
        END.
           
        sl = sl + 1.
        VIEW STREAM tul FRAME fPage.  
        rl = 6.
    END.

    RETURN TRUE.
END.

ASSIGN sl = 0
       rl = 0
       lcDateHeader = "Payments after " +
                      STRING(idtPaymDate,"99.99.9999").
fCheckPage(999).       

IF icFile > "" THEN DO:

   OUTPUT STREAM tul TO VALUE(icFile).
   
   PUT STREAM tul UNFORMATTED
      "Customer"       CHR(9)
      "Name"           CHR(9)
      "Invoice Debt"   CHR(9)
      "Qty of Unpaid"  CHR(9)
      "Due Debt"       CHR(9)
      "Deposit"        CHR(9)
      "Overpaym"       CHR(9)
      "Adv.Paym"       CHR(9)
      "Total Debt"     CHR(9)
      "Payments"       CHR(9)
      "Payment Qty"    CHR(9)
      "Latest Payment" SKIP. 

END.

FOR EACH InvGroup NO-LOCK WHERE
         InvGroup.Brand     = gcBrand  AND
         InvGroup.InvGroup >= icInvGrp1 AND
         InvGroup.InvGroup <= icInvGrp2,
    EACH Customer NO-LOCK WHERE
         Customer.InvGroup = InvGroup.InvGroup AND
         Customer.CustNum >= iiCustNum1        AND
         Customer.CustNum <= iiCustNum2
BY Customer.CustNum:

    liQty = liQty + 1.
    IF (liQty < 100 OR liQty MOD 100 = 0) AND
       SESSION:BATCH = FALSE
    THEN DO:
       PAUSE 0.
       DISPLAY "Checked:" liQty   FORMAT ">>,>>>,>>9" SKIP
               " Picked:" liPrint FORMAT ">>,>>>,>>9"
       WITH NO-LABELS OVERLAY ROW 15 CENTERED TITLE " Processing "
       FRAME fQty.
    END.

    ASSIGN ldDebt   = 0
           ldDue    = 0
           liInvQty = 0
           liDueQty = 0.
    
    /* debt from invoices */
    FOR EACH Invoice NO-LOCK WHERE
             Invoice.Brand       = gcBrand          AND
             Invoice.CustNum     = Customer.CustNum AND
             Invoice.ClaimState >= idClaimQty1      AND
             Invoice.ClaimState <= idClaimQty2      AND
             Invoice.InvType    >= iiInvType1       AND
             Invoice.InvType    <= iiInvType2:

       /* part of an active payment plan */
       IF iiPaymPlan > 0 THEN DO:
             
          llFound = FALSE.
          FOR EACH PPInv OF Invoice NO-LOCK,
             FIRST PaymPlan OF PPInv NO-LOCK WHERE
                   PaymPlan.PPStatus < 4:
             llFound = TRUE.
             LEAVE.
          END.     

          IF (iiPaymPlan = 1 AND NOT llFound) OR
             (iiPaymPlan = 2 AND llFound)
          THEN NEXT. 
       END.
 
       /* open balance */
       ldBal = fInvBal(BUFFER Invoice,
                       TODAY).
      
       IF ldBal = 0 THEN NEXT.
       
       ASSIGN ldDebt   = ldDebt + ldBal
              liInvQty = liInvQty + 1.
              
       /* overdue */
       IF Invoice.DueDate < TODAY THEN ASSIGN
              ldDue    = ldDue + ldBal
              liDueQty = liDueQty + 1.
    END.
    
    
    /* special balances */
    ASSIGN ldDeposit  = fGetCustBal(Customer.CustNum,"TOTAL","DP")
           ldOverPaym = fGetCustBal(Customer.CustNum,"TOTAL","OP")
           ldAdvPaym  = fGetCustBal(Customer.CustNum,"TOTAL","AP").
           
    IF ldDebt     = 0 AND
       ldDeposit  = 0 AND
       ldOverPaym = 0 AND
       ldAdvPaym  = 0
    THEN NEXT.
    
    /* into temptable -> sort by debt */    
    CREATE ttCust.
    ASSIGN ttCust.CustNum = Customer.CustNum
           ttCust.CustName = Customer.CustName
           ttCust.Debt     = ldDebt
           ttCust.InvQty   = liInvQty
           ttCust.DueDebt  = ldDue
           ttCust.DueQty   = liDueQty
           ttCust.Deposit  = ldDeposit
           ttCust.OverPaym = ldOverPaym 
           ttCust.AdvPaym  = ldAdvPaym
           ttCust.TotDebt  = ldDebt - ldOverPaym - ldAdvPaym
           liPrint         = liPrint + 1.
           
    /* latest payments */
    FOR EACH Payment OF Customer NO-LOCK WHERE
             Payment.AccDate >= idtPaymDate AND
             Payment.AccDate <= TODAY       AND
             /* credit payments skipped */
             Payment.PaymSrc NE "CR":
    
       DO liCnt = 1 TO 10:
          /* payments to money account */ 
          IF Payment.AccType[liCnt] = 4 THEN DO:
             ASSIGN ttCust.Paid    = ttCust.Paid + Payment.Posting[liCnt]
                    ttCust.PaymQty = ttCust.PaymQty + 1.
                     
             IF ttCust.Latest = ? OR ttCust.Latest < Payment.AccDate
             THEN ttCust.Latest = Payment.AccDate.
          END.
       END.

    END.

END.
  
FOR EACH ttCust USE-INDEX TotDebt:

    IF icFile = "" THEN DO:
       fCheckPage(0).
    
       PUT STREAM tul           
       ttCust.CustNum  AT 1   FORMAT ">>>>>>>9"
       ttCust.CustName AT 10  FORMAT "X(20)"
       ttCust.Debt     TO 42  FORMAT "->>>>>>9.99"
       ttCust.Deposit  TO 53  FORMAT "->>>>>9.99"
       ttCust.OverPaym TO 64  FORMAT "->>>>>9.99"
       ttCust.AdvPaym  TO 75  FORMAT "->>>>>9.99"
       ttCust.TotDebt  TO 88  FORMAT "->>>>>>9.99"
       ttCust.Paid     TO 99  FORMAT "->>>>>9.99"
       ttCust.PaymQty  TO 103 FORMAT ">>9"
       ttCust.Latest   AT 105 FORMAT "99.99.99"
       SKIP.
    
       rl = rl + 1.
    END.
    
    ELSE PUT STREAM tul UNFORMATTED 
       ttCust.CustNum  CHR(9)
       ttCust.CustName CHR(9)
       ttCust.Debt     CHR(9)
       ttCust.InvQty   CHR(9)
       ttCust.DueDebt  CHR(9)
       ttCust.Deposit  CHR(9)
       ttCust.OverPaym CHR(9)
       ttCust.AdvPaym  CHR(9)
       ttCust.TotDebt  CHR(9)
       ttCust.Paid     CHR(9)
       ttCust.PaymQty  CHR(9)
       (IF ttCust.Latest NE ?
        THEN STRING(ttCust.Latest,"99.99.99")
        ELSE "")       SKIP.
           
    ACCUMULATE ttCust.Debt     (TOTAL)
               ttCust.Deposit  (TOTAL)
               ttCust.OverPaym (TOTAL)
               ttCust.AdvPaym  (TOTAL)
               ttCust.TotDebt  (TOTAL)
               ttCust.Paid     (TOTAL)
               ttCust.PaymQty  (TOTAL).
END.

HIDE FRAME fQty NO-PAUSE. 

/* grand total */
IF icFile = "" THEN DO:
   
   fCheckPage(1).
   
   PUT STREAM tul 
      FILL("-",112) AT 1 FORMAT "X(112)" 
      SKIP
      "Total"                       AT 1 
      (ACCUM TOTAL ttCust.Debt)     TO 42  FORMAT "->>>>>>>9.99"
      (ACCUM TOTAL ttCust.Deposit)  TO 53  FORMAT "->>>>>9.99"
      (ACCUM TOTAL ttCust.OverPaym) TO 64  FORMAT "->>>>>9.99"
      (ACCUM TOTAL ttCust.AdvPaym)  TO 75  FORMAT "->>>>>9.99"
      (ACCUM TOTAL ttCust.TotDebt)  TO 88  FORMAT "->>>>>>>9.99"
      (ACCUM TOTAL ttCust.Paid)     TO 99  FORMAT "->>>>>9.99"
      (ACCUM TOTAL ttCust.PaymQty)  TO 103 FORMAT ">>9"
      SKIP.
   rl = rl + 2.

   {Syst/uprfeed.i rl}
END.

ELSE OUTPUT STREAM tul CLOSE.

ASSIGN SESSION:NUMERIC-FORMAT = lcSessionNum.


