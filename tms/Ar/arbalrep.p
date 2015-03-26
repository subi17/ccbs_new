/* ----------------------------------------------------------------------------
  MODULE .......: ARBALREP
  FUNCTION .....: List Balance data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 25.02.02
  MODIFIED .....: 26.09.02/aam customer balances in table CustBal
                  13.11.02 lp input perameters only InvGroup
                  12.09.03/aam brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
{accexcel.i}
{fcustbal.i}

/* print-linemuuttujat */
{utumaa.i}

DEF INPUT PARAMETER iInvGrp1    AS CHAR  NO-UNDO.
DEF INPUT PARAMETER iInvGrp2    AS CHAR  NO-UNDO.

DEF VAR viiva1 AS CHAR FORMAT "x(91)".
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR jar    AS CHAR FORMAT "x(24)".
DEF VAR order  AS INT.
DEF VAR sl     AS INT.
DEF VAR rl     AS INT.
DEF VAR rlx    AS INT.
DEF VAR lev    AS INT INIT 90.

DEF VAR xOk         AS LOG  NO-UNDO.
DEF VAR xEmpty      AS CHAR NO-UNDO INIT "<EMPTY>".
DEF VAR xSessionNum AS CHAR NO-UNDO.
DEF VAR xDateHeader AS CHAR NO-UNDO.
DEF VAR xQty        AS INT  NO-UNDO.

DEF TEMP-TABLE wBal NO-UNDO
    FIELD InvGrp   AS CHAR
    FIELD Name     AS CHAR
    FIELD Deposit  AS DEC
    FIELD OverPaym AS DEC
    FIELD AdvPaym  AS DEC
    INDEX InvGrp AS UNIQUE InvGrp.

DEF TEMP-TABLE wUnreg NO-UNDO
    FIELD BankAcc AS CHAR
    FIELD Amount  AS DEC
    INDEX BankAcc IS UNIQUE BankAcc.

ASSIGN 
    xSessionNum = SESSION:NUMERIC-FORMAT
    SESSION:NUMERIC-FORMAT = "European"
    viiva1 = fill("=",lev)
    viiva2 = fill("=",lev)
    viiva3 = fill("-",lev)
    viiva4 = fill("-",lev).

form header
   viiva1 AT 1 SKIP
   ynimi at 1 format "x(30)" 
      "A/R BALANCES" AT 40
      "Page" AT 81  
      sl format "ZZZZ9" SKIP
   xDateHeader AT 40 FORMAT "X(30)"
      pvm format "99.99.9999" AT 81 SKIP
   viiva2 AT 1 skip(1)
   WITH width 95 NO-LABEL no-box FRAME sivuotsi.


FUNCTION CheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF rl >= skayt1 - iAddLine THEN DO:
        {uprfeed.i rl}
        ASSIGN rlx = 0
               sl = sl + 1.
        VIEW STREAM tul FRAME sivuotsi.  
        ASSIGN rl = 5.
    END.

    RETURN TRUE.
END.

FUNCTION fPrintBal RETURNS LOGICAL
    (iDeposit  AS DEC,
     iAdvPaym  AS DEC,
     iOverPaym AS DEC).

   PUT STREAM tul
      "Deposits: "         AT 10
      iDeposit  FORMAT "->>,>>>,>>9.99" TO 50
      SKIP
      "Advance payments: " AT 10
      iAdvPaym  FORMAT "->>,>>>,>>9.99" TO 50
      SKIP
      "Overpayments: "     AT 10
      iOverPaym FORMAT "->>,>>>,>>9.99" TO 50
      SKIP(1).
   ASSIGN rl = rl + 4.

END FUNCTION.

ASSIGN sl = 1
       rl = 0
       xDateHeader = "".

/* overpayments AND advance payments */
FOR EACH InvGroup NO-LOCK WHERE
         InvGroup.Brand     = gcBrand  AND
         InvGroup.InvGroup >= iInvGrp1 AND
         InvGroup.InvGroup <= iInvGrp2,
EACH Customer NO-LOCK WHERE
     Customer.InvGroup = InvGroup.InvGroup:

     FIND wBal WHERE wBal.InvGrp = InvGroup.InvGroup NO-ERROR.
     IF NOT AVAILABLE wBal THEN DO:
        CREATE wBal.
        ASSIGN wBal.InvGrp = InvGroup.InvGroup
               wBal.Name   = InvGroup.IGName.
    END.

    ASSIGN wBal.Deposit  = wBal.Deposit  + 
                              fGetCustBal(Customer.CustNum,"TOTAL","DP")
           wBal.OverPaym = wBal.OverPaym + 
                              fGetCustBal(Customer.CustNum,"TOTAL","OP")
           wBal.AdvPaym  = wBal.AdvPaym  + 
                              fGetCustBal(Customer.CustNum,"TOTAL","AP")
           xQty      = xQty + 1.

    IF xQty MOD 100 = 0  AND
       SESSION:BATCH = FALSE
    THEN DO:
        PAUSE 0.
        DISPLAY "Inv.grp:" InvGroup.InvGroup SKIP
                "Cust ..:" Customer.CustNum  SKIP
                "Qty ...:" xQty FORMAT ">>,>>>,>>9"
        WITH NO-LABELS OVERLAY ROW 12 CENTERED TITLE " Collecting "
        FRAME fQty.
    END.

END.

HIDE FRAME fQty NO-PAUSE. 

ASSIGN xQty = 0. 

/* unregistered payments */
FOR EACH UnregPaym NO-LOCK WHERE
    UnregPaym.Brand = gcBrand AND
    UnregPaym.State = 0: /* AND 
    UnregPaym.AccDate LE iDate2:  */

    FIND wUnreg WHERE 
         wUnreg.BankAcc = UnregPaym.BankAcc NO-ERROR.
    IF NOT AVAILABLE wUnreg THEN DO:
       CREATE wUnreg.
       ASSIGN wUnreg.BankAcc = UnregPaym.BankAcc.
    END.

    ASSIGN wUnreg.Amount = wUnreg.Amount + UnregPaym.PaidAmt - UnregPaym.Booked
           xQty          = xQty + 1.

    IF xQty MOD 100  = 0 AND
       SESSION:BATCH = FALSE
    THEN DO:
        PAUSE 0.
        DISPLAY "Unregistered paym." SKIP
                "Qty :" xQty  
        WITH NO-LABELS OVERLAY ROW 12 CENTERED TITLE " Collecting "
        FRAME fQty1.
    END.

END.

ASSIGN sl = 1. 
VIEW STREAM tul FRAME sivuotsi.
ASSIGN rl = 5.

/* balances BY invoicing group */
FOR EACH wBal:

   CheckPage(4).
   PUT STREAM tul UNFORMATTED
      wBal.InvGrp AT 5
      " "
      wBal.Name
      SKIP.
   ASSIGN rl = rl + 1.

   fPrintBal(wBal.Deposit,
             wBal.AdvPaym,
             wBal.OverPaym).

   ACCUMULATE wBal.Deposit  (TOTAL)
              wBal.AdvPaym  (TOTAL)
              wBal.OverPaym (TOTAL).
END.

/* total balances (only IF more than 1 group) */
IF iInvGrp1 NE iInvGrp2 THEN DO:
   CheckPage(4).
   PUT STREAM tul UNFORMATTED
      "Total" AT 5
      SKIP.
   ASSIGN rl = rl + 1.

   fPrintBal((ACCUM TOTAL wBal.Deposit),
             (ACCUM TOTAL wBal.AdvPaym),
             (ACCUM TOTAL wBal.OverPaym)).
END.

/* unregistered payments BY bank account */
CheckPage(2).
PUT STREAM tul SKIP(1)      
    "Unregistered payments: " AT 5 SKIP.
ASSIGN rl = rl + 1.
FOR EACH wUnreg:
    CheckPage(0).
    PUT STREAM tul
        wUnreg.BankAcc AT 7  FORMAT "X(24)"
        wUnreg.Amount  TO 45 FORMAT "->>,>>>,>>9.99"
        SKIP.
    ASSIGN rl = rl + 1. 

    ACCUMULATE wUnreg.Amount (TOTAL).
END.

CheckPage(1).
PUT STREAM tul 
    FILL("-",41) AT 5 FORMAT "X(41)" 
    SKIP
    "Total" AT 5 
    (ACCUM TOTAL wUnreg.Amount) TO 45 FORMAT "->>,>>>,>>9.99"
    SKIP.
ASSIGN rl = rl + 2.

{uprfeed.i rl}

ASSIGN SESSION:NUMERIC-FORMAT = xSessionNum.


