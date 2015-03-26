/*-----------------------------------------------------------------------------
  MODULE .......: CUSTBALREP.P
  FUNCTION .....: Report for customer's open invoices, payments and
                  interests
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 14.08.02
  MODIFIED .....: 26.09.02/aam customer balances in table CustBal 
                  13.11.02 lp definition's view changed
                  12.09.03/aam brand,
                               explanation of report etc.
                  12.02.04/aam logic separated to custbalrep,
                               take also paid invoices
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
{utumaa.i}
{fcustbal.i}


DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.
DEF INPUT PARAMETER idtFrom   AS DATE NO-UNDO. 

DEF VAR sl       AS I  NO-UNDO.
DEF VAR rl       AS I  NO-UNDO.
DEF VAR rlx      AS I  NO-UNDO.
DEF VAR xSesNum  AS CH NO-UNDO.
DEF VAR lPaid    AS DE NO-UNDO.
DEF VAR lDebt    AS DE NO-UNDO.
DEF VAR lDue     AS DE NO-UNDO.
DEF VAR lTotVal  AS DE NO-UNDO.
DEF VAR lTotPaid AS DE NO-UNDO.
DEF VAR lTotDue  AS DE NO-UNDO.
DEF VAR lTotDebt AS DE NO-UNDO.
DEF VAR lPaym    AS DE NO-UNDO.
DEF VAR lClaim   AS C  NO-UNDO.
DEF VAR i        AS I  NO-UNDO.
DEF VAR DueTul   AS LO NO-UNDO.

DEF VAR line1 AS CH NO-UNDO format "x(78)".
DEF VAR line2 LIKE line1 NO-UNDO.
DEF VAR line3 LIKE line1 NO-UNDO.

DEF VAR ldCustDP AS DEC NO-UNDO. 
DEF VAR ldCustOP AS DEC NO-UNDO.
DEF VAR ldCustAP AS DEC NO-UNDO. 

FUNCTION CheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF rl >= skayt1 - iAddLine THEN DO:
        {uprfeed.i rl}
        ASSIGN rlx = 0
               sl = sl + 1.
        view STREAM tul FRAME pagehead.  
        ASSIGN rl = 6.
    END.

    RETURN TRUE.
END.

FIND FIRST Customer WHERE 
           Customer.Brand   = gcBrand AND
           Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN.           

ASSIGN 
   line1 = fill("=",78)
   line2 = fill("-",78)
   line3 = line1
   xSesNum = SESSION:NUMERIC-FORMAT
   SESSION:NUMERIC-FORMAT = "European"
   sl = 1
   rl = 0.

FORM header
   line1 AT 1 SKIP
   ynimi at 1 format "x(28)" 
   "Customer Balance Report" AT 30
   "Page" AT 68  
   sl format "ZZZZ9" SKIP
   "Customer" AT 1
   Customer.Custnum AT 10 
   pvm format "99.99.9999" AT 68 SKIP
   line3 AT 1 SKIP(1)
WITH width 80 NO-LABEL no-box FRAME pagehead.

IF NOT CAN-FIND(FIRST Invoice OF Customer) THEN DO:
   MESSAGE "Nothing to print !" VIEW-AS ALERT-BOX.
   RETURN.
END.

VIEW STREAM tul FRAME pagehead.
rl = rl + 6.

PUT STREAM tul UNFORMATTED
   Customer.Custname AT 1 SKIP
   Customer.Address SKIP
   Customer.Zipcode " " Customer.Postoffice
   SKIP(1)
   "INVOICES" skip(1)
   "InvNum" AT 3 "InvDate" AT 12 "DueDate" AT 22
   "Value" AT 38 "Paid" AT 51 "Debt" AT 62 "Due" AT 75 SKIP
   line2 SKIP.
rl = rl + 7.

FOR EACH Invoice NO-LOCK WHERE
         Invoice.Brand   = gcBrand AND
         Invoice.CustNum = Customer.CustNum:

   RUN invbal(INPUT Invoice.InvNum, OUTPUT lDebt ).
   IF lDebt = 0 AND Invoice.InvDate < idtFrom THEN NEXT. 

   lPaid = Invoice.InvAmt - lDebt.

   CheckPage(3).

   PUT STREAM tul UNFORMATTED
      Invoice.InvNum AT 3  
      Invoice.InvDate AT 12
      Invoice.DueDate AT 22
      Invoice.InvAmt TO 42 format "->>>,>>9.99"
      lPaid TO 54 format "->>>,>>9.99".
   ASSIGN
      rl = rl + 1.

   IF Invoice.DueDate < pvm THEN
      lDue = Invoice.Invamt - lPaid.
   ELSE lDue = 0.   

   PUT STREAM tul UNFORMATTED
      lDebt TO 65 format "->>>,>>9.99"
      lDue TO 77 FORMAT "->>>,>>9.99" SKIP.

   FOR EACH Payment OF Invoice NO-LOCK:
      lPaym = 0.       
      DO i = 1 to 10:
         if Payment.AccType[i] = 1 THEN
            lPaym = lPaym - Payment.Posting[i].
      END.      

      IF lPaym NE 0 THEN DO:

         CheckPage(1).

         PUT STREAM tul UNFORMATTED
            "payment" AT 5
            Payment.PaymDate AT 14 
            lPaym            TO 33 format "->>>,>>9.99"
            SKIP.
         ASSIGN
            rl = rl + 1.
      END.   
   END.

   FOR EACH ClaimHist OF Invoice NO-LOCK:

      CheckPage(1).

      IF ClaimHist.Claim = 1 THEN lClaim = "remind".
      ELSE lClaim = "claim".

      PUT STREAM tul UNFORMATTED
         lClaim AT 5
         ClaimHist.ClaimDate AT 14 
         ClaimHist.ClaimAmt  TO 33 format "->>>,>>9.99"
         SKIP.                   
      rl = rl + 1.
   END.


   ASSIGN 
      lTotDebt = lTotDebt + lDebt
      lTotPaid = lTotPaid + lPaid
      lTotVal  = lTotVal  + Invoice.InvAmt.
   IF Invoice.DueDate < pvm THEN lTotDue = lTotDue + lDue.

   PUT STREAM tul UNFORMATTED SKIP(1).

END. /* EACH Invoice */

CheckPage(7).

ASSIGN ldCustDP = fGetCustBal(Customer.CustNum,"TOTAL","DP")
       ldCustOP = fGetCustBal(Customer.CustNum,"TOTAL","OP")
       ldCustAP = fGetCustBal(Customer.CustNum,"TOTAL","AP").

PUT STREAM tul UNFORMATTED 
   line2 SKIP 
   "TOTAL" AT 3
   lTotVal
   FORMAT "->>>,>>9.99" TO 42 
   lTotPaid TO 54 FORMAT "->>>,>>9.99" 
   lTotDebt TO 65 FORMAT "->>>,>>9.99"
   lTotDue  TO 77 FORMAT "->>>,>>9.99" SKIP
   skip(1)

   "BALANCES" SKIP(1)

   "Deposit....................:" AT 3 
   ldCustDP  TO 65 FORMAT "->>>,>>9.99"
   SKIP

   "Overpayment................:" AT 3
   ldCustOP  TO 65 FORMAT "->>>,>>9.99"
   SKIP

   "Advance payment............:" AT 3 
   ldCustAP  TO 65 FORMAT "->>>,>>9.99" 
   SKIP

   "Debt with balances reduced.:" AT 3 
   (lTotDebt - ldCustAP - ldCustOP)
   TO 65 FORMAT "->>>,>>9.99"
   SKIP(1).
rl = rl + 7.

FOR EACH CustIntEvent of Customer NO-LOCK
BREAK BY CustIntEvent.PaymDate:

   IF FIRST(CustIntEvent.PaymDate) THEN DO:
      CheckPage(6).

      PUT STREAM tul 
         SKIP "INTERESTS" 
         SKIP(1)
         "InvNum" AT 3
         "PaymDate" AT 12 
         "PaidAmt" AT 26
         "Interest" AT 40 SKIP
         line2 SKIP.

      rl = rl + 5.
   END.

   CheckPage(1).

   PUT STREAM tul UNFORMATTED
      CustIntEvent.InvNum AT 3
      CustIntEvent.PaymDate AT 12
      CustIntEvent.PaidAmt  TO 32 FORMAT "->>>,>>9.99"
      CustIntEvent.Amt TO 47 FORMAT "->>>,>>9.99" 
      SKIP.
   rl = rl + 1.
   ACCUMULATE CustIntEvent.PaidAmt (TOTAL).
   ACCUMULATE CustIntEvent.Amt     (TOTAL).

   IF LAST(CustIntEvent.PaymDate) THEN DO:
      PUT STREAM tul UNFORMATTED
         line2 SKIP
         "TOTAL" AT 3
        (ACCUM TOTAL CustIntEvent.PaidAmt) TO 32 FORMAT "->>>,>>9.99"
        (ACCUM TOTAL CustIntEvent.Amt)     TO 47 FORMAT "->>>,>>9.99" SKIP.
      rl = rl + 2.
   END.
   
END.

{uprfeed.i rl}
ASSIGN SESSION:NUMERIC-FORMAT = xSesNum.


