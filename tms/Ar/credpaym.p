/* credpaym.p      11.09.2001/aam
   make payments FOR debit AND credit invoice pairs 

             04.04.2002/aam UPDATE Invoice.PaymDate, Invoice.PaidAmt, 
                            Invoice.PaymState AND Payment.PaymSrc 
             10.04.2002/ht  ImportStamp added
             03.05.2002/aam use PaymVouch (fvoucher.i)
             17.05.2002/aam partial credit 
             03.06.2002/aam katun added TO memo
             26.09.2002/aam customer balances in table CustBal 
             23.10.2002/aam makepaym.p separated from this
             15.04.2003/aam makepaym returns voucher nbr
             26.09.2003/aam credit also negative invoices
             18.04.2006/aam update payment plan
*/   

{commali.i}
{tmsparam2.i}
{fpplan.i}

DEF INPUT PARAMETER iInvno AS INT  NO-UNDO.
DEF INPUT PARAMETER iDate  AS Date NO-UNDO.

DEF BUFFER blasku FOR Invoice.

DEF VAR xClearAcc AS INT NO-UNDO.
DEF VAR xDebit    AS dec NO-UNDO.
DEF VAR xcredit   AS dec NO-UNDO.
DEF VAR liRetry   AS INT NO-UNDO. 
DEF VAR liVoucher AS INT NO-UNDO. 

/* AccNum TO be used AS the clearance AccNum */
ASSIGN 
    xClearAcc = fCParamI("CreditPaymAcc").
IF xClearAcc = 0 THEN ASSIGN 
    xClearAcc = fCparamI("DiscAcc").

IF iDate = ? THEN ASSIGN iDate = TODAY.


/* credited invoice */
FOR FIRST Invoice exclusive-lock where  
    Invoice.InvNum = iInvno AND
    Invoice.CrInvNum > 0,
/* credit invoice */
FIRST blasku exclusive-lock where
    blasku.InvNum   = Invoice.CrInvNum AND
    blasku.CrInvNum = Invoice.InvNum:

    /* CURRENT balances */
    RUN invbal (Invoice.InvNum, OUTPUT xDebit).
    RUN invbal (blasku.InvNum, OUTPUT xcredit).

    /* Balance FOR debit invoice must be greater OR equal TO the 
       credit Balance */
    IF abs(xDebit) LT abs(xcredit) OR
       xDebit = 0 OR
       xcredit = 0
    THEN NEXT.

    /* IF partial credit THEN make also a partial payment TO debit invoice */
    IF abs(xcredit) LT abs(xDebit) THEN 
       xDebit = -1 * xcredit. 

    /* FIRST the debit invoice */
    DO liRetry = 1 TO 5:  /* in case of an undo error try again */

       RUN makepaym (BUFFER Invoice,
                     xDebit,
                     iDate,
                     xClearAcc,
                     "CR",
                     5,
                     FALSE,
                     FALSE,
                     "",
                     "Credited with invoice " +
                        string(blasku.InvNum) +
                        " Handler: " + katun,
                     OUTPUT liVoucher
                    ).

       /* succeeded */
       IF CAN-FIND(FIRST Payment OF Invoice WHERE
                         Payment.Voucher = liVoucher)
       THEN DO:
          
          /* check if invoice was part of a payment plan */
          fPaymPlanPaid(Invoice.CustNum,
                        Invoice.InvNum,
                        xDebit).

          LEAVE.
       END.    
    END. 


    /* THEN the credit invoice */
    DO liRetry = 1 TO 5:  /* in case of an undo error try again */

       RUN makepaym (BUFFER blasku,
                     xCredit,
                     iDate,
                     xClearAcc,
                     "CR",
                     5,
                     FALSE,
                     FALSE,
                     "",
                     "Credit for invoice " +
                        string(Invoice.InvNum) +
                        " Handler: " + katun,
                     OUTPUT liVoucher
                    ).

       /* succeeded */
       IF CAN-FIND(FIRST Payment OF blasku WHERE
                         Payment.Voucher = liVoucher)
       THEN LEAVE.
    END. 

END.




