/* fclbal.i         22.05.03/aam

   count open balance that is checked against credit limit 
*/


FUNCTION fCreditLimitOpenBal RETURNS DECIMAL
   (BUFFER bCLCustomer FOR Customer,
    INPUT  idtDate     AS DATE,
    INPUT  icType      AS CHAR,
    OUTPUT odLastAlarm AS DEC).

   DEF VAR ldCLBal AS DEC NO-UNDO.
   DEF VAR ldBal   AS DEC NO-UNDO.
   DEF VAR ldProp  AS DEC NO-UNDO. 

   /* billed, unpaid events */
   FOR EACH Invoice OF bCLCustomer NO-LOCK WHERE
            Invoice.InvAmt > 0:

      ldBal = fInvBal(BUFFER Invoice,
                      idtDate).

      IF ldBal <= 0 THEN NEXT. 

      /* if partly paid then take according proportion of row amounts */
      ldProp = MIN(1,ldBal / Invoice.InvAmt).

      FOR EACH InvRow OF Invoice NO-LOCK WHERE
         /* gsm-calls, minimum fees, gprs */
         LOOKUP(STRING(InvRow.RowType),"2,5,6") > 0:

          ldCLBal = ldCLBal + 
                        InvRow.Amt * ldProp *
                        /* amount with VAT */
                        (IF NOT Invoice.VatIncl
                         THEN 1 + InvRow.VatPerc / 100
                         ELSE 1).
      END.

   END.         

   odLastAlarm = 0.

   RETURN ldCLBal.

END FUNCTION.    


/* customer's credit limit */
FUNCTION fCustCreditLimit RETURNS DECIMAL
   (idtDate AS DATE).

   DEF VAR ldCreditLimit AS DEC NO-UNDO.

   ldCreditLimit = Customer.CreditLimit.

   /* temporary increase of credit limit (always take newest; dto is desc) */
   FOR FIRST AddCustLimit OF Customer NO-LOCK WHERE
             AddCustLimit.DTo   >= idtDate:

      ldCreditLimit = AddCustLimit.Amount.         

   END.

   RETURN ldCreditLimit.

END FUNCTION.

