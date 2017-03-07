/* inv_sum.p        10.01.2001/aam
   calculate invoice sums 

            07.06.02/aam use Invoice.OverPaym FOR overpayment
            01.08.02/aam invoice can include lines both with and without VATAmt
            26.09.02/aam customer balances in table CustBal
            01.10.02/aam use fInvoiceAmt()
            12.09.03/aam brand

*/


{Syst/commali.i}
{Func/fcustbal.i}
{Func/finvamt.i}

DEF INPUT PARAMETER iInv AS INT NO-UNDO.

DEF BUFFER blasku FOR Invoice.

DEF VAR punetto1 AS DEC NO-UNDO.


FOR FIRST blasku no-lock where
    blasku.InvNum = iInv,

   FIRST Customer of blasku no-lock,

   FIRST PriceList no-lock where
         Price.Brand         = Customer.Brand  AND
         PriceList.PriceList = Customer.PriceList.

   FIND Invoice where Invoice.InvNum = blasku.InvNum exclusive-lock.

   IF Invoice.CrInvNum NE 0 THEN DO:

      MESSAGE 
         "Invoice " + string(Invoice.InvNum) + 
         " is credited, updating not allowed !"
      VIEW-AS ALERT-BOX ERROR.

      RETURN.
   END.

   /* customer's balance, first reduce the original amount */
   fCustBal(Customer.CustNum,
            "",
            "ARBAL",
            -1 * Invoice.InvAmt). 

   fInvoiceAmt(BUFFER Invoice). 

   /* customer's balance, add the new amount  */
   fCustBal(Customer.CustNum,
            "",
            "ARBAL",
            Invoice.InvAmt). 

END.
