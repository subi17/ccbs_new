/* fclvat.i        22.11.02/aam 
   vat handling for credit loss postings 
                   08.05.03/aam take only proportion of invoice's vat if 
                                advpaym/overpaym has been used on it
                   04.09.06/aam better handling when vat is 0              
                   04.10.06/aam round the result
*/

DEF VAR ldCLVat   AS DEC NO-UNDO. 
DEF VAR liCLCount AS INT NO-UNDO.

FUNCTION fCLVat RETURNS DECIMAL
   (idPaid          AS DEC,
    BUFFER bInvoice FOR Invoice,
    OUTPUT oiVatAcc AS INT). 

   ASSIGN ldCLVat  = 0
          oiVatAcc = 0.

   /* calculate total vat */
   DO liCLCount = 1 TO 5:
      ldCLVat = ldCLVat + bInvoice.VatAmount[liCLCount].

      IF bInvoice.VatAmount[liCLCount] NE 0 AND
         oiVatAcc = 0 
      THEN oiVatAcc = bInvoice.VatAccount[liCLCount].
   END.

   /* nothing to do */ 
   IF ldCLVat = 0 THEN RETURN ldCLVat.
   
   /* if adv.payment or overpayment has been used then invoice's open 
      balance is not equal to vat's basis amount */
   IF bInvoice.AmtExclVat + ldCLVat NE bInvoice.InvAmt 
   THEN ldCLVat = ldCLVat * bInvoice.InvAmt / (bInvoice.AmtExclVat + ldCLVat).

   /* calculate proportional amount (invoice amount shouldn't be 0 if 
      a payment is being made, but check it just to be sure) */
   IF bInvoice.InvAmt NE 0 THEN 
      ldCLVat = ldCLVat * idPaid / bInvoice.InvAmt.

   RETURN ROUND(ldCLVat,2).

END.    
