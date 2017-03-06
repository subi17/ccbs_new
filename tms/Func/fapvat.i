/* fapvat.i     07.06.02/aam 
   handling of VAT FOR advance payments
*/

{Syst/commali.i}
{Func/ftaxdata.i}

DEF VAR liAPVatAcc     AS INT  NO-UNDO. 
DEF VAR ldeAPVatPerc   AS DEC  NO-UNDO. 
DEF VAR liAPVatDebtAcc AS INT  NO-UNDO. 

/* definitions FOR assisting account AND vatcode */
{Func/tmsparam.i AdvPaymVatAcc  RETURN}. 
IF AVAILABLE TMSParam THEN liAPVatAcc = TMSParam.IntVal.


FUNCTION fAPVatPerc RETURNS LOGICAL
   (icRegion AS CHAR,
    idaDate AS DATE):
 
   DEF VAR liVatCode AS INT NO-UNDO.
   /* get appropiate vatcode according to customer's taxzone */
   liVatCode = fRegionTaxCode(icRegion,"1", idaDate). 
      
   FIND VatCode WHERE VatCode.VatCode = liVatCode NO-LOCK NO-ERROR.
   IF AVAILABLE VatCode THEN ASSIGN
      liAPVatDebtAcc = VatCode.AccNum
      ldeAPVatPerc   = VatCode.VatPerc.
   ELSE ASSIGN 
      liAPVatDebtAcc = 0
      ldeAPVatPerc   = 0.
 
END.

/* VAT of paid advance payment */
FUNCTION fAPVatAmt RETURNS DECIMAL
    (icRegion  AS CHAR,
     idAdvPaym AS DEC,
     idaDate AS DATE).

   fAPVatPerc(icRegion, idaDate).
   
   RETURN ROUND(idAdvPaym * ldeAPVatPerc / (100 + ldeAPVatPerc),2).

END FUNCTION. 


