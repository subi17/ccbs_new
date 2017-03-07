/* fpaymconfig.i    08.01.08/aam 

   payment accounting rules 
*/

{Syst/commali.i}
{Func/cparam2.i}

FUNCTION fGetPaymentAccounts RETURNS LOGIC
   (iiPaymType AS INT,
    icPaymSrc  AS CHAR,
    idtAccDate AS DATE,
    icTaxZone  AS CHAR,
    OUTPUT oiPaymAcc AS INT EXTENT 10):
    
   DEF VAR liPaymCnt AS INT  NO-UNDO.
   DEF VAR lcConfSrc AS CHAR NO-UNDO.
  
   /* get default accounts according to payment type/source configuration */
   DO liPaymCnt = 1 TO 2:
   
      /* first try to find specific rule for payment source, then general
         with an empty source */
      CASE liPaymCnt:
      WHEN 1 THEN lcConfSrc = icPaymSrc.
      WHEN 2 THEN lcConfSrc = "".
      END CASE.
      
      FOR FIRST PaymConfig NO-LOCK WHERE
                PaymConfig.Brand     = gcBrand    AND
                PaymConfig.PaymType  = iiPaymType AND
                PaymConfig.PaymSrc   = lcConfSrc  AND
                PaymConfig.FromDate <= idtAccDate AND
                PaymConfig.ToDate   >= idtAccDate:
         
         ASSIGN 
            oiPaymAcc[1] = PaymConfig.DebitAccNum
            oiPaymAcc[2] = PaymConfig.CreditAccNum.
         
         /* tax account */
         IF PaymConfig.TaxRules THEN DO:
            
            FIND FIRST PaymConfTax WHERE
                       PaymConfTax.PaymConfig = PaymConfig.PaymConfig AND
                       PaymConfTax.TaxZone    = icTaxZone NO-LOCK NO-ERROR.
            IF AVAILABLE PaymConfTax THEN 
               oiPaymAcc[3] = PaymConfTax.TaxAccNum.
         END.

         /* if rule was found then use it even if it doesn't necessarily
            have both accounts defined */
         liPaymCnt = 3. 
      END.
         
   END.
      
   /* if there was no configuration available then use common defaults */
   IF oiPaymAcc[1] = 0 THEN    
   CASE iiPaymType: 
   WHEN 0 OR 
   WHEN 3 OR
   WHEN 4 OR
   WHEN 7 OR
   WHEN 8 THEN oiPaymAcc[1] = fCParamI("BankAcc").
   WHEN 1 THEN oiPaymAcc[1] = fCParamI("CreditLossAcc").
   WHEN 2 THEN oiPaymAcc[1] = fCParamI("CreditLossCancel").
   WHEN 5 THEN oiPaymAcc[1] = fCParamI("CreditPaymAcc").
   WHEN 6 THEN oiPaymAcc[1] = fCParamI("RefundBalAcc").
   END CASE.
   
   IF oiPaymAcc[2] = 0 THEN 
   CASE iiPaymType:
   WHEN 3 THEN oiPaymAcc[2] = fCParamI("ResDepositsAcc").
   WHEN 4 THEN oiPaymAcc[2] = fCParamI("AdvPaymAcc").
   WHEN 6 THEN oiPaymAcc[2] = fCParamI("BankAcc").
   END CASE.

END FUNCTION.


 
