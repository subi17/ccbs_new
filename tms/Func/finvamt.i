/* finvamt.i        01.10.02/aam
   calculate CalcInv's total amounts 

                    15.10.02/aam currency handling
                    23.06.05/aam round total sum to 2 decimals
*/
{fcurrency.i}
{fapvat.i}

DEF TEMP-TABLE ttRowVat NO-UNDO
   FIELD VatPerc    AS DEC 
   FIELD VatCode    AS INT 
   FIELD VatAcc     AS INT
   FIELD Amount     AS DEC
   INDEX VatPerc VatPerc.

DEF VAR liCnt  AS INT NO-UNDO. 

FUNCTION fInvoiceAmt RETURNS LOGICAL
   (BUFFER CalcInv FOR Invoice). 

   DEF BUFFER CalcRow FOR InvRow. 

   EMPTY TEMP-TABLE ttRowVat. 

   FOR EACH CalcRow of CalcInv EXCLUSIVE-LOCK:

      /* row is with VAT */
      IF CalcRow.VatCode > 0 THEN DO:
         FIND VatCode NO-LOCK WHERE
              VatCode.VatCode = CalcRow.VatCode NO-ERROR.
         IF AVAILABLE VatCode THEN ASSIGN 
            CalcRow.VatPerc = VatCode.VatPerc. 
      END.

      /* amounts by vat percent */
      FIND FIRST ttRowVat WHERE
                 ttRowVat.VatPerc = CalcRow.VatPerc NO-ERROR.
      IF NOT AVAILABLE ttRowVat THEN DO:
         CREATE ttRowVat.
         ASSIGN ttRowVat.VatPerc = CalcRow.VatPerc
                ttRowVat.VatCode = CalcRow.VATCode. 
         IF CalcRow.VatCode > 0 AND AVAILABLE VatCode
         THEN ttRowVat.VatAcc = VatCode.AccNum. 
      END.

      ASSIGN ttRowVat.Amount = ttRowVat.Amount + CalcRow.Amt. 

   END.

   ASSIGN CalcInv.InvAmt     = 0
          CalcInv.AmtExclVat = 0
          CalcInv.VatAmt     = 0
          CalcInv.VatBasis   = 0
          CalcInv.VatPercent = 0 
          CalcInv.VatAmount  = 0
          CalcInv.VatAccount = 0
          liCnt              = 0.
          CalcInv.ExchRate   = fCurrRate(CalcInv.Currency,CalcInv.InvDate). 

   FOR EACH ttRowVat BY ttRowVat.VatPerc:

      ASSIGN liCnt                     = liCnt + 1
             CalcInv.VatBasis[liCnt]   = ttRowVat.Amount
             CalcInv.VatPercent[liCnt] = ttRowVat.VatPerc
             CalcInv.VatAccount[liCnt] = ttRowVat.VatAcc. 

      /* VATAmt included in prices */
      IF CalcInv.VatIncl THEN ASSIGN
         CalcInv.VatAmount[liCnt] = ROUND(ttRowVat.Amount * 
                                          ttRowVat.VatPerc /
                                         (100 + ttRowVat.VATPerc),2)       
         CalcInv.VATAmt           = CalcInv.VatAmt + CalcInv.VatAmount[liCnt]
         CalcInv.InvAmt           = CalcInv.InvAmt + ttRowVat.Amount
         CalcInv.AmtExclVat       = CalcInv.InvAmt - CalcInv.VatAMt.

      /* VATAmt excluded in prices */
      ELSE ASSIGN 
         CalcInv.AmtExclVAT       = CalcInv.AmtExclVat + ttRowVat.Amount
         CalcInv.VatAmount[liCnt] = ROUND(ttRowVat.Amount * 
                                          ttRowVat.VatPerc / 100,2)
         CalcInv.VATAmt           = CalcInv.VATAmt + CalcInv.VatAmount[liCnt]
         CalcInv.InvAmt           = CalcInv.AmtExclVAT + CalcInv.VATAmt.
   END.


   /* VAT handling FOR advance payment */
   IF CalcInv.AdvPaym NE 0 AND CalcInv.VATAmt NE 0 THEN DO:
      ASSIGN 
      liCnt                        = liCnt + 1
      CalcInv.VATAmount[liCnt]     = IF CalcInv.InvAmt = -1 * CalcInv.AdvPaym
                                     THEN CalcInv.VATAmt
                                     ELSE fAPVatAmt(CalcInv.Region,
                                                    -1 * CalcInv.AdvPaym,
                                                    CalcInv.Invdate)
      CalcInv.VATAccount[liCnt]    = liAPVatAcc
      CalcInv.VatPercent[liCnt]    = ldeAPVatPerc
      CalcInv.VatBasis[liCnt]      = -1 * CalcInv.AdvPaym

      /* last percent is the highest */
      CalcInv.VatBasis[liCnt - 1]  = CalcInv.VatBasis[liCnt - 1] -
                                     CalcInv.VatBasis[liCnt]
      CalcInv.VATAmount[liCnt - 1] = CalcInv.VATAmount[liCnt - 1] - 
                                     CalcInv.VATAmount[liCnt].
   END.                

   ASSIGN      
      CalcInv.Rounding   = 0
      CalcInv.InvAmt     = CalcInv.InvAmt + CalcInv.AdvPaym    + 
                                            CalcInv.OverPaym   +
                                            CalcInv.InterestAmt.

   /* total amount with 2 decimals */
   CalcInv.InvAmt = ROUND(CalcInv.InvAmt,2).

   /* amount in home currency */
   CalcInv.CurrAmt = fToHomeCurr(CalcInv.InvAmt,CalcInv.ExchRate). 

   RETURN TRUE. 

END FUNCTION.
