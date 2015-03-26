{commali.i}

FUNCTION fCopyRatePlan RETURN LOGICAL
  (INPUT icRatePlan  AS CHAR,
   INPUT icNewRatePlan AS CHAR).

   DEF BUFFER xxRatePlan FOR RatePlan.
   
   FOR EACH RatePlan WHERE 
            RatePlan.Brand    = "1"    AND 
            RatePlan.RatePlan = icRatePlan NO-LOCK .

      CREATE xxRatePlan.
      BUFFER-COPY RatePlan EXCEPT rateplan.rateplan TO xxRatePlan
      ASSIGN 
         xxRatePlan.RatePlan = icNewRatePlan
         xxRatePlan.Memo     = icRatePlan + " SÄÄSTÖHINNASTO " .   
   END.         
END.

FUNCTION fCopyPriceList RETURN LOGICAL
  (INPUT icPriceList AS CHAR,
   INPUT ilRatePlan  AS LOG ).

   DEF BUFFER xxPricelist for Pricelist.
   DEF BUFFER xxTariff    FOR Tariff.
  
   DEF VAR liDate AS INT NO-UNDO.
  
   liDate = Year(Today)  * 10000 + 
            Month(Today) * 100   + 
            Day(Today).

   FIND FIRST PriceList WHERE 
              PriceList.Brand     = gcBrand     AND 
              PriceList.PriceList = icPriceList NO-LOCK NO-ERROR.
              
   CREATE xxPriceList.
   BUFFER-COPY PriceList except Pricelist.pricelist to xxPricelist.
   ASSIGN 
      xxPriceList.Pricelist = icPriceList + "_" + STRING(liDate)
      xxPriceList.PlName    = icPricelist + " SÄÄSTÖSOPIMUS " +                                               "HINNASTO " + STRING(lidate) + " ALKAEN".

   IF ilRatePlan THEN fCopyRatePlan(icPriceList, xxPriceList.PriceList).
         
   FOR EACH Tariff no-lock where
            Tariff.Brand      = gcBrand   AND
            Tariff.PriceList  = icPriceList AND
            Tariff.ValidTO   >= Today     AND 
            Tariff.CustNum    = 0.

      CREATE xxTariff.
      BUFFER-COPY tariff except TariffNum TO xxTariff.
      ASSIGN
         xxTariff.TariffNum = next-value(Tariff)
         xxTariff.ValidFrom = Today
         xxTariff.PriceList = xxPriceList.Pricelist.
   END.
END.

