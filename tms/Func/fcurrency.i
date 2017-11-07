/* --------------------------------------------------------------------------
  MODULE .......: FCURRENCY.I
  FUNCTION .....: Currency related functions
  APPLICATION ..: TMS
  AUTHOR .......: AAM
  CREATED ......: 14.10.02 aam
  MODIFIED .....: 26.02.03/aam fCurrUnit added
                  26.06.03 kl  ilCurrUnit from Tariff directly
                  09.09.03/aam brand
                  03.04.07/aam RateDate changed to descending index

---------------------------------------------------------------------------- */


DEF VAR lcHomeCurr     AS CHAR  NO-UNDO. 
DEF VAR liExchRateMet  AS INT   NO-UNDO. 

/* home currency */
{Func/tmsparam.i DefCurrency RETURN} lcHomeCurr = TMSParam.CharVal.

/* should rates be multiplied or divided with exchange rate */
{Func/tmsparam.i ExchRateMet}  
IF AVAILABLE TMSParam THEN liExchRateMet = TMSParam.IntVal.


/* get currency exchange rate */
FUNCTION fCurrRate RETURNS DECIMAL
   (iCurrency AS CHAR,
    iDate     AS DATE).

   DEF VAR ldCurrRate AS DEC NO-UNDO.

   ldCurrRate = 1. 

   /*  currency is not home currency */
   IF iCurrency NE lcHomeCurr AND iCurrency NE "" THEN DO:

      FIND FIRST CurRate where
                 CurRate.Currency  = iCurrency AND
                 CurRate.RateDate <= iDate
      NO-LOCK NO-ERROR.

      IF AVAIL CurRate THEN ldCurrRate = CurRate.ExchRate.

   END.

   RETURN ldCurrRate.

END FUNCTION.


/* convert amount to home currency */
FUNCTION fToHomeCurr RETURNS DECIMAL
   (iAmount AS DEC,
    iRate   AS DEC).

   IF iRate = 0 THEN RETURN iAmount.

   /* multiplied or divided */
   IF liExchRateMet = 1 THEN RETURN iAmount / iRate.
   ELSE RETURN iAmount * iRate.

END FUNCTION.

/* convert amount to foreign currency */
FUNCTION fToForCurr RETURNS DECIMAL 
   (iAmount AS DEC,
    iRate   AS DEC).

   IF iRate = 0 THEN RETURN iAmount.

   /* multiplied or divided */
   IF liExchRateMet = 1 THEN RETURN iAmount * iRate.
   ELSE RETURN iAmount / iRate.

END FUNCTION.

/* is currency unit full or sub */
FUNCTION fCurrUnit RETURNS LOGICAL
   (idNet          AS DEC,
    idGross        AS DEC,
    ilCurrUnit     AS LOGIC,
    icPList        AS CHAR,
    iiTariff       AS INT,
    icBrand        AS CHAR,
    OUTPUT odNet   AS DEC,
    OUTPUT odGross AS DEC).

   ASSIGN odNet   = idNet
          odGross = idGross.

   IF ilCurrUnit = ? THEN DO:
      /* if pricelist is given then use it directly, otherwise get it
         through tariff */
      IF icPList = "" THEN DO:    
         FIND FIRST Tariff WHERE
                    Tariff.Brand     = icBrand AND
                    Tariff.TariffNum = iiTariff
         NO-LOCK NO-ERROR.
         /* this doesn't work on prices with mere customer nbr */              
         IF AVAILABLE Tariff THEN ilCurrUnit = Tariff.CurrUnit.
      END.
      ELSE DO:
         FIND FIRST PriceList WHERE
              PriceList.Brand     = icBrand AND
              PriceList.PriceList = icPList
         NO-LOCK NO-ERROR.
         IF AVAILABLE PriceList THEN ilCurrUnit = PriceList.CurrUnit.
         ELSE                        ilCurrUnit = FALSE. 
      END.
   END.

   IF ilCurrUnit = FALSE THEN ASSIGN 
      odNet   = odNet / 100
      odGross = odGross / 100. 

   RETURN ilCurrUnit.

END FUNCTION.    



