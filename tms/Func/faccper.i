/* faccper.i            10.09.02/aam
   get the accounting period
   check if period is locked 

                        09.09.03/aam brand
*/

&IF "{&FACCPER_I}" NE "YES"
&THEN

&GLOBAL-DEFINE FACCPER_I YES

DEF VAR liAPeriod AS INT NO-UNDO.

FUNCTION fAccPeriod RETURNS INTEGER
   (iDate AS DATE).

   liAPeriod = 0. 

   FOR FIRST AccPeriod NO-LOCK WHERE
      AccPeriod.Brand     = gcBrand AND
      AccPeriod.FromDate <= iDate   AND
      AccPeriod.ToDate   >= iDate:

      liAPeriod = AccPeriod.Period.
   END. 

   RETURN liAPeriod.

END FUNCTION.


FUNCTION fPeriodLocked RETURNS LOGICAL
   (iDate AS DATE,
    iMsg  AS LOGIC).

   liAPeriod = 0. 

   FOR FIRST AccPeriod NO-LOCK WHERE
      AccPeriod.Brand     = gcBrand AND
      AccPeriod.FromDate <= iDate   AND
      AccPeriod.ToDate   >= iDate:

      liAPeriod = IF AccPeriod.PerLocked 
                  THEN 1
                  ELSE 0. 
   END. 

   /* notify user */
   IF liAPeriod = 1 AND iMsg THEN DO:
      MESSAGE "Period for" STRING(iDate,"99.99.9999") "is locked." SKIP 
              "No new events can be posted or old events changed"  SKIP
              "on this period before releasing it."
      VIEW-AS ALERT-BOX
      ERROR
      TITLE " Accounting period ". 
   END. 

   RETURN (liAPeriod = 1). 

END FUNCTION.

&ENDIF
