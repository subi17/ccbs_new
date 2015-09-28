&IF "{&PREPAIDFEE_I}" NE "YES"
&THEN

&GLOBAL-DEFINE PREPAIDFEE_I YES
{commali.i}

FUNCTION fgetPrepaidFeeAmount RETURNS DECIMAL 
         (INPUT icDCEvent       AS CHAR,
          INPUT idDate          AS DATE):

   DEFINE VARIABLE ldAmount AS DECIMAL NO-UNDO INITIAL 0.
   DEF BUFFER DayCampaign FOR DayCampaign.
   DEF BUFFER FMItem FOR FMItem.

   FOR FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = icDCEvent,
      FIRST FMItem NO-LOCK WHERE
            FMItem.Brand     = gcBrand AND
            FMItem.FeeModel  = DayCampaign.FeeModel  AND
            FMItem.ToDate   >= idDate AND
            FMItem.FromDate <= idDate:
       ldAmount = FMItem.Amount.   
   END.

   RETURN ldAmount.

END FUNCTION.

&ENDIF
