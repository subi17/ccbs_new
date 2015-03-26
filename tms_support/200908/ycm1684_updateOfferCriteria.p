
/* run it first for included values and after thatn in excluded values */

DEFINE VARIABLE lcValues AS CHARACTER NO-UNDO. 

/* look for all Offer Criteria that include Renewal_POS order channle 
   and add the new order channel */

FOR EACH OfferCriteria EXCLUSIVE-LOCK WHERE
         OfferCriteria.Brand = "1" AND
         OfferCriteria.CriteriaType = "OrderChannel" AND
         LOOKUP("Renewal_POS",OfferCriteria.IncludedValue) > 0:
     
         lcValues = OfferCriteria.IncludedValue.
         lcValues = lcValues + ",Renewal_POS_STC".

        
         ASSIGN OfferCriteria.IncludedValue = lcValues.

         DISPLAY OfferCriteria.Offer 
                 OfferCriteria.IncludedValue FORMAT "x(40)".
                 

END.

/* look for all Offer Criteria that exclude  Renewal_POS order channle 
   and add the new order channel */
/*
FOR EACH OfferCriteria EXCLUSIVE-LOCK WHERE
         OfferCriteria.Brand = "1" AND
         OfferCriteria.CriteriaType = "OrderChannel" AND
         LOOKUP("Renewal_POS",OfferCriteria.ExcludedValue) > 0:

         lcValues = OfferCriteria.ExcludedValue.
         lcValues = lcValues + ",Renewal_POS_STC".

         ASSIGN OfferCriteria.ExcludedValue = lcValues.

         DISPLAY OfferCriteria.Offer 
                 OfferCriteria.ExcludedValue FORMAT "x(40)".
END.
*/
              

