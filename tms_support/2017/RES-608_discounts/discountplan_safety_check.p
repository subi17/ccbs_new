/*Checking that billcode is unique.*/
DEF BUFFER bDiscountPlan1 FOR DiscountPlan.
DEF BUFFER bDiscountPlan2 FOR DiscountPlan.

FOR EACH bDiscountPlan1 NO-LOCK :
   FIND FIRST bDiscountPlan2 NO-LOCK WHERE 
              bDiscountPlan2.billcode eq bDiscountPlan1.billcode AND
              bDiscountPlan2.dpid ne bDiscountPlan1.dpid NO-ERROR.
   IF AVAIL bDiscountPlan2 THEN DO:
      /*message bDiscountPlan2.billcode VIEW-AS ALERT-BOX.*/
      disp bDiscountPlan2.
   END.

END.
