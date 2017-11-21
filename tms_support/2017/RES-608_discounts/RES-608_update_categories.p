

FUNCTION fUpdateCategory RETURNS CHAR
   (icDPName AS CHAR,
    icCategory AS CHAR):
   FIND FIRST DiscountPlan NO-LOCK where
              DiscountPlan.DPName EQ icDPName NO-ERROR.
   IF NOT AVAIL DiscountPlan THEN RETURN "Discount Plan not found".
   DiscountPlan.Category = icCategory.
   RETURN "".

END.

