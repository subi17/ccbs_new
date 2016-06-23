/**
 * Get Discount Plan.
 *
 * @input  ids;array of string;mandatory;discount plan rule ids
 * @output billitem;array of struct;discount plan information

 */

{newton/src/header_get.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray,STRING(liCounter)).
   
   FIND FIRST DiscountPlan NO-LOCK WHERE 
              DiscountPlan.Brand = gcBrand AND 
              DiscountPlan.DPRuleId = pcId NO-ERROR.

   IF NOT AVAIL DiscountPlan THEN RETURN
      appl_err("Discount Plan not found: " + pcId).

   FIND FIRST DPRate WHERE
              DPRate.DPId       = DiscountPlan.DPId AND
              DPRate.ValidFrom <= TODAY AND
              DPRate.ValidTo   >= TODAY NO-LOCK NO-ERROR.

   lcResultStruct = add_struct(resp_array, "").

   add_string(lcResultStruct,"id",DiscountPlan.DPRuleId).
   add_string(lcResultStruct,"name",DiscountPlan.DPName).
   add_string(lcResultStruct,"unit",DiscountPlan.DPUnit).
   add_int(lcResultStruct,"valid_periods",DiscountPlan.ValidPeriods).
   add_int(lcResultStruct,"cc_display",DiscountPlan.CCDisplay).
   add_datetime(lcResultStruct,"valid_from",DiscountPlan.ValidFrom).
   add_datetime(lcResultStruct,"valid_to",DiscountPlan.ValidTo).

   IF AVAILABLE DPRate THEN
      add_double(lcResultStruct,"amount",DPRate.DiscValue).
   ELSE
      add_double(lcResultStruct,"amount",0.0).
END.

