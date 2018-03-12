/**
 * Get Discount Plan.
 *
 * @input  ids;array of string;mandatory;discount plan rule ids
 * @output discountplan;array of struct;discount plan information
 * @discountplan  plan_id;int;dpid of discountplan
                  id;string;dpruleid and brand pipe separated
                  brand;string;Brand
                  name;string;DPName
                  unit;string;DPUnit
                  valid_periods;int;Valid periods
                  cc_display;int;CC display
                  valid_from;date;Valid from date
                  valid_to;date;Valid to date
                  amount;double;Amount
                  priority;int;priority
                  category;string;DPCategory
                  commercial_names;array of struct;commercial name information
   @commercial_names  language;string;ISO 639-1 code for the language
                      name;string;commerical name
 */

{newton/src/header_get.i}
{Func/multitenantfunc.i}

DEFINE VARIABLE lcCommercialNamesArray AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCommercialNameStruct AS CHARACTER NO-UNDO.

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray,STRING(liCounter)).
   
   IF NUM-ENTRIES(pcId,"|") > 1 THEN
      ASSIGN
          pcTenant = ENTRY(2,pcId,"|")
          pcId     = ENTRY(1,pcId,"|").
   ELSE
      RETURN appl_err("Invalid tenant information").

   {newton/src/settenant.i pcTenant}

   FIND FIRST DiscountPlan NO-LOCK WHERE 
              DiscountPlan.Brand = Syst.Var:gcBrand AND 
              DiscountPlan.DPRuleId = pcId NO-ERROR.

   IF NOT AVAIL DiscountPlan THEN RETURN
      appl_err("Discount Plan not found: " + pcId).

   FIND FIRST DPRate WHERE
              DPRate.DPId       = DiscountPlan.DPId AND
              DPRate.ValidFrom <= TODAY AND
              DPRate.ValidTo   >= TODAY NO-LOCK NO-ERROR.

   lcResultStruct = add_struct(resp_array, "").

   add_int(lcResultStruct,"plan_id",DiscountPlan.DPID).
   add_string(lcResultStruct,"id",DiscountPlan.DPRuleId + "|" + 
                                  fConvertTenantToBrand(pcTenant)).
   add_string(lcResultStruct,"brand",fConvertTenantToBrand(pcTenant)).
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

   add_int(lcResultStruct,"priority",DiscountPlan.Priority).
   add_string(lcResultStruct,"category",DiscountPlan.DPCategory).

   IF CAN-FIND(FIRST RepText NO-LOCK WHERE
                  RepText.Brand     = Syst.Var:gcBrand        AND
                  RepText.LinkCode  = DiscountPlan.DPRuleId   AND
                  RepText.TextType  = {&REPTEXT_DISCOUNTPLAN} AND
                  RepText.ToDate   >= TODAY                   AND
                  RepText.FromDate <= TODAY)
   THEN DO:
      lcCommercialNamesArray = add_array(lcResultStruct, "commercial_names").
      FOR EACH RepText NO-LOCK WHERE
            RepText.Brand     = Syst.Var:gcBrand        AND
            RepText.LinkCode  = DiscountPlan.DPRuleId   AND
            RepText.TextType  = {&REPTEXT_DISCOUNTPLAN} AND
            RepText.ToDate   >= TODAY                   AND
            RepText.FromDate <= TODAY,
          Language NO-LOCK WHERE Language.Language = RepText.Language:
         lcCommercialNameStruct = add_struct(lcCommercialNamesArray, "").
         add_string(lcCommercialNameStruct, "language", Language.LanguageCode).
         add_string(lcCommercialNameStruct, "name", RepText.RepText).
      END.
   END.
END.

