/**
 * Get topup scheme.
 *
 * @input  ids;array of string;mandatory;topup scheme ids
 * @output topup_scheme;array of struct;topup scheme data
 * @topup_scheme id;string;topup scheme id
               amount;double;topup amount
               valid_from;double;
               valid_to;datetime;not returned if >= 19.01.2038 (http://en.wikipedia.org/wiki/Year_2038_problem)
               discount_amount;double;
               duration_unit;string;
               valid_from_rule;string;
               penalty_fee;string; dctype 3 = penalty fee ,  type 5 = monthly fee 
 */

{newton/src/header_get.i}
{Func/fcustpl.i}
{Func/multitenantfunc.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF NUM-ENTRIES(pcId,"|") > 1 THEN
      ASSIGN
          pcTenant = ENTRY(2,pcId,"|")
          pcId     = ENTRY(1,pcId,"|").
   ELSE
      RETURN appl_err("Invalid tenant information").

   {newton/src/settenant.i pcTenant}

   FIND DayCampaign NO-LOCK WHERE 
        DayCampaign.Brand = gcBrand AND 
        DayCampaign.DCEvent = pcId NO-ERROR.

   IF NOT AVAIL DayCampaign THEN RETURN appl_err("Periodical contract not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", DayCampaign.DCEvent + "|" + fConvertTenantToBrand(pcTenant)).
   add_string(lcResultStruct, "brand", fConvertTenantToBrand(pcTenant)). 
   add_string(lcResultStruct,"name", DayCampaign.DCName). 
   add_date_or_time(lcResultStruct,"valid_from", DayCampaign.ValidFrom, 0). 
   IF DayCampaign.ValidTo < 1/19/2038 THEN
      add_datetime(lcResultStruct,"valid_to", DayCampaign.ValidTo). 
   add_int(lcResultStruct,"duration", DayCampaign.DurMonths). 
   add_string(lcResultStruct,"duration_unit", STRING(DayCampaign.DurUnit)). 
   add_string(lcResultStruct,"valid_from_rule", STRING(DayCampaign.Effective)). 

   CASE DayCampaign.DCType:
   WHEN "3" THEN DO:
         IF DayCampaign.TermFeeCalc > 0 AND 
            DayCampaign.TermFeeModel NE '' THEN DO:

            FIND FIRST FMItem NO-LOCK  WHERE
                       FMItem.Brand     = gcBrand AND
                       FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                       FMItem.FromDate <= TODAY AND
                       FMItem.ToDate   >= TODAY NO-ERROR.
            IF AVAIL FMItem THEN 
               add_string(lcResultStruct, "penalty_fee", STRING(FMItem.Amount)).
         END.
   END.
   WHEN "5" THEN DO:
         FIND FIRST FMItem NO-LOCK  WHERE
                    FMItem.Brand     = gcBrand AND
                    FMItem.FeeModel  = DayCampaign.FeeModel AND
                    FMItem.FromDate <= TODAY AND
                    FMItem.ToDate   >= TODAY NO-ERROR.
         IF AVAIL FMItem THEN 
            add_string(lcResultStruct,"penalty_fee",STRING( FMItem.Amount  * DECIMAL(FMItem.FFItemQty) )).
   END.
   END CASE.

END.
