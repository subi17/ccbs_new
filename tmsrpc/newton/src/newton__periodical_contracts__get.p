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

DEF VAR lcPenFeeStruct AS CHAR NO-UNDO. 
DEF VAR lcPList AS CHAR NO-UNDO. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   FIND DayCampaign NO-LOCK WHERE 
        DayCampaign.Brand = gcBrand AND 
        DayCampaign.DCEvent = pcId NO-ERROR.

   IF NOT AVAIL DayCampaign THEN RETURN appl_err("Periodical contract not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", DayCampaign.DCEvent). 
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

               lcPenFeeStruct = add_struct(lcResultStruct,"penalty_fee"). 
               FOR EACH CLIType WHERE
                        CliType.Brand = gcBrand NO-LOCK:
                        lcPList = fCliTypeFeeModelPriceList(
                                    CLIType.CliType,
                                    DayCampaign.TermFeeModel,
                                    TODAY).
                  
                        FIND FIRST FMItem NO-LOCK  WHERE
                                   FMItem.Brand     = gcBrand AND
                                   FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                                   FMItem.PriceList = lcPList AND
                                   FMItem.FromDate <= TODAY AND
                                   FMItem.ToDate   >= TODAY NO-ERROR.
                        IF AVAIL FMItem THEN 
                        add_double(lcPenFeeStruct, CLIType.CLIType, FMItem.Amount).
                END.
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
