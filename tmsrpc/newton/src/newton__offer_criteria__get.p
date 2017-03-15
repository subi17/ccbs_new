/**
 * Get offer criteria.
 *
 * @input  ids;array of string;mandatory;offer criteria ids
 * @output offer_criteria;array of struct;offer criteria data
 * @offer_criteria id;string;offer criteria id
          offer_id;string;
          valid_from;datetime;
          valid_to;datetime;not returned if >= 19.01.2049
          included_values;string;
          excluded_values;string;
          criteria_type;string;
 */

{newton/src/header_get.i}

DEF VAR liId AS INT NO-UNDO. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF NUM-ENTRIES(pcID,"|") > 1 THEN
       ASSIGN
           pcTenant = ENTRY(2, pcID, "|")
           pcID     = ENTRY(1, pcID, "|").
   ELSE
       RETURN appl_err("Invalid tenant information").

   {newton/src/settenant.i pcTenant}
       
   liId = INT(pcID) NO-ERROR.

   FIND OfferCriteria NO-LOCK WHERE 
      OfferCriteria.OfferCriteriaID = liId NO-ERROR.

   IF NOT AVAIL OfferCriteria THEN RETURN appl_err("Offer criteria not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", STRING(OfferCriteria.OfferCriteriaID)). 
   add_string(lcResultStruct, "offer_id", OfferCriteria.Offer). 
   add_timestamp(lcResultStruct, "valid_from", OfferCriteria.BeginStamp). 
   IF OfferCriteria.EndStamp < 20380119 THEN 
      add_timestamp(lcResultStruct, "valid_to", OfferCriteria.EndStamp). 
   add_string(lcResultStruct, "included_values", OfferCriteria.IncludedValue). 
   add_string(lcResultStruct, "excluded_values", OfferCriteria.ExcludedValue). 
   add_string(lcResultStruct, "criteria_type", OfferCriteria.CriteriaType). 

END.
