/**
 * Get offer.
 *
 * @input ids;array of string;mandatory;offer ids
 * @output offer;array of struct;offer data
 * @offer id;string;offer id
      description;int;
      display_item_amounts;boolean;
      valid_from;datetime;
      valid_to;datetime;not returned if >= 19.01.2038 (http://en.wikipedia.org/wiki/Year_2038_problem)
      amount;double;
      priority;int;
      vat_included;boolean;
      active;boolean;
 */
{newton/src/header_get.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF NUM-ENTRIES(pcID,"|") > 1 THEN
       ASSIGN
           pcTenant = ENTRY(2, pcID, "|")
           pcID     = ENTRY(1, pcID, "|").
   ELSE
       RETURN appl_err("Invalid tenant information").

   {newton/src/settenant.i pcTenant}
       
   FIND Offer NO-LOCK WHERE 
      Offer.Brand = gcBrand AND 
      Offer.Offer = pcId NO-ERROR.

   IF NOT AVAIL Offer THEN RETURN appl_err("Offer not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", Offer.Offer). 
   add_string(lcResultStruct, "description", Offer.Description). 
   add_boolean(lcResultStruct, "display_item_amounts", LOGICAL(Offer.DispItemAmounts)). 
   add_datetime(lcResultStruct, "valid_from", Offer.FromDate). 
   IF Offer.ToDate < 1/19/2038 THEN 
      add_datetime(lcResultStruct, "valid_to", Offer.ToDate). 
   add_double(lcResultStruct, "amount", Offer.OfferAmount). 
   add_int(lcResultStruct, "priority", Offer.Priority). 
   add_boolean(lcResultStruct, "vat_included", Offer.VatIncl). 
   add_boolean(lcResultStruct, "active", Offer.Active). 
END.
