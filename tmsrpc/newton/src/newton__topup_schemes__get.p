/**
 * Get topup scheme.
 *
 * @input  id;array of string;mandatory;topup scheme id
 * @output topup_scheme;array of struct;topup scheme data
 * @topup_scheme id;string;topup scheme id
               valid_from;datetime;
               valid_to;datetime;not returned if >= 31.12.2049
               vat_included;boolean;is vat included in amount
               name;string;description
 */

{newton/src/header_get.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   
   IF NUM-ENTRIES(pcId,"|") > 1 THEN
      ASSIGN
          pcTenant = ENTRY(2,pcId,"|")
          pcId     = ENTRY(1,pcId,"|").
   ELSE
      RETURN appl_err("Invalid tenant information").

   {newton/src/settenant.i pcTenant}
   
   FIND TopupScheme NO-LOCK WHERE 
      TopupScheme.Brand = gcBrand AND 
      TopupScheme.TopupScheme = pcId NO-ERROR.

   IF NOT AVAIL TopupScheme THEN RETURN appl_err("Topup scheme not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", TopupScheme.TopupScheme + "|" + pcTenant).
   add_string(lcResultStruct, "brand", pcTenant).  
   add_datetime(lcResultStruct,"valid_from", TopupScheme.FromDate). 
   IF TopupScheme.ToDate < 12/31/2049 THEN
      add_datetime(lcResultStruct,"valid_to", TopupScheme.ToDate). 
   add_boolean(lcResultStruct,"vat_included", TopupScheme.VATIncl). 
   add_string(lcResultStruct,"name", TopupScheme.Description). 
END.
