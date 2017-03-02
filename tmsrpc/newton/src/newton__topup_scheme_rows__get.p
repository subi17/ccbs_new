/**
 * Get topup scheme row.
 *
 * @input  id;array of string;mandatory;topup scheme row id
 * @output topup_scheme_row;array of struct;topup scheme row data
 * @topup_scheme_row id;string;topup scheme row id
               amount;double;
               valid_from;datetime;
               valid_to;datetime;not returned if >= 31.12.2049
               discount;double;
               topup_scheme_id;string;related topup scheme id
 */

{newton/src/header_get.i}

DEF VAR liId AS INTEGER NO-UNDO. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF NUM-ENTRIES(pcId,"|") > 1 THEN
      ASSIGN
          pcTenant = ENTRY(2,pcId,"|")
          pcId     = ENTRY(1,pcId,"|").
   ELSE
      RETURN appl_err("Invalid tenant information").

   liId = INT(pcId) NO-ERROR.
   
   {newton/src/settenant.i pcTenant}

   FIND TopupSchemeRow NO-LOCK WHERE 
        TopupSchemeRow.TopupSchemeRowId = liId NO-ERROR.

   IF NOT AVAIL TopupSchemeRow THEN RETURN appl_err("Topup scheme row not found: "+ pcId).

      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", STRING(TopupSchemeRow.TopupSchemeRowId) + "|" + BUFFER-TENANT-NAME(TopupSchemeRow)). 
   add_string(lcResultStruct, "brand", BUFFER-TENANT-NAME(TopupSchemeRow)). 
   IF TopupSchemeRow.DisplayAmount > 0 THEN
      add_double(lcResultStruct,"amount", TopupSchemeRow.DisplayAmount). 
   ELSE
      add_double(lcResultStruct,"amount", TopupSchemeRow.Amount). 
   add_timestamp(lcResultStruct,"valid_from", TopupSchemeRow.BeginStamp). 
   IF TopupSchemeRow.EndStamp < 20491231 THEN
      add_timestamp(lcResultStruct,"valid_to", TopupSchemeRow.EndStamp). 
   add_double(lcResultStruct,"discount", TopupSchemeRow.DiscountAmount). 
   add_string(lcResultStruct,"topup_scheme_id", TopupSchemeRow.TopupScheme). 

END.
