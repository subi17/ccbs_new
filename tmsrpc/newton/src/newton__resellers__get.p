/**
 * Get reseller data
 *
 * @input  id;array of string;mandatory;reseller id
 * @output topup_scheme_row;array of struct;topup scheme row data
 * @reseller id;string;mandatory;reseller id
             name;string;mandatory;reseller name
             commisssion_percentage;double;mandatory;
             address;string;mandatory;
             email;string;mandatory;
             entity_code;int;optional;not returned if null
             active;boolean;mandatory;
             bank_codes;string;mandatory;bank codes in json format;
             can_have_bank;boolean;mandatory;
 */

{newton/src/header_get.i}
{Func/multitenantfunc.i}

DEF VAR lcArray AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR lcJSON AS LONGCHAR NO-UNDO. 

DEFINE TEMP-TABLE bank_codes
   FIELD bank_code LIKE ResellerTF.TFBank
   FIELD valid_from LIKE ResellerTF.ValidFrom.

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   
   IF NUM-ENTRIES(pcId,"|") > 1 THEN
      ASSIGN
          pcTenant = ENTRY(2,pcId,"|")
          pcId     = ENTRY(1,pcId,"|").
   ELSE
      RETURN appl_err("Invalid tenant information").
   
   {newton/src/settenant.i pcTenant}

   FIND Reseller NO-LOCK WHERE 
        Reseller.Brand = gcBrand AND
        Reseller.Reseller = pcID NO-ERROR.

   IF NOT AVAIL Reseller THEN RETURN appl_err("Reseller not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", Reseller.Reseller + "|" + BUFFER-TENANT-NAME(Reseller)). 
   add_string(lcResultStruct, "brand", BUFFER-TENANT-NAME(Reseller)).
   add_string(lcResultStruct,"name", Reseller.RsName). 
   add_double(lcResultStruct,"commission_percentage", Reseller.CommPerc).
   add_string(lcResultStruct,"address", Reseller.Address[1]).
   add_string(lcResultStruct,"email", Reseller.Email). 
   add_boolean(lcResultStruct,"active", Reseller.Active).  
   add_boolean(lcResultStruct,"can_have_bank",
      (Reseller.Fuc1 > "" AND Reseller.Fuc2 > "")).  
   
   IF Reseller.EntityCode NE ? THEN
      add_int(lcResultStruct,"entity_code", Reseller.EntityCode).  

   FOR EACH ResellerTF NO-LOCK WHERE
            ResellerTF.Brand = gcBrand AND
            ResellerTF.Reseller = Reseller.Reseller:

      CREATE bank_codes.
      ASSIGN
         bank_codes.bank_code = ResellerTF.TFBank
         bank_codes.valid_from = ResellerTF.ValidFrom.
   END.

   IF AVAIL bank_codes THEN DO:
      IF NOT TEMP-TABLE bank_codes:WRITE-JSON("LONGCHAR", lcJSON, FALSE) THEN 
         RETURN appl_err("Bank data serialization failed").

      add_string(lcResultStruct,"bank_codes",STRING(lcJSON)).
      EMPTY TEMP-TABLE bank_codes.
   END.
   ELSE add_string(lcResultStruct,"bank_codes","").
END. 

FINALLY:
   EMPTY TEMP-TABLE bank_codes.
END.
