/**
 * Get service packages.
 *
 * @input ids;array of string;mandatory;  service packages ids
 * @output charge_event;array of struct; service packages data
 * @charge_event id;string; service package id
                 name;string; service package name  
          
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

   FIND ServPac NO-LOCK WHERE 
        ServPac.Brand = gcBrand AND 
        ServPac.ServPac = pcId NO-ERROR.

   IF NOT AVAIL ServPac THEN RETURN appl_err("Service package not found: "+ pcId).

   /* pick up the amount using the first CTServEl,
      taking into account this ServPac contain only one component 
      with a common definition in all the CLIType where it has been created */
   FIND FIRST CTServEl  WHERE
              CTServEl.Brand     = gcBrand   AND
              CTServEl.ServPac   = ServPac.ServPac  AND
              CTServEl.FromDate <= TODAY  NO-LOCK NO-ERROR. 
   IF NOT AVAIL CTServEl THEN RETURN appl_err("Service package is not defined in any clitype").

   
  lcResultStruct = add_struct(resp_array, "").
  add_string(lcResultStruct, "id", ServPac.ServPac + "|" + pcTenant).
  add_string(lcResultStruct, "brand", pcTenant). 
  add_string(lcResultStruct, "name", ServPac.SPName).
  add_int(lcResultStruct,"amount",INT(CTServEl.DefParam)).

END.
