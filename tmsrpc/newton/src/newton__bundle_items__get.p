/**
 * Get bundle item.
 *
 * @input  id;array of string;mandatory; bundle item id
 * @output bundle_item;array of struct;bundle item data
 * @bundle_item id;string;bundle item id
                 name;string;description
 */

{newton/src/header_get.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   
   FIND TMSParam NO-LOCK WHERE 
        TMSParam.Brand = gcBrand AND
        TMSParam.ParamGroup = "BundleItem" AND
        TMSParam.ParamCode = pcId NO-ERROR.
 
   IF NOT AVAIL TMSParam THEN RETURN appl_err("Bundle Item not found: "+ pcId).
   
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", TMSParam.ParamCode). 
   add_string(lcResultStruct,"name", TMSParam.CharVal). 
END.
