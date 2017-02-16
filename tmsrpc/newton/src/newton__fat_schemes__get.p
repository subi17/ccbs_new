/**
 * Get FAT scheme.
 *
 * @input  ids;array of int;mandatory;FAT scheme ids
 * @output fat_scheme;array of struct;FAT scheme data
 * @fat_scheme id;string;FAT scheme id
               name;string;FAT scheme name
               amount;double;default FAT amount
               division_periods;int;number of months divided
               transfer_unused;boolean;
 */
{newton/src/header_get.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   FIND FatGroup NO-LOCK WHERE FatGroup.Brand = gcBrand AND FatGroup.FtGrp = pcId NO-ERROR.

   IF NOT AVAIL FatGroup THEN 
      RETURN appl_err("FAT scheme not found: " + pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", FatGroup.FtGrp). 
   add_string(lcResultStruct,"name", FatGroup.FtgName). 
   add_double(lcResultStruct,"amount", FatGroup.Amount). 
   add_int(lcResultStruct,"division_periods", FatGroup.PeriodQty). 
   add_boolean(lcResultStruct,"transfer_unused", FatGroup.Transfer). 

END.
