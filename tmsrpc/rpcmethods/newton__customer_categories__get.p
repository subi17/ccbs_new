/**
 * Get customer category information for one category record.
 *
 * @input  categories;array of string;mandatory;customercategory for which the data is taken.
 * @output custcat;array of struct;customer category data
 * @custcat id;string; 
            limit;int;default subscription limit
            activationlimit;int;default subscription activation limit
            name;string;category name
 */

{header_get.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   FIND CustCat WHERE CustCat.Brand = gcBrand AND 
                      CustCat.Category = pcId NO-LOCK NO-ERROR.
   IF AVAIL CustCat THEN
   DO:
      lcResultStruct = add_struct(resp_array, "").
      add_string(lcResultStruct, "id", pcId). 
      add_string(lcResultStruct,"name", CustCat.CatName). 
      add_int(lcResultStruct, "limit", CustCat.MobSubLimit ). 
      add_int(lcResultStruct, "activationlimit", CustCat.ActivationLimit).
   END.
   ELSE
      appl_err("Category was not found with category " + pcId).

END.
