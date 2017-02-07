/**
 * Get billing item.
 *
 * @input  ids;array of string;mandatory;billing item ids (billcode)
 * @output billitem;array of struct;billing item data
 * @billitem id;string;
           name;string;
           billing_group;string;7=terminal,9=SIM
           ui_order;int;UI order
           active;boolean;is (terminal) billitem active
           title_en;string;english translation
           title_es;string;spanish translatation
           title_eu;string;
           title_ca;string; 
           title_ga;string;
 */

{rpcmethods/header_get.i}
{Func/transname.i}

DEFINE VARIABLE lctitle   AS CHARACTER NO-UNDO EXTENT 5 
                          INITIAL ["title_es","title_ca","title_eu","title_ga","title_en"] . 
DEFINE VARIABLE liLang AS INTEGER NO-UNDO. 
DEFINE VARIABLE lctrans AS CHARACTER NO-UNDO. 

FUNCTION fGetTranslationName RETURN CHARACTER
  (INPUT pcCode AS CHARACTER, INPUT piLang AS INTEGER):
  DEFINE VARIABLE cRetText AS CHARACTER NO-UNDO. 

   cRetText = fTranslationName(gcBrand,
                                 1,  /* BillItem */
                                 pcCode,
                                 piLang,
                                 TODAY).
   RETURN cRetText.
END.

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   
   FIND BillItem NO-LOCK WHERE 
        BillItem.Brand = gcBrand AND 
        BillItem.BillCode = pcId NO-ERROR.

   IF NOT AVAIL BillItem THEN RETURN appl_err("Billing item not found: " + pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", BillItem.BillCode). 
   add_string(lcResultStruct,"billing_group", BillItem.BIGroup). 
   add_int(lcResultStruct, "ui_order", BillItem.OrderChannelOrder).
   add_boolean(lcResultStruct, "active", BillItem.Active).
   add_string(lcResultStruct,"name", BillItem.BIName).   
    
   DO liLang = 1 TO 5 :
       lctrans = fGetTranslationName(BillItem.BillCode, liLang).
       IF lctrans <> ? AND lctrans <> "" THEN 
          add_string(lcResultStruct,lctitle[liLang],lctrans).
   END.

END.

