/**
 * Get offer item.
 *
 * @input ids;array of string;mandatory;offer item ids
 * @output offer_item;array of struct;offer item data
 * @offer_item id;string;offer id
      offer_id;string;
      amount;double;
      valid_from;datetime;
      valid_to;datetime;not returned if >= 19.01.2038 (http://en.wikipedia.org/wiki/Year_2038_problem)
      display_in_ui;boolean;
      display_on_invoice;boolean;
      item_id;string;
      item_type;string;TMS type name is converted to Newton class name
      vat_included;int;(0=no,1=yes)
      periods;int;(eg: 1-12)
 */

{newton/src/xmlrpc_names.i}
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

   liId = INT(pcId) NO-ERROR.

   FIND OfferItem NO-LOCK WHERE 
      OfferItem.OfferItemId = liId NO-ERROR.

   IF NOT AVAIL OfferItem THEN RETURN appl_err("Offer item not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id", STRING(OfferItem.OfferItemId)). 
   add_string(lcResultStruct, "offer_id", OfferItem.Offer). 
   add_double(lcResultStruct, "amount", OfferItem.Amount). 
   add_timestamp(lcResultStruct, "valid_from", OfferItem.BeginStamp). 
   IF OfferItem.EndStamp < 20380119 THEN
      add_timestamp(lcResultStruct, "valid_to", OfferItem.EndStamp). 
   add_boolean(lcResultStruct, "display_in_ui", OfferItem.DispInUI). 
   add_boolean(lcResultStruct, "display_on_invoice", OfferItem.DispOnInvoice). 
   add_string(lcResultStruct, "item_id", OfferItem.ItemKey). 
   add_boolean(lcResultStruct, "vat_included", OfferItem.VatIncl). 
   add_string(lcResultStruct, "item_type", fConvertToWebName(OfferItem.ItemType)).
   add_int(lcResultStruct, "periods", OfferItem.Periods).
END.

FINALLY:
   EMPTY TEMP-TABLE ttNamePairs.
END.
