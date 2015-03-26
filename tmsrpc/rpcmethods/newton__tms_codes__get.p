/**
 * Get tmscodes.
 *
 * @input  id;array of string;mandatory;tmscodes idenfier id
 * @output tmscodes;array of struct;tmscodes data
 * @tmscodes id;string;tmscodes codevalue and codename pair
 */

{xmlrpc_names.i}
{header_get.i}


FUNCTION fGetRequestTypes RETURNS LOGICAL:

   FOR EACH RequestType NO-LOCK WHERE
            RequestType.Brand = "1" AND
            RequestType.InUse = TRUE:

      add_string(lcResultStruct, 
                 STRING(RequestType.ReqType), 
                 RequestType.ReqName). 
   END.
END.

FUNCTION fGetTMSCodes RETURNS LOGICAL (
   icTable AS CHAR,
   icField AS CHAR):

   FOR EACH TMSCodes WHERE
      TMSCodes.TableName = icTable AND
      TMSCodes.FieldName = icField AND 
      TMSCodes.InUse     = 1 NO-LOCK:

      IF icTable = "OfferItem" AND icField = "ItemType" THEN DO:
         add_string(lcResultStruct,
                    fConvertToWebName(TMSCodes.CodeValue),
                    TMSCodes.CodeName).
         NEXT.
      END.

      add_string(lcResultStruct, TMSCodes.CodeValue, TMSCodes.CodeName). 
  
   END.

END FUNCTION. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   lcResultStruct = add_struct(resp_array, "").
   CASE pcID:
      WHEN "criteria_type" THEN fGetTMSCodes("OfferCriteria", "CriteriaType"). 
      WHEN "duration_unit" THEN fGetTMSCodes("DayCampaign", "DurUnit"). 
      WHEN "item_type" THEN fGetTMSCodes("OfferItem", "ItemType"). 
      WHEN "NumberType" THEN fGetTMSCodes("Order", "NumberType"). 
      WHEN "OldPayType" THEN fGetTMSCodes("Order", "OldPayType"). 
      WHEN "OrderChannel" THEN fGetTMSCodes("Order", "OrderChannel"). 
      WHEN "PayType" THEN fGetTMSCodes("CLIType", "PayType"). 
      WHEN "valid_from_rule" THEN fGetTMSCodes("DayCampaign", "Effective").
      WHEN "Order_StatusCode" THEN fGetTMSCodes("Order", "StatusCode").
      WHEN "RenewalSegment" THEN fGetTMSCodes("MobSub","SegmentOffer").
      WHEN "profession" THEN fGetTMSCodes("OrderCustomer","Profession").
      WHEN "request_type" THEN fGetRequestTypes().
      WHEN "TFBank" THEN fGetTMSCodes("FixedFee","TFBank").
      OTHERWISE RETURN appl_err("Unknown tmscodes identifier: " + pcId).
   END.

END.
   
FINALLY:
   EMPTY TEMP-TABLE ttNamePairs.
END.
