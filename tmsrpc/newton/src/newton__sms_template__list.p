/** 
 * RPC to return list of all InvText SMSs.
 *
 * @input empty;
 * @output invtxt_st;array;containing list of all invtexts
 * @invtxt_st keyvalue;string;Unique value for identifying
              sendrule;string;sendrule of SMS
              description;string;title of SMS
              process;string;process name
              langitems;array;array of language item structs
 * @langitems language;int;translation of SMS
              smstext;string;content of SMS
           
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.CUICommon:gcBrand = "1".


DEFINE VARIABLE resp_array       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcResultStruct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLangItemStruct AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lclangArray      AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcTenant         AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

{newton/src/settenant.i pcTenant}

FOR EACH InvText NO-LOCK WHERE
         InvText.Brand     = Syst.CUICommon:gcBrand AND
         InvText.Target    = "SMS" AND
         InvText.Language  = 1 AND
         InvText.ToDate   >= TODAY AND
         InvText.FromDate <= TODAY:

   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "keyvalue", InvText.KeyValue).
   add_string(lcResultStruct, "sendrule", InvText.SendRule).
   add_string(lcResultStruct, "description",
      (IF InvText.MainTitle > ""
       THEN InvText.MainTitle 
       ELSE InvText.TxtTitle)).
   add_string(lcResultStruct, "process", InvText.Category).
   lclangArray = add_array(lcResultStruct, "langitems").
   
   lcLangItemStruct = add_struct(lclangArray, "").
   add_int(lcLangItemStruct, "language", InvText.Language).
   add_string(lcLangItemStruct, "smstext", InvText.InvText).
   FOR EACH RepText NO-LOCK WHERE
            RepText.Brand     = Syst.CUICommon:gcBrand AND
            RepText.LinkCode  = STRING(InvText.ITNum) AND
            RepText.TextType  = 32 AND
            RepText.ToDate   >= TODAY AND
            RepText.FromDate <= TODAY:
      lcLangItemStruct = add_struct(lclangArray, "").
      add_int(lcLangItemStruct, "language", RepText.Language).
      add_string(lcLangItemStruct, "smstext", RepText.RepText).
   END.
END.

FINALLY:
   END.

