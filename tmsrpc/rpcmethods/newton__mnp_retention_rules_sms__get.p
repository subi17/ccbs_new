/**
 * Get mnp retention collection rule SMS texts
 *
 * @input ;empty
 * @output array or struct;
 * @struct sms_token;string;
           sms_text;string;
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "") = ? THEN RETURN.

resp_array = add_array(response_toplevel_id, "").
      
FOR EACH InvText NO-LOCK WHERE
         InvText.Brand = gcBrand AND
         InvText.Target = "SMS" AND
         InvText.KeyValue BEGINS "MNPRescue" AND
         InvText.Language = 1 AND
         InvText.FromDate <= TODAY AND
         InvText.Todate >= TODAY:
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "sms_token", InvText.Key).
   add_string(lcResultStruct, "sms_text", InvText.InvText).
END.
 
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
