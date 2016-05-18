/**
 * Get mnp retention dispatch rules
 *
 * @input ;empty
 * @output array of struct;mnp retention dispatch rule data
 * @struct id;string;retention platform id
           percentage;decimal;allocation percentage
           sms_sender;string;SMS sender number
           name;string;Platform name
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "") = ? THEN RETURN.

resp_array = add_array(response_toplevel_id, "").
      
FOR EACH mnpretplatform NO-LOCK WHERE
         mnpretplatform.brand = gcBrand AND
         mnpretplatform.Todate >= TODAY AND
         mnpretplatform.FromDate <= TODAY:
   
   lcResultStruct = add_struct(resp_array, "").
      
   add_string(lcResultStruct, "id", mnpretplatform.RetentionPlatform).
   add_string(lcResultStruct, "name", mnpretplatform.Name).
   add_double(lcResultStruct, "percentage", mnpretplatform.Percentage).
   add_string(lcResultStruct, "sms_sender" ,mnpretplatform.SMSSender).
END.
 
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
