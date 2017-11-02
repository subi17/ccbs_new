/**
 * Get mnp retention dispatch rules
 *
 * @input ;empty
 * @output array of struct;mnp retention dispatch rule data
 * @struct id;string;retention platform id
           percentage;decimal;allocation percentage
           sms_sender;string;SMS sender number
           name;string;Platform name
           operators;string;retention operators for the given platform id
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.CUICommon:katun = "Newton".
Syst.CUICommon:gcBrand = "1".

DEF VAR pcTenant       AS CHAR      NO-UNDO.
DEF VAR lcResultStruct AS CHAR      NO-UNDO. 
DEF VAR resp_array     AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "string") = ? THEN RETURN.

pcTenant   = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

{newton/src/settenant.i pcTenant}
      
FOR EACH mnpretplatform NO-LOCK WHERE
         mnpretplatform.brand = Syst.CUICommon:gcBrand AND
         mnpretplatform.Todate >= TODAY AND
         mnpretplatform.FromDate <= TODAY:
   
   lcResultStruct = add_struct(resp_array, "").
      
   add_string(lcResultStruct, "id", mnpretplatform.RetentionPlatform).
   add_string(lcResultStruct, "name", mnpretplatform.Name).
   add_double(lcResultStruct, "percentage", mnpretplatform.Percentage).
   add_string(lcResultStruct, "sms_sender" ,mnpretplatform.SMSSender).
   add_string(lcResultStruct, "operators", mnpretplatform.Operators).   /* YDR-2644 21/09/17 DP */

END.
 
FINALLY:
   END.
