/**
 * Get active mnp retention file rules 
 *
 * @input  brand;string;mandatory
 * @output array or struct;mnp retention collection rules
 * @struct id;int;
           paytype;string;CONT/TARJ
           consumption_average;decimal;
           penalty_left;decimal;
           penalty_months_left;int;
           segment_offer;string;
           sms_token;string;
           sms_text;string;
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAR pcTenant   AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") = ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

resp_array = add_array(response_toplevel_id, "").
      
FOR EACH mnpretentionrule NO-LOCK WHERE
         mnpretentionrule.brand = gcBrand AND
         mnpretentionrule.ToDate >= TODAY AND
         mnpretentionrule.FromDate <= TODAY:
   
   lcResultStruct = add_struct(resp_array, "").
      
   add_int(lcResultStruct, "id", mnpretentionrule.RetentionRuleID).
   add_string(lcResultStruct, "paytype", mnpretentionrule.CLIType).
   add_double(lcResultStruct, "consumption_average", mnpretentionrule.ConsumptionAverage).
   add_double(lcResultStruct, "penalty_left", mnpretentionrule.PenaltyLeft).
   add_int(lcResultStruct, "penalty_months_left", mnpretentionrule.PenaltyMonthsLeft).
   add_string(lcResultStruct, "segment_offer", mnpretentionrule.SegmentCode).
   add_string(lcResultStruct, "sms_token", mnpretentionrule.SMSText).

   IF MNPRetentionRule.SMSText > "" THEN DO:
      FIND FIRST InvText NO-LOCK WHERE
                 InvText.Brand = gcBrand AND
                 InvText.Target = "SMS" AND
                 InvText.KeyValue = MNPRetentionRule.SMSText AND
                 InvText.Language = 1 AND
                 InvText.FromDate <= TODAY AND
                 InvText.Todate >= TODAY NO-ERROR.
      add_string(lcResultStruct, "sms_text", 
                  (IF AVAIL InvText THEN InvText.InvText
                   ELSE "")).
   END.
   ELSE add_string(lcResultStruct, "sms_text", "").
END.
 
FINALLY:
   END.
