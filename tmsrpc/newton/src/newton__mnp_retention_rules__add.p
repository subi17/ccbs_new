/**
 * Add mnp retention file rule 
 *
 * @input paytype;string;optional;CONT/TARJ
          consumption_average;decimal;optional;
          penalty_left;decimal;optional;only for CONT
          penalty_months_left;int;optional;only for CONT
          segment_offer;string;optional;
          sms_token;string;optional;
          username;string;mandatory;
 * @output boolean;true 
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcUsername AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO. 

DEFINE TEMP-TABLE ttMNPRetentionRule NO-UNDO LIKE MNPRetentionRule.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcStruct = validate_struct(pcStruct,"paytype,consumption_average,penalty_left,penalty_months_left,segment_offer,sms_token,username!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF NUM-ENTRIES(lcStruct) <= 1 THEN RETURN appl_err("Rule condition is missing").

{Syst/commpaa.i}
{Syst/eventval.i}
{Func/create_eventlog.i}
ASSIGN
   katun = pcUsername
   gcBrand = "1".

DEF VAR liRetentionRuleID AS INT NO-UNDO. 

liRetentionRuleID = 1. 
FOR EACH MNPRetentionRule NO-LOCK BY MNPRetentionRule.RetentionRuleID DESC:
   liRetentionRuleID = MNPRetentionRule.RetentionRuleID + 1.
   LEAVE.
END.

CREATE ttMNPRetentionRule.
ASSIGN
   ttMNPRetentionRule.RetentionRuleID = liRetentionRuleID
   ttMNPRetentionRule.Brand = gcBrand
   ttMNPRetentionRule.Fromdate = TODAY
   ttMNPRetentionRule.Todate = 12/31/2049.

IF LOOKUP("paytype",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.CLIType = get_string(pcStruct, "paytype").
   IF LOOKUP(ttMNPRetentionRule.CLIType, "CONT,TARJ") = 0 THEN
     RETURN appl_err(SUBST("Invalid paytype: &1", ttMNPRetentionRule.CLIType)).
END.

IF LOOKUP("consumption_average",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.ConsumptionAverage = get_double(pcStruct, "consumption_average").
   IF ttMNPRetentionRule.ConsumptionAverage < 0 THEN RETURN
      appl_err("Incorrect consumption_average value").
END.

IF LOOKUP("penalty_left",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.PenaltyLeft = get_double(pcStruct,"penalty_left").
   IF ttMNPRetentionRule.CLIType NE "CONT" THEN RETURN
      appl_err("penalty_left value is allowed only for CONT").
   IF ttMNPRetentionRule.PenaltyLeft < 0 THEN RETURN
      appl_err("Incorrect penalty_left value").
END.

IF LOOKUP("penalty_months_left",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.PenaltyMonthsLeft = get_double(pcStruct,"penalty_months_left").
   IF ttMNPRetentionRule.PenaltyMonthsLeft < 0 THEN RETURN
      appl_err("Incorrect penalty_months_left value").
   IF ttMNPRetentionRule.CLIType NE "CONT" THEN RETURN
      appl_err("penalty_months_left value is allowed only for CONT").
END.

IF LOOKUP("segment_offer",lcStruct) > 0 THEN
   ttMNPRetentionRule.SegmentCode = get_string(pcStruct,"segment_offer").

IF LOOKUP("sms_token",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.SMSText = get_string(pcStruct,"sms_token").

   IF ttMNPRetentionRule.SMSText > "" THEN DO:
      FIND FIRST InvText NO-LOCK WHERE
                 InvText.Brand = gcBrand AND
                 InvText.Target = "SMS" AND
                 InvText.KeyValue = ttMNPRetentionRule.SMSText AND
                 InvText.FromDate <= TODAY AND
                 InvText.Todate >= TODAY NO-ERROR.
      IF NOT AVAIL InvText THEN RETURN 
         appl_err(SUBST("Invalid SMS token: ", ttMNPRetentionRule.SMSText)). 
   END.
END.

IF gi_xmlrpc_error NE 0 THEN RETURN.

CREATE MNPRetentionRule.
BUFFER-COPY ttMNPRetentionRule TO MNPRetentionRule.

IF llDoEvent THEN fMakeCreateEvent((BUFFER MNPRetentionRule:HANDLE),"",katun,"").

RELEASE MNPRetentionRule.

add_boolean(response_toplevel_id, ?, TRUE).
 
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   EMPTY TEMP-TABLE ttMNPRetentionRule.
END.
