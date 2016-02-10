/**
 * Set mnp retention file rule 
 *
 * @input id;int;mandatory;
          paytype;string;optional;CONT/TARJ
          consumption_average;decimal;optional;
          penalty_left;decimal;optional;only for CONT
          penalty_months_left;int;optional;only for CONT
          segment_offer;string;optional;
          sms_token;string;optional;
          valid_to;date;optional;value must be TODAY-1 
          username;string;mandatory;
 * @output boolean;true 
*/

{xmlrpc/xmlrpc_access.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR piID AS INT NO-UNDO. 
DEF VAR pcUsername AS CHAR NO-UNDO. 
DEF VAR llEqual AS LOG NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO. 

DEFINE TEMP-TABLE ttMNPRetentionRule NO-UNDO LIKE MNPRetentionRule.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcStruct = validate_struct(pcStruct,"id!,paytype,consumption_average,penalty_left,penalty_months_left,segment_offer,sms_token,username!,valid_to").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").
piID = get_int(pcStruct, "id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
{Syst/eventval.i}
ASSIGN
   katun = pcUsername
   gcBrand = "1".

FIND FIRST MNPRetentionRule WHERE
           MNPRetentionRule.RetentionRuleID = piID
NO-LOCK NO-ERROR.

IF NOT AVAIL MNPRetentionrule THEN
   RETURN appl_err(SUBST("Unknown MNP retention rule: &1", piID)).

CREATE ttMNPRetentionRule.
BUFFER-COPY mnpretentionrule TO ttMNPRetentionRule.

IF LOOKUP("paytype",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.CLIType = get_string(pcStruct, "paytype").
   IF LOOKUP(ttMNPRetentionRule.CLIType, "CONT,TARJ") = 0 THEN
     RETURN appl_err(SUBST("Invalid paytype: &1", ttMNPRetentionRule.CLIType)).
END.

IF LOOKUP("consumption_average",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.ConsumptionAverage = get_double(pcStruct,
                                                      "consumption_average").
   IF ttMNPRetentionRule.ConsumptionAverage < 0 THEN
      RETURN appl_err("Incorrect consumption_average value").
END.

IF LOOKUP("penalty_left",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.PenaltyLeft = get_double(pcStruct,"penalty_left").
   IF ttMNPRetentionRule.CLIType NE "CONT" THEN 
      RETURN appl_err("penalty_left value is allowed only for CONT").
   IF ttMNPRetentionRule.PenaltyLeft < 0 THEN
      RETURN appl_err("Incorrect penalty_left value").
END.

IF LOOKUP("penalty_months_left",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.PenaltyMonthsLeft = get_double(pcStruct,
                                                     "penalty_months_left").
   IF ttMNPRetentionRule.PenaltyMonthsLeft < 0 THEN
      RETURN appl_err("Incorrect penalty_months_left value").
   IF ttMNPRetentionRule.CLIType NE "CONT" THEN
      RETURN appl_err("penalty_months_left value is allowed only for CONT").
END.

IF LOOKUP("segment_offer",lcStruct) > 0 THEN
   ttMNPRetentionRule.SegmentCode = get_string(pcStruct,"segment_offer").

IF LOOKUP("valid_to",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.ToDate = get_date(pcStruct,"valid_to").
   IF ttMNPRetentionRule.ToDate NE (TODAY - 1) THEN
      RETURN appl_err("valid_to must be TODAY - 1").
END.

IF LOOKUP("sms_token",lcStruct) > 0 THEN DO:
   ttMNPRetentionRule.SMSText = get_string(pcStruct,"sms_token").

   IF ttMNPRetentionRule.SMSText > "" THEN DO:
      FIND FIRST InvText NO-LOCK WHERE
                 InvText.Brand = gcBrand AND
                 InvText.Target = "SMS" AND
                 InvText.KeyValue = ttMNPRetentionRule.SMSText AND
                 InvText.FromDate <= TODAY AND
                 InvText.Todate >= TODAY NO-ERROR.
      IF NOT AVAIL InvText THEN 
         RETURN appl_err(SUBST("Invalid SMS token: ", 
                         ttMNPRetentionRule.SMSText)). 
   END.
END.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {Func/lib/eventlog.i}
   DEF VAR lhMNPRetentionRule AS HANDLE NO-UNDO.
   lhMNPRetentionRule = BUFFER MNPRetentionRule:HANDLE.
END.

BUFFER-COMPARE ttMNPRetentionRule TO MNPRetentionRule SAVE llEqual.

IF NOT llEqual THEN DO:

   FIND CURRENT MNPRetentionRule EXCLUSIVE-LOCK.

   IF llDoEvent THEN DO:
      RUN StarEventInitialize(lhMNPRetentionRule).
      RUN StarEventSetOldBuffer(lhMNPRetentionRule).
   END.
   
   BUFFER-COPY ttMNPRetentionrule EXCEPT ttMNPRetentionRule.RetentionRule 
      TO MNPRetentionrule.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhMNPRetentionRule).
   END.

   RELEASE MNPRetentionrule.
END.

add_boolean(response_toplevel_id, ?, TRUE).
 
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   IF llDoEvent THEN fCleanEventObjects().
   EMPTY TEMP-TABLE ttMNPRetentionRule.
END.
