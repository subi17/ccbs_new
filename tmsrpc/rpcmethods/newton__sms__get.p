/**
 * Get a sms message.
 *
 * @input ids;array of string;mandatory;sms ids
 * @output sms;array of struct;sms detail data
 * @sms msisdn;string;mandatory;mobsub cli 
         type;int;mandatory;sms type id
        type_name;string;mandatory;sms type name
        status;int;mandatory;status id
        status_name;string;mandatory;status name 
        creation_time;datetime;mandatory;creation date
        delivery_time;datetime;optional;delivery time
        content;string;mandatory; sms content
        id;string;mandatory;sms id
        sender;string;mandatory;sms sender number
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAr liCounter AS INTEGER NO-UNDO. 

DEF VAR lrCallAlarm AS ROWID.
DEF VAR lcCreditName AS CHARACTER NO-UNDO. 
DEF VAR lcStatusNames AS CHARACTER NO-UNDO init "NEW,FAIL,OK,CANCEL".

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.
pcIDArray = get_array(param_toplevel_id, "0").
resp_array = add_array(response_toplevel_id, "").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".
{lib/smpp/smpp_defs.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   lrCallAlarm = TO-ROWID(pcID).

   FIND CallAlarm NO-LOCK WHERE
        ROWID(CallAlarm) = lrCallAlarm NO-ERROR.

   IF NOT AVAIL CallAlarm THEN RETURN appl_err("SMS message not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "msisdn", CallAlarm.CLI).
   add_int(lcResultStruct,"type",CallAlarm.CreditType).
   lcCreditName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "CallAlarm",
                                   "CreditType",
                                   STRING(CallAlarm.CreditType)).
   add_string(lcResultStruct,"type_name",lcCreditName).
   add_int(lcResultStruct,"status",CallAlarm.DeliStat).
   add_string(lcResultStruct,"status_name", entry(CallAlarm.DeliStat, lcStatusNames)).
   add_timestamp(lcResultStruct,"creation_time",CallAlarm.ActStamp).
   IF CallAlarm.DeliStamp NE 0 THEN 
      add_timestamp(lcResultStruct,"delivery_time",CallAlarm.DeliStamp).
   add_string(lcResultStruct,"content",CallAlarm.DeliMsg).
   add_string(lcResultStruct, "id", STRING(ROWID(CallAlarm))). 
   add_string(lcResultStruct, "sender", (IF CallAlarm.Orig > ""
                                         THEN CallAlarm.Orig
                                         ELSE {&SMPP_DEFAULT_SOURCE_ADDRESS})).
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
