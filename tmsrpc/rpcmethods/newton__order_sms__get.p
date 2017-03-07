/**
 * Get order sms messages.
 *
 * @input ids;array of string;mandatory;sms ids
 * @output array of struct;sms_data
 * @sms_data msisdn;string;mandatory;mobsub cli 
        creation_time;datetime;mandatory;creation date
        delivery_time;datetime;optional;delivery time
        content;string;mandatory; sms content
        id;string;mandatory;sms id
        source;string;mandatory;"yoigo" or "customer"
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAr liCounter AS INTEGER NO-UNDO. 

DEF VAR liSMSSeq AS INT NO-UNDO.
DEF VAR lcSource AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.
pcIDArray = get_array(param_toplevel_id, "0").
resp_array = add_array(response_toplevel_id, "").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{Syst/commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".
{Func/smsmessage.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   liSMSSeq = INT(pcID).

   FIND SMSMessage NO-LOCK WHERE
        SMSMessage.SMSSeq = liSMSSeq NO-ERROR.

   IF NOT AVAIL SMSMessage THEN
      RETURN appl_err("SMS message not found: "+ pcId).

   IF SMSMessage.DeliType EQ {&SMS_DELITYPE_IN} THEN 
      lcSource = "customer".
   ELSE lcSource = "yoigo".
      
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct,"msisdn", SMSMessage.MSISDN).
   add_timestamp(lcResultStruct,"creation_time",SMSMessage.ActStamp).
   IF SMSMessage.DeliStamp NE 0 THEN 
      add_timestamp(lcResultStruct,"delivery_time",SMSMessage.DeliStamp).
   add_string(lcResultStruct,"content",SMSMessage.DeliMsg).
   add_string(lcResultStruct, "id", STRING(SMSMessage.SMSSeq)). 
   add_string(lcResultStruct, "source", lcSource).
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
