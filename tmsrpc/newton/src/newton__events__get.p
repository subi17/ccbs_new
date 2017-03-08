/**
 * Get events (VFR - Vista For Retailers)
 *
 * @input ids;array of string;mandatory;event ids
 * @output event;array of struct;event data
 * @event id;string;event id (request id)
      msisdn;string;
      customer_number;string;
      username;string;
      created_at;datetime;
      event_type;string;vm,icc,stc,acc,data,imei
      change;string;old_value|new_value or new_value or service|new_value (vm)
 */
{newton/src/header_get.i}

DEFINE VARIABLE lcEventType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcChange AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcState AS CHARACTER NO-UNDO. 
DEFINE VARIABLE piID AS INTEGER NO-UNDO. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   piID = INT(pcId) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN appl_err("Incorrect ID").

   FIND MsRequest NO-LOCK WHERE MsRequest.MsRequest = piID NO-ERROR.
   IF NOT AVAIL MsRequest THEN 
      RETURN appl_err("Event not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").
   
   add_string(lcResultStruct, "id", STRING(MsRequest.MsRequest)). 
   add_string(lcResultStruct, "msisdn", MsRequest.CLI).
   add_int(lcResultStruct, "customer_number", MsRequest.Custnum).
   add_string(lcResultStruct, "username", MsRequest.UserCode).
   add_timestamp(lcResultStruct, "created_at", MsRequest.CreStamp).

   CASE MsRequest.ReqType:

      WHEN 1 THEN DO:

         ASSIGN
            lcEventType = "vm"
            lcChange = MsRequest.ReqCParam1.
      
         CASE MsRequest.ReqCParam1:
            WHEN "CF" THEN CASE MsRequest.ReqIParam1:
               WHEN 0 THEN lcState = "OFF". 
               WHEN 1 THEN lcState = "CF". 
               WHEN 2 THEN lcState = "MCA". 
               OTHERWISE lcState = STRING(MsRequest.ReqIParam1).
            END.
            OTHERWISE lcState = STRING(MsRequest.ReqIParam1).
         END.

         lcChange = lcChange + "|" + lcState.

      END.
      WHEN 15 THEN ASSIGN
         lcEventType = "icc"
         lcChange = MsRequest.ReqCParam2.
      WHEN 0 THEN ASSIGN
         lcEventType = "stc"
         lcChange = MsRequest.ReqCParam1 + "|" + MsRequest.ReqCParam2.
      WHEN 10 THEN ASSIGN
         lcEventType = "acc"
         lcChange = STRING(MsRequest.Custnum) + "|" +
         /* IF MsRequest.ReqIParam1 > 0 THEN we change to an existing customer */
            (IF MsRequest.ReqIParam1 > 0 THEN STRING(MsRequest.ReqIParam1)
            ELSE ENTRY(12,MsRequest.ReqCParam1,";") + ";" +
                 ENTRY(13,MsRequest.ReqCParam1,";")).
      WHEN 78 THEN ASSIGN
         lcEventType = "data"
         lcChange = STRING(MsRequest.ReqIParam1).
      WHEN 80 THEN ASSIGN
         lcEventType = "imei"
         lcChange = MsRequest.ReqCParam1 + "|" + MsRequest.ReqCParam2.
      OTHERWISE lcEventType = STRING(MsRequest).
   END.
   
   add_string(lcResultStruct, "event_type", lcEventType). 
   add_string(lcResultStruct, "change", lcChange).
END.
