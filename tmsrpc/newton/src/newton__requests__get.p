/**
 * Get Request data
 *
 * @input  id;array of string;MsRequest ID
 * @output request;array of struct;request data
 * @request id;int;request id
            mobsub_id;int;subscription id
            msisdn;string;msisdn of the request
            customer_number;int;customer number of the request
            name;string;name of the customer of the request
            activated_at;timestamp;timestamp of the request activation
            handled_at;timestamp;timestamp of the request handling
            updated_at;timestamp;timestamp of the request updating
            created_at;timestamp;timestamp of the request creation
            type;int;request type id 
            status;int;request status id
            request_source;string;request source id
            user_id;string;user who created request
            create_fees;boolean;create possible fees
            send_sms;boolean;is sms sended
            send_sms;string;sms id to send customer
 */

{newton/src/header_get.i}
DEFINE VARIABLE katun AS CHARACTER NO-UNDO. 
&SCOPED-DEFINE BrandVarDefined YES
{Func/func.p}
DEF VAR liId AS INT NO-UNDO. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   liId = INT(pcID) NO-ERROR.
   
   FIND MsRequest NO-LOCK WHERE MsRequest.MsRequest = liId NO-ERROR.
   IF NOT AVAIL MsRequest THEN 
      RETURN appl_err(SUBST("Request with id &1 was not found", liId)).
         
   lcResultStruct = add_struct(resp_array, "").

   add_int(lcResultStruct, "id", MsRequest.MsRequest).
   add_int(lcResultStruct, "mobsub_id", MsRequest.MsSeq).
   add_string(lcResultStruct, "msisdn", MsRequest.CLI).

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
   IF AVAIL Customer THEN DO:
      add_int(lcResultStruct, "customer_number", Customer.Custnum). 
      add_string(lcResultStruct, "name", fDispCustName(BUFFER Customer)). 
   END.

   add_timestamp(lcResultStruct, "activated_at", MsRequest.ActStamp).
   add_timestamp(lcResultStruct, "handled_at", MsRequest.DoneStamp).
   add_timestamp(lcResultStruct, "updated_at", MsRequest.UpdateStamp).
   add_timestamp(lcResultStruct, "created_at", MsRequest.CreStamp).
   add_int(lcResultStruct, "type", MsRequest.ReqType).
   add_int(lcResultStruct, "status", MsRequest.ReqStatus).
   add_string(lcResultStruct, "request_source", MsRequest.ReqSource).
   add_string(lcResultStruct, "user_id", MsRequest.UserCode).
   add_boolean(lcResultStruct, "create_fees", MsRequest.CreateFees).
   add_int(lcResultStruct, "send_sms", MsRequest.SendSMS).
   add_string(lcResultStruct, "sms_text", MsRequest.SMSText).
END.
