/**
 * Return relationship  services <-> subscription_types for xfera
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.

lcArray = add_array(response_toplevel_id, "").

FUNCTION create_services_subscription_types RETURN LOGICAL
      (pcServiceId AS CHAR,
       pcSubscriptionTypeId AS CHAR):
    lcStruct = add_struct(lcArray, "").
    add_string(lcStruct, "service_id", pcServiceId).
    add_string(lcStruct, "subscription_type_id", pcSubscriptionTypeId).
END FUNCTION.

create_services_subscription_types("Balance_1", "postpaid").
create_services_subscription_types("Balance_2", "postpaid").
create_services_subscription_types("PhoneBlock", "postpaid").
create_services_subscription_types("SmsBlock", "postpaid").
create_services_subscription_types("AllSmsBlock", "postpaid").
create_services_subscription_types("AllPhoneBlock", "postpaid").
create_services_subscription_types("ForeignPhoneBlock", "postpaid").
create_services_subscription_types("RoamingBlock", "postpaid").
create_services_subscription_types("MMS", "postpaid").
create_services_subscription_types("MMS", "prepaid").
create_services_subscription_types("AnswerMachine", "postpaid").
create_services_subscription_types("SecretNumber", "postpaid").
create_services_subscription_types("CallListing", "postpaid").
create_services_subscription_types("RedirIfBusy", "postpaid").
create_services_subscription_types("NumberSend", "postpaid").
create_services_subscription_types("NumberReceive", "postpaid").
create_services_subscription_types("GPRS", "postpaid").
create_services_subscription_types("GPRS", "prepaid").
create_services_subscription_types("FriendNumbers", "postpaid").
create_services_subscription_types("SmsPack_1", "postpaid").
create_services_subscription_types("VoicePack_1", "postpaid").
create_services_subscription_types("VoicePack_2", "postpaid").
create_services_subscription_types("VoicePack_3", "postpaid").
create_services_subscription_types("GPRSEXTRA", "postpaid").

