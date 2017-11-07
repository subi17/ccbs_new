/**
 * Return all services for xfera
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.

lcArray = add_array(response_toplevel_id, "").

FUNCTION create_service RETURN LOGICAL
      ( pcServiceid AS CHAR,
        pcNameForm AS CHAR,
        pcType AS CHAR ):
    lcStruct = add_struct(lcArray, "").
    add_string(lcStruct, "id", pcServiceid).
    add_string(lcStruct, "tms_alias", pcNameForm).
    add_string(lcStruct, "stype", pcType).
END FUNCTION.

create_service("Balance_1", "SALDOREM", "Balance").
create_service("Balance_2", "SALDOAGR", "Balance").
create_service("PhoneBlock", "-Numeroestot", "Block").
create_service("SmsBlock", "-Tekstiviestiestot", "Block").
create_service("AllSmsBlock", "SMR", "BlockOnOff").
create_service("AllPhoneBlock", "KLL", "BlockOnOff").
create_service("ForeignPhoneBlock", "KLU", "BlockOnOff").
create_service("RoamingBlock", "OWN", "BlockOnOff").
create_service("MMS", "MMS", "OnOff").
create_service("AnswerMachine", "PP2", "OnOff").
create_service("SecretNumber", "NUMBERINQ", "OnOff").
create_service("CallListing", "CALLSPEC", "OnOff").
create_service("RedirIfBusy", "SP2", "OnOff").
create_service("NumberSend", "AES", "OnOff").
create_service("NumberReceive", "ANN", "OnOff").
create_service("GPRS", "GPRS", "OnOff").
create_service("FriendNumbers", "PNP", "Friend").
create_service("SmsPack_1", "", "SmsPack").
create_service("VoicePack_1", "PUHEPAK", "VoicePack").
create_service("VoicePack_2", "PUHEPAK", "VoicePack").
create_service("VoicePack_3", "PUHEPAK", "VoicePack").
create_service("GPRSEXTRA", "GPRSEXTRA", "GPRSPack").

