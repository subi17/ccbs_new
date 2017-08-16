/**
 * List of SVAs and their status for a subscription
 *
 * @Input msseq;int;mandatory;the subscription
 * @output services;array;a list of structs
 * @service service_id;string;newton alias for a service
            value;string/int;Status of the service (on/off)
            params;struct;Optionally additional information
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
katun    = "NewtonAd".
gcBrand  = "1".
{Syst/tmsconst.i}
/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Output parameters */
DEF VAR resp_struct     AS CHAR NO-UNDO.
DEF VAR top_array       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.
DEF VAR params_struct   AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN 
    RETURN.

piMsSeq = get_pos_int(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub WHERE MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN
    RETURN appl_err(SUBST("MobSub &1 not found", piMsSeq)).

FIND FRIST TPService WHERE TPService.MsSeq = piMsSeq AND TPService.Operation = {&TYPE_ACTIVATION} AND TPService.ServType = "Television" NO-LOCK NO-ERROR.
IF NOT AVAIL TPService THEN 
    RETURN appl_err("TV Service not found").

resp_struct = add_struct(response_toplevel_id, "").

add_string(resp_struct, "MsSeq", Mobsub.MsSeq).
IF Mobsub.fixednumber NE ? THEN
   add_string(resp_struct, "fixed_number", Mobsub.FixedNumber).
ELSE
   add_string(resp_struct, "fixed_number", "").

add_string   (resp_struct, "product"       , TPService.Product).
add_string   (resp_struct, "serialnumber"  , TPService.SerialNbr).
add_string   (resp_struct, "offer"         , TPService.Offer).
add_string   (resp_struct, "user"          , TPService.UserCode).
add_string   (resp_struct, "status"        , TPService.ServStatus).
add_string   (resp_struct, "externalid"    , TPService.MessageId).
add_string   (resp_struct, "responsecode"  , TPService.ResponseCode).
add_string   (resp_struct, "additionalinfo", TPService.AdditionalInfo).
add_timestamp(resp_struct, "created_time"  , TPService.CreatedTS).
add_timestamp(resp_struct, "updated_time"  , TPService.UpdateTS).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
          
