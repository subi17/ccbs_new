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
{Func/vasfunc.i}
/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Local variables */
DEF VAR lii             AS INT NO-UNDO.

/* Output parameters */
DEF VAR top_array       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.
DEF VAR params_struct   AS CHAR NO-UNDO.

DEF VAR llgSVA AS LOGICAL NO-UNDO.
DEF VAR liParams AS INT NO-UNDO.
DEF VAR ldPrice AS DECIMAL NO-UNDO.
DEF VAR liStatus AS INT NO-UNDO.
IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_pos_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND MobSub NO-LOCK WHERE
     MobSub.MsSeq = piMsSeq AND
     MobSub.Brand = gcBrand NO-ERROR.
IF NOT AVAILABLE MobSub THEN
    RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

top_array = add_array(response_toplevel_id, "").

FOR EACH daycampaign NO-LOCK:
   liParams = 0.
   llgSVA = fIsSVA(daycampaign.dcevent, liParams).
   IF llgSVA EQ TRUE THEN DO:
      top_struct = add_struct(top_array, "").
      add_string(top_struct, "service_id", daycampaign.dcevent).

      FIND FIRST FMItem NO-LOCK WHERE
                 FMItem.Brand EQ gcBrand AND
                 FMItem.FeeModel EQ daycampaign.feemodel NO-ERROR.
      IF AVAIL FMItem THEN ldPrice = FMItem.Amount.
      ELSE ldPrice = 0.

      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsSeq EQ piMsSeq AND
                 /*MsRequest.ReqStatus EQ {&REQYEST_STATUS_DONE} AND*/ 
                 (MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} OR
                 MsRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION} ) AND
                 MsRequest.ReqCparam3 EQ daycampaign.dcname
                 USE-INDEX MsActStamp NO-ERROR.
      IF AVAIL MsRequest THEN DO:
         IF MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
            liStatus = 2. /*Pending activation*/
         ELSE IF MsRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION} AND
            MsRequest.ReqStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
            liStatus = 3. /*Pending deactivation*/
         ELSE IF MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE} THEN
            liStatus = 1. /* activation done*/
         ELSE liStatus = 0. /*Inactive*/
      END.
      ELSE liStatus = 0. /*Inactive, never requested activation*/

      add_double(top_struct, "price", ldPrice).
      add_int(top_struct, "status", liStatus).

      /*This can be covered with wider solution in near future when we
        implement mapping for contract categories*/
      add_string(top_struct, "category", "pro").
   END.
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
          
