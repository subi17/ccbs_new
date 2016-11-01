/**
 * Add or or remove subscription barrings
 *
 * @Input string;mandatory;user code
          int;mandatory;subscription id
          data;string;mandatory;barring commands (barr_x=1,barr_y=0,...)
 * @output boolean;true

 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/barrfunc.i}
katun    = "NewtonAd".
gcBrand  = "1".

/* Input parameters */
DEF VAR pcUser AS CHAR NO-UNDO.
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR pcCommand AS CHAR NO-UNDO.

/* Local variables */
DEF VAR lcBarringCode AS CHAR NO-UNDO.
DEF VAR lcSetStatus  AS CHAR NO-UNDO.

/* Output parameters */
DEF VAR top_array       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.
DEF VAR liReq           AS INT NO-UNDO.

IF validate_request(param_toplevel_id, "string,int,string") EQ ? THEN RETURN.

/*input data*/
pcUser = get_string(param_toplevel_id, "0").
piMsSeq = get_pos_int(param_toplevel_id, "1").
pcCommand = get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUser) EQ "VISTA_" THEN RETURN appl_err("username is empty").

katun = pcUser.

FIND FIRST Mobsub NO-LOCK WHERE
           Mobsub.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAILABLE Mobsub THEN
    RETURN appl_err("Subscription not found").

/*YPR-4774*/
/*(De)Activation is not allowed if fixed line provisioning is pending*/
IF MobSub.MsStatus EQ {&MSSTATUS_FIXED_PROV_ONG} /*16*/ THEN
   RETURN appl_err("Mobile line provisioning is not complete").

IF fIsReasonableSet(pcCommand, MobSub.MsSeq) EQ FALSE THEN
   RETURN appl_err("Barring status already active/inactive").

/*Barrengine makes required validations*/
RUN Mm/barrengine.p(MobSub.MsSeq,
                 pcCommand,
                 {&REQUEST_SOURCE_NEWTON},
                 "",
                 fMakeTS(),
                 "",
                 OUTPUT lcSetStatus).

liReq = INT(lcSetStatus) NO-ERROR.
IF liReq > 0 THEN lcSetStatus = "".
IF lcSetStatus NE "" THEN RETURN appl_err(lcSetStatus).


/*response*/
add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
