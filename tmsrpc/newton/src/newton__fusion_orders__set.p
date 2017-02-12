/**
 * Set fusion order status (cancel fixed line order)
 *
 * @input order_id;int;mandatory;
          username;string;mandatory
          fusion_order_status;string;mandatory;supports CAN only
          update_ts;string;optional;used to check if record has been modified (the value should be same as what is received from orders.get method)
 * @output string;empty=ok,any other text means that order fusion status change has bee failed
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Mc/orderfusion.i}

DEF VAR lcTopStruct        AS CHAR NO-UNDO. 
DEF VAR lcTopStructFields  AS CHAR NO-UNDO. 
DEF VAR pcUpdateTS         AS CHAR NO-UNDO. 

DEF VAR piOrderId          AS INTEGER NO-UNDO. 
DEF VAR pcUserName         AS CHARACTER NO-UNDO. 
DEF VAR pcFusionStatus AS CHAR NO-UNDO. 
DEF VAR pdeUpdateTS AS DEC NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
lcTopStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcTopStructFields = validate_struct(lcTopStruct, "username!,order_id!,fusion_order_status!,update_ts").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   piOrderId = get_int(lcTopStruct, "order_id")
   pcUserName = get_string(lcTopStruct, "username")
   pcFusionStatus = get_string(lcTopStruct,"fusion_order_status")
   pcUpdateTS =  get_string(lcTopStruct, "update_ts")
      WHEN LOOKUP("update_ts", lcTopStructFields) > 0.
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

{Syst/commpaa.i}
katun = pcUserName.
gcbrand = "1".
{Syst/tmsconst.i}

IF TRIM(pcUserName) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF pcFusionStatus NE {&FUSION_ORDER_STATUS_PENDING_CANCELLED} THEN
   RETURN appl_err(SUBST("Unsupported new fusion order status: &1",  pcFusionStatus)).

IF pcUpdateTS > "" AND
   STRING(pdeUpdateTS) NE pcUpdateTS THEN RETURN
   appl_err({&MSG_RECORD_CHANGED}).

RUN Mc/fusion_order_cancel.p(piOrderId).
IF NOT RETURN-VALUE BEGINS "OK:" THEN 
   RETURN appl_err(RETURN-VALUE).

add_string(response_toplevel_id, "", "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
