/**
 * Set fusion order values
 *
 * @input order_id;int;mandatory;
          username;string;mandatory
          fixed_line_number;string;optional;
          fusion_order_status;string;optional;
          update_ts;string;optional;used to check if record has been modified (the value should be same as what is received from orders.get method)
 * @output string;empty=ok,any other text means that order fusion status change has bee failed
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR lhBuff             AS HANDLE NO-UNDO.
DEF VAR lcTopStruct        AS CHAR NO-UNDO. 
DEF VAR lcTopStructFields  AS CHAR NO-UNDO. 
DEF VAR pcUpdateTS         AS CHAR NO-UNDO. 
DEF VAR piOrderId          AS INTEGER NO-UNDO. 
DEF VAR pcUserName         AS CHARACTER NO-UNDO. 
DEF VAR llEqual            AS LOG NO-UNDO. 
DEF VAR lirequest          AS INT NO-UNDO. 
DEF VAR llReleaseMobile    AS LOG NO-UNDO. 
DEF VAR lcFusionError      AS CHAR NO-UNDO. 

DEFINE TEMP-TABLE ttOrderFusion LIKE OrderFusion.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
lcTopStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcTopStructFields = validate_struct(lcTopStruct, "username!,order_id!,fixed_line_number,fusion_order_status,update_ts").

ASSIGN
   piOrderId = get_int(lcTopStruct, "order_id")
   pcUserName = get_string(lcTopStruct, "username")
   pcUpdateTS =  get_string(lcTopStruct, "update_ts")
      WHEN LOOKUP("update_ts", lcTopStructFields) > 0.
IF gi_xmlrpc_error NE 0 THEN RETURN.

{commpaa.i}
katun = pcUserName.
gcbrand = "1".

{eventval.i}
{tmsconst.i}
{timestamp.i}
&GLOBAL-DEFINE STAR_EVENT_USER katun 

IF llDoEvent THEN DO:
   {lib/eventlog.i}
END.

/* check order exist */
FIND Order NO-LOCK WHERE
     Order.Brand = gcbrand AND
     Order.OrderId = piOrderId NO-ERROR.
IF NOT AVAIL Order THEN 
   RETURN appl_err(SUBST("Unknown Order id &1",STRING(piOrderId))).

FIND FIRST OrderFusion NO-LOCK WHERE
           OrderFusion.Brand = Order.Brand AND
           OrderFusion.OrderId = Order.OrderId NO-ERROr.
IF NOT AVAIL OrderFusion THEN 
   RETURN appl_err("Not fusion order").

IF TRIM(pcUserName) EQ "VISTA_" THEN RETURN appl_err("username is empty").

CREATE ttOrderFusion.
BUFFER-COPY OrderFusion TO ttOrderFusion.

ASSIGN
   ttOrderFusion.FixedNumber = get_string(lcTopStruct,"fixed_line_number") WHEN
      LOOKUP("fixed_line_number", lcTopStructFields) > 0
   ttOrderFusion.FusionStatus = get_string(lcTopStruct,"fusion_order_status") WHEN
      LOOKUP("fusion_order_status", lcTopStructFields) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcUpdateTS > "" AND
   STRING(ttOrderFusion.UpdateTS) NE pcUpdateTS THEN RETURN
   appl_err({&MSG_RECORD_CHANGED}).

BUFFER-COMPARE ttOrderFusion TO OrderFusion SAVE llEqual.
IF llEqual THEN DO:
   add_string(response_toplevel_id, "", "").
   RETURN.
END.

FIND CURRENT OrderFusion EXCLUSIVE-LOCK.

IF ttOrderFusion.FusionStatus NE 
   OrderFusion.FusionStatus THEN CASE ttOrderFusion.FusionStatus:

   WHEN "PCAN" THEN DO:
      IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN
         RETURN appl_err(SUBST("Incorrect Order status &1", Order.StatusCode)).
      IF LOOKUP(OrderFusion.FusionStatus, "NEW,INT,ERR,REJ") = 0 THEN      /* TODO Add list to tmscont.i */
         RETURN appl_err("Fusion order status change not allowed").
/* TODO If FusionStatus (PCAN) then TMS invoke Gwy/masmovil_cancel_order.p integration service to cancel the fixed line order in MasMovil system. */
   END.
   OTHERWISE RETURN appl_err(SUBST("Unsupported new fusion order status: &1",  ttOrderFusion.fusionstatus)).
END.

IF llDoEvent THEN DO:
   lhBuff = BUFFER OrderFusion:HANDLE.
   RUN StarEventInitialize(lhBuff).
   RUN StarEventSetOldBuffer(lhBuff).
END.

ttOrderFusion.UpdateTS = fMakeTS().

BUFFER-COPY ttOrderFusion USING
   FixedNumber
   FusionStatus 
   UpdateTS
TO OrderFusion. 

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBuff).

RELEASE OrderFusion.

add_string(response_toplevel_id, "", lcFusionError).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   EMPTY TEMP-TABLE ttOrderFusion.
   IF llDoEvent THEN fCleanEventObjects().
END.
