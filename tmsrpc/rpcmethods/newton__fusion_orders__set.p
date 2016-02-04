/**
 * Set fusion order values
 *
 * @input order_id;int;mandatory;
          username;string;mandatory
          fixed_line_number;string;optional;
          fusion_order_status;string;optional;
          fixed_line_order_id;string;optional;
          fixed_line_order_status;string;optional;
          fixed_line_order_sub_status;string;optional;
          external_ticket;string;optional;
          update_ts;string;optional;used to check if record has been modified (the value should be same as what is received from orders.get method)
 * @output string;empty=ok,any other text means that order fusion status change has bee failed
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR lhBuff AS HANDLE NO-UNDO.
DEF VAR pcOrderStruct AS CHARACTER NO-UNDO.
DEF VAR pcCustomerStruct AS CHARACTER NO-UNDO.  
DEF VAR pcMemoStruct AS CHARACTER NO-UNDO. 
DEF VAR lcError AS CHARACTER NO-UNDO.
DEF VAR lcTopStruct AS CHAR NO-UNDO. 
DEF VAR lcTopStructFields AS CHAR NO-UNDO. 
DEF VAR pcUpdateTS AS CHAR NO-UNDO. 

DEF VAR piOrderId AS INTEGER NO-UNDO. 
DEF VAR pcUserName AS CHARACTER NO-UNDO. 
DEF VAR llEqual AS LOG NO-UNDO. 
DEF VAR lirequest AS INT NO-UNDO. 
DEF VAR llReleaseMobile AS LOG NO-UNDO. 

DEF VAR lcFusionError AS CHAR NO-UNDO. 

DEFINE TEMP-TABLE ttOrderFusion LIKE OrderFusion.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
lcTopStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcTopStructFields = validate_struct(lcTopStruct, "username!,order_id!,fixed_line_number,fusion_order_status,fixed_line_order_id,fixed_line_order_status,fixed_line_order_sub_status,external_ticket,update_ts").

ASSIGN
   piOrderId = get_int(lcTopStruct, "order_id")
   pcUserName = get_string(lcTopStruct, "username")
   pcUpdateTS =  get_string(lcTopStruct, "update_ts")
      WHEN LOOKUP("update_ts", lcTopStructFields) > 0.
IF gi_xmlrpc_error NE 0 THEN RETURN.

{Syst/commpaa.i}
katun = pcUserName.
gcbrand = "1".
{Func/orderfunc.i} 
{Syst/eventval.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
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
      LOOKUP("fusion_order_status", lcTopStructFields) > 0
   ttOrderFusion.FixedOrderId = get_string(lcTopStruct,"fixed_line_order_id") WHEN
      LOOKUP("fixed_line_order_id", lcTopStructFields) > 0
   ttOrderFusion.FixedStatus = get_string(lcTopStruct,"fixed_line_order_status") WHEN
      LOOKUP("fixed_line_order_status", lcTopStructFields) > 0
   ttOrderFusion.FixedSubStatus = get_string(lcTopStruct,"fixed_line_order_sub_status") WHEN
      LOOKUP("fixed_line_order_sub_status", lcTopStructFields) > 0
   ttOrderFusion.ExternalTicket = get_string(lcTopStruct,"external_ticket") WHEN
      LOOKUP("external_ticket", lcTopStructFields) > 0.

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
      IF LOOKUP(OrderFusion.FusionStatus,"NEW,ONG") = 0 THEN
         RETURN appl_err("Fusion order status change not allowed").
   END.
   OTHERWISE RETURN appl_err(SUBST("Unsupported new fusion order status: &1",  ttOrderFusion.fusionstatus)).
END.

IF ttOrderFusion.FixedStatus > "" AND
   ttOrderFusion.FixedOrderId > "" AND
   ttOrderFusion.FusionStatus EQ "NEW" THEN ASSIGN
   ttOrderFusion.FusionStatus = "ONG"
   llReleaseMobile = TRUE.

IF ttOrderFusion.FixedStatus > "" AND
   OrderFusion.FixedStatus NE ttOrderFusion.FixedStatus
   THEN CASE ttOrderFusion.FixedStatus:
   
   WHEN "cancelled" THEN DO:

      IF LOOKUP(OrderFusion.fusionstatus,"NEW,ONG,PCAN") = 0 THEN
         lcFusionError = "Fusion order status change not allowed".
      ELSE DO:

         IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
            Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1} OR
            Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2} OR
            Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3} OR
            Order.StatusCode EQ {&ORDER_STATUS_IN_CONTROL} OR
            Order.StatusCode EQ {&ORDER_STATUS_MNP_REJECTED} OR
            Order.StatusCode EQ {&ORDER_STATUS_MORE_DOC_NEEDED} THEN DO:

            RUN Mc/closeorder.p(Order.OrderId,TRUE).

            IF RETURN-VALUE NE "" THEN
               lcFusionError = "Order closing failed: " + 
                                     STRING(RETURN-VALUE).
            ELSE ttOrderFusion.FusionStatus = "CAN".
         END.
         ELSE IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 
            THEN lcFusionError = "Order closing not allowed".
         ELSE IF LOOKUP(Order.StatusCode,{&ORDER_STATUS_DELIVERED}) > 0 THEN DO:

            RUN Mm/fusion_stc_fallback.p(Order.OrderId, OUTPUT liRequest).

            IF liRequest = 0 THEN
               lcFusionError = "Fallback STC request creation failed: " + 
                               STRING(RETURN-VALUE).
            ELSE ttOrderFusion.FusionStatus = "CAN".
         END.
         ELSE ttOrderFusion.FusionStatus = "CAN".
      END.
   END.
   WHEN "installed" THEN DO:
      
      IF LOOKUP(OrderFusion.FusionStatus,"ONG,PFIN") = 0 THEN
         lcFusionError = "Fusion order status change not allowed".
      ELSE DO:
         IF Order.OrderType EQ {&ORDER_TYPE_STC} OR
            Order.ICC > "" OR
            (Order.OrderChannel <> "Fusion_POS" AND Order.ICC = "") THEN DO:
      
            IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) = 0 AND
               Order.StatusCode NE {&ORDER_STATUS_IN_CONTROL} AND 
               Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN 
               ttOrderFusion.FusionStatus = "FIN".
            ELSE IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN
               lcFusionError = "Wrong mobile order status".
            ELSE DO:
               RUN Mc/orderinctrl.p(Order.OrderId, 0, TRUE).
               IF RETURN-VALUE > "" THEN
                  lcFusionError = "Mobile order release failed".
               ELSE ttOrderFusion.FusionStatus = "FIN".
            END.
         END.
         ELSE ttOrderFusion.FusionStatus = "PFIN".
      END.
   END.
END.

/* YBU-3072 */
IF llReleaseMobile AND
   Order.OrderType < 2 AND
   Order.ICC > "" AND 
   ttOrderFusion.FixedStatus NE "cancelled" AND
   Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:
   RUN Mc/orderinctrl.p(Order.OrderId, 0, TRUE).
   IF RETURN-VALUE > "" THEN
      lcFusionError = "Mobile order release failed".
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
   FixedOrderId
   FixedStatus
   FixedSubStatus
   ExternalTicket
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
