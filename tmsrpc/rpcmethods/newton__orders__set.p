/**
 * Set order values
 *
 * @input  int;mandatory;order id
           string;mandatory; user name
           struct;mandatory;order 
           struct;mandatory;ordercustomer
           struct;mandatory;memo 
 * @order  risk_code;string;optional; new risk code
           imei;string;optional;only allowed for pending fusion line orders from POS
           icc;string;optional;only allowed for pending fusion line orders from POS
 * @ordercustomer additional_documentation;int;optional;additional documentation id 
 * @memo   title;string;optional;
           reason;string;optional;
           content;string;optional;
 * @output boolean;True
 */

/* NOTE: There's Web side error translations for this RPC. YBU-3023 */

{xmlrpc/xmlrpc_access.i}
{timestamp.i}

DEF VAR lhBuff AS HANDLE NO-UNDO.
DEF VAR piOrderId AS INTEGER NO-UNDO. 
DEF VAR pcUserName AS CHARACTER NO-UNDO. 
DEF VAR pcOrderStruct AS CHARACTER NO-UNDO.
DEF VAR pcCustomerStruct AS CHARACTER NO-UNDO.  
DEF VAR pcMemoStruct AS CHARACTER NO-UNDO. 

DEF VAR lcOrderFields AS CHAR NO-UNDO. 
DEF VAR lcMemoFields AS CHAR NO-UNDO. 
DEF VAR lcCustomerFields AS CHAR NO-UNDO. 

DEF VAR pcRiskCode AS CHARACTER NO-UNDO INIT ?. 
DEF VAR pcICC AS CHAR NO-UNDO. 
DEF VAR pcIMEI AS CHAR NO-UNDO. 

DEF VAR pcTitle AS CHARACTER NO-UNDO. 
DEF VAR pcReason AS CHARACTER NO-UNDO.
DEF VAR pcContent AS CHARACTER NO-UNDO. 

DEF VAR piAdditionalDoc AS INT NO-UNDO INIT ?. 

IF validate_request(param_toplevel_id, "int,string,struct,struct,struct") EQ ? THEN RETURN.
ASSIGN
   piOrderId  = get_int(param_toplevel_id, "0")
   pcUserName = "VISTA_" + get_string(param_toplevel_id,"1")
   pcOrderStruct = get_struct(param_toplevel_id, "2")
   pcCustomerStruct = get_struct(param_toplevel_id, "3")
   pcMemoStruct = get_struct(param_toplevel_id,"4").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{commpaa.i}
katun = pcUserName.
gcbrand = "1".
{tmsconst.i}
&GLOBAL-DEFINE STAR_EVENT_USER katun 
{lib/eventlog.i}

/* validate order struct */
lcOrderFields = validate_request(pcOrderStruct,"risk_code,imei,icc").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcRiskCode = get_string(pcOrderStruct,"risk_code")
   WHEN LOOKUP("risk_code", lcOrderFields) > 0
      pcICC = get_string(pcOrderStruct,"icc")
   WHEN LOOKUP("icc", lcOrderFields) > 0
      pcIMEI = get_string(pcOrderStruct,"imei")  
   WHEN LOOKUP("imei", lcOrderFields) > 0.
IF gi_xmlrpc_error NE 0 THEN RETURN.
   
/* validate customer struct */
lcCustomerFields = validate_request(pcCustomerStruct,"additional_documentation").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP("additional_documentation", lcCustomerFields) > 0 THEN
   piAdditionalDoc = get_int(pcCustomerStruct,"additional_documentation").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* validate memo struct */
lcMemoFields = validate_request(pcMemoStruct,"title,reason,content").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF NUM-ENTRIES(lcMemoFields) >  0 THEN ASSIGN
   pcTitle = get_string(pcMemoStruct,"title") WHEN
      LOOKUP("title", lcMemoFields) > 0
   pcReason = get_string(pcMemoStruct,"reason") WHEN
      LOOKUP("reason", lcMemoFields) > 0
   pcContent = get_string(pcMemoStruct,"content") WHEN
      LOOKUP("content", lcMemoFields) > 0.
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* Busines logic validations */

FIND Order NO-LOCK WHERE
     Order.Brand = gcBrand AND
     Order.OrderId = piOrderId NO-ERROR.
IF NOT AVAIL Order THEN 
   RETURN appl_err(SUBST("Unknown Order id &1",STRING(piOrderId))).

IF TRIM(pcUserName) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF piAdditionalDoc NE ? THEN DO:
   
   FIND OrderCustomer WHERE 
        OrderCustomer.Brand = gcBrand AND 
        OrderCustomer.OrderId = piOrderId AND
        OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

   IF NOT AVAILABLE OrderCustomer THEN 
       RETURN appl_err("OrderCustomer not found").

   FIND CURRENT OrderCustomer EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF LOCKED OrderCustomer THEN RETURN appl_err("OrderCustomer record locked").
END.

IF NUM-ENTRIES(lcOrderFields) > 0 THEN DO:
   
   IF pcRiskCode NE ? AND Order.RiskCode EQ pcRiskCode THEN RETURN 
      appl_err("risk code is same as current value!").

   IF pcICC > "" THEN DO:

      FIND FIRST OrderFusion NO-LOCK WHERE
                 OrderFusion.Brand = Order.Brand AND
                 OrderFusion.OrderID = Order.OrderID NO-ERROR.
      IF NOT AVAIL OrderFusion THEN
         RETURN appl_err("ICC set is only allowed for fusion orders").
      
      IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN RETURN
         appl_err("Order is in wrong status, cannot update ICC").

      IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_FINALIZED} THEN
         RETURN appl_err("Wrong fusion order status, cannot update ICC").

      FIND FIRST SIM EXCLUSIVE-LOCK WHERE
                 SIM.brand EQ gcBrand AND
                 SIM.ICC EQ pcIcc AND
                 SIM.simstat EQ 1 NO-ERROR.
      IF NOT AVAILABLE SIM THEN
         RETURN appl_err(SUBST("SIM with ICC &1 not found or not free", pcIcc)).

      FIND orderaction NO-LOCK where
           orderaction.brand = order.brand and
           orderaction.orderid = order.orderid  and
           orderaction.itemtype = "simtype" NO-ERROR.
      IF AVAIL orderaction AND orderaction.itemkey NE sim.simart THEN DO:
         RELEASE SIM.
         RETURN appl_err("SIM type does not match").
      END.

   END.

   IF pcIMEI > "" THEN DO:
      
      IF NOT AVAIL OrderFusion THEN DO:
         FIND FIRST OrderFusion NO-LOCK WHERE
                    OrderFusion.Brand = Order.Brand AND
                    OrderFusion.OrderID = Order.OrderID NO-ERROR.
         IF NOT AVAIL OrderFusion THEN
            RETURN appl_err("IMEI set is only allowed for fusion orders").
      END.
      
      IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_FINALIZED} THEN
         RETURN appl_err("Wrong fusion order status, cannot update IMEI").
      
      FIND OrderAccessory EXCLUSIVE-LOCK WHERE
           OrderAccessory.Brand = gcBrand AND
           OrderAccessory.OrderId = Order.OrderId AND
           OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.

      IF AMBIGUOUS OrderAccessory THEN
         RETURN appl_err("Order includes more than one terminal").
      ELSE IF NOT AVAIL OrderAccessory THEN
         RETURN appl_err("Order doesn't contain any terminal").

      IF OrderAccessory.IMEI > "" THEN DO:
         RELEASE OrderAccessory.
         RETURN appl_err("IMEI already set").
      END.
   END.

END.

/* Updates */
IF pcRiskCode NE ? OR
   pcICC > "" THEN DO:

   FIND CURRENT Order EXCLUSIVE-LOCK.
   
   lhBuff = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhBuff).
   RUN StarEventSetOldBuffer(lhBuff).
   
   IF pcRiskCode NE ? THEN ASSIGN
      Order.RiskCode   = pcRiskCode
      Order.SendToROI  = {&ROI_HISTORY_TO_SEND} WHEN
         Order.OrderType NE {&ORDER_TYPE_STC}.
      
   IF pcICC > "" THEN DO:
      ASSIGN
         SIM.simstat = 4.
         Order.ICC = pcICC.
      RELEASE SIM.
   END.
   
   RUN StarEventMakeModifyEvent(lhBuff).

   FIND CURRENT Order NO-LOCK.
END.

IF pcICC > "" AND
   Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:

   RUN orderinctrl.p(Order.OrderId, 0, TRUE).

   IF RETURN-VALUE > "" THEN 
      UNDO, RETURN appl_err("Mobile order release failed").
   ELSE IF OrderFusion.FusionStatus EQ "PFIN" THEN DO:

      FIND CURRENT OrderFusion EXCLUSIVE-LOCK.
      
      lhBuff = BUFFER OrderFusion:HANDLE.
      RUN StarEventInitialize(lhBuff).
      RUN StarEventSetOldBuffer(lhBuff).
      ASSIGN
         OrderFusion.FusionStatus = "FIN"
         OrderFusion.UpdateTS = fMakeTS().
      RUN StarEventMakeModifyEvent(lhBuff).

      RELEASE OrderFusion.
   END.
END.

IF pcIMEI > "" THEN DO:
   lhBuff = BUFFER OrderAccessory:HANDLE.
   RUN StarEventInitialize(lhBuff).
   RUN StarEventSetOldBuffer(lhBuff).
   OrderAccessory.IMEI = pcIMEI.
   RUN StarEventMakeModifyEvent(lhBuff).
   RELEASE OrderAccessory.
END.

IF NUM-ENTRIES(lcMemoFields) > 0 THEN DO:
   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = "1"
      Memo.HostTable = "Order"
      Memo.KeyValue  = STRING(piOrderId)
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = pcUsername
      Memo.MemoTitle = pcTitle
      Memo.MemoText  = (pcReason + chr(10) + pcContent).
END.

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   fCleanEventObjects().
END.
