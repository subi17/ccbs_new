/**
 * cancellation of inflight orders (by orderId).
 *
 *
 * @input orderid;int;mandatory;order number to cancel
          sfid;string;mandatory;salesforce id of shop or callcenter
          reason;string;mandatory;reason for cancellation of order 
          
 * @output success;int 0 = successful;otherwise error
 
 **/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
{Func/fixedlinefunc.i}
{Func/ordercancel.i}
Syst.Var:gcBrand = "1".

/*Input Parameters*/
DEF VAR piOrderId    AS INT  NO-UNDO.
DEF VAR pcSalesManId AS CHAR NO-UNDO.
DEF VAR pcReason     AS CHAR NO-UNDO.

/*Local Variables*/
DEF VAR llCloseOrder AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN
   RETURN.
   
piOrderId = get_int(param_toplevel_id, "0").
pcSalesManId = get_string(param_toplevel_id, "1").
pcReason = get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.   

/* Busines logic validations */
{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

FIND FIRST Order WHERE 
           Order.Brand EQ Syst.Var:gcBrand AND
           Order.OrderId EQ piOrderId 
           NO-LOCK NO-ERROR.
IF NOT AVAIL Order THEN 
   RETURN appl_err("OrderId is invalid").

IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} AND
   Order.StatusCode NE {&ORDER_STATUS_COMPANY_NEW} AND
   Order.StatusCode NE {&ORDER_STATUS_COMPANY_MNP} AND
   Order.StatusCode NE {&ORDER_STATUS_ROI_LEVEL_1} AND
   Order.StatusCode NE {&ORDER_STATUS_ROI_LEVEL_2} AND
   Order.StatusCode NE {&ORDER_STATUS_ROI_LEVEL_3} AND
   Order.StatusCode NE {&ORDER_STATUS_MORE_DOC_NEEDED} THEN
   RETURN appl_err("Order is not in valid state to cancel").   

FIND FIRST OrderCustomer WHERE 
           OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
           OrderCustomer.OrderId EQ piOrderId        AND
           OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
           NO-LOCK NO-ERROR.     
IF NOT AVAILABLE OrderCustomer THEN 
   RETURN appl_err("Not a fixedline Order to Cancel").

FIND FIRST OrderFusion WHERE
           OrderFusion.Brand EQ Syst.Var:gcBrand AND
           OrderFusion.OrderID EQ piOrderId 
           NO-LOCK NO-ERROR.
IF NOT AVAIL OrderFusion THEN
   RETURN appl_err("Fixed line connection is not available for this order").

IF LOOKUP(OrderFusion.FusionStatus, "NEW,INT") EQ 0 THEN
   RETURN appl_err("Fusion status is not in valid state to cancel").

IF LOOKUP(OrderFusion.FixedStatus,"CERRADA,CERRADA PARCIAL,CANCELACION EN PROCESO,CANCELADA,En proceso,EN PROCESO - NO CANCELABLE,PENDIENTE CANCELAR") > 0 THEN
      RETURN appl_err("Fixedline Status is not in valid state to cancel").

FIND FIRST CliType WHERE
           CliType.Brand EQ Syst.Var:gcBrand AND
           CliType.CliType = Order.CliType AND
           CliType.TariffType EQ {&CLITYPE_TARIFFTYPE_MOBILEONLY} 
           NO-LOCK NO-ERROR.
IF AVAIL CliType THEN
   RETURN appl_err("Invalid TariffType").
   
IF NOT fIsConvergenceTariff(Order.CLIType) THEN 
   RETURN appl_err("Only Convergent Orders are allowed for cancellation" ).   
   
 IF llCloseOrder THEN DO:
   RUN Mc/closeorder.p (INPUT piOrderId, INPUT TRUE).
   ocResult = RETURN-VALUE. 
   IF ocResult NE "" THEN 
     RETURN appl_err(ocResult).
   ELSE DO:
      fReleaseImei(Order.OrderId).
      add_boolean(response_toplevel_id, "", true).
   END.
  RETURN .
END. /* IF llCloseOrder THEN DO: */
