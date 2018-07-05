
/*------------------------------------------------------------------------
    File        : newton__fixedline_order_cancellation.p
    Purpose     : 

    Syntax      :

    Description : cancellation of inflight orders

    Author(s)   : 
    Created     : Wed Jul 04 11:26:01 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/**
 * cancellation of inflight orders (by orderId).
 *
 *
 * @input orderid;int;mandatory;order number to cancel
          sfid;string;mandatory;salesforce id of shop or callcenter
          reason;string;optional;reason for cancellation of order 
          
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

FIND FIRST OrderCustomer WHERE 
           OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
           OrderCustomer.OrderId EQ piOrderId        AND
           OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
           NO-LOCK NO-ERROR.     
IF NOT AVAILABLE OrderCustomer THEN 
   RETURN appl_err("Installation address possible for convergent order only").

FIND FIRST OrderFusion WHERE
           OrderFusion.Brand EQ Syst.Var:gcBrand AND
           OrderFusion.OrderID EQ piOrderId 
           NO-LOCK NO-ERROR.
IF NOT AVAIL OrderFusion THEN
   RETURN appl_err("Fixed line connection is not available for this order").
   
IF AVAIL OrderFusion THEN DO:
   IF LOOKUP(OrderFusion.FixedStatus,"CERRADA,CERRADA PARCIAL,CANCELACION EN PROCESO,CANCELADA,En proceso,EN PROCESO - NO CANCELABLE") > 0 THEN
      RETURN appl_err("Order is not in valid state to update").
END.

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