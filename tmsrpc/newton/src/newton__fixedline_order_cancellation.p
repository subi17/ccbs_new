/**
 * cancellation of inflight orders (by orderId).
 *
 *
 * @input orderid;int;mandatory;order number to cancel
          sfid;string;mandatory;salesman id
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
DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN
   RETURN.
   
piOrderId = get_int(param_toplevel_id, "0").
pcSalesManId = get_string(param_toplevel_id, "1").
pcReason = get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.   

/* Busines logic validations */
{newton/src/findtenant.i YES ordercanal Order OrderId piOrderId}

FIND FIRST OrderFusion WHERE
           OrderFusion.Brand EQ Syst.Var:gcBrand AND
           OrderFusion.OrderID EQ piOrderId 
           NO-LOCK NO-ERROR.   
IF AVAIL OrderFusion THEN
   IF LOOKUP(OrderFusion.FixedStatus,"CERRADA,CERRADA PARCIAL,CANCELACION EN PROCESO,CANCELADA,En proceso,EN PROCESO - NO CANCELABLE,PENDIENTE CANCELAR") > 0 THEN
      RETURN appl_err("Invalid Fixedline status").
   
RUN Mc/fusion_order_cancel.p(Order.OrderID).
IF NOT RETURN-VALUE BEGINS "OK:" THEN
   RETURN appl_err(RETURN-VALUE).


add_boolean(response_toplevel_id, "", true).

FINALLY:
END.
