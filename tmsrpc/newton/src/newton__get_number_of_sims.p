/* ----------------------------------------------------------------------
  MODULE .......: newton__get_number_of_sims.p
  TASK .........: gets number of sims and devices to be delivered
  APPLICATION ..:
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 20.11.2017
  CHANGED ......:
  Version ......:
  ---------------------------------------------------------------------- */

/* @input string;mandatory;TenantId
          int;mandatory;OrderId   */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/orderfunc.i}
{Func/fixedlinefunc.i}
{Func/extralinefunc.i}

DEF VAR pcTenant           AS CHAR NO-UNDO.
DEF VAR piOrderId          AS INT  NO-UNDO.
DEF VAR lcError            AS CHAR NO-UNDO.
DEF VAR lcResultStruct     AS CHAR NO-UNDO. 
DEF VAR lcOrderList        AS CHAR NO-UNDO. 
DEF VAR liNoOfSims         AS INT  NO-UNDO. 
DEF VAR liNoOfDevices      AS INT  NO-UNDO. 
DEF VAR lcTerminalBillCode AS CHAR NO-UNDO. 

DEFINE BUFFER bOrder         FOR Order.
DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
DEFINE BUFFER bCLIType       FOR CLIType.
DEFINE BUFFER bOrderAction   FOR OrderAction.

IF validate_request(param_toplevel_id, "string,int") EQ ? THEN RETURN.

pcTenant  = get_string(param_toplevel_id, "0").
piOrderId = get_int(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF piOrderId EQ 0 THEN RETURN appl_err("OrderId is null").

{newton/src/settenant.i pcTenant}

FIND FIRST Order NO-LOCK WHERE 
           Order.Brand   = Syst.Var:gcBrand AND 
           Order.OrderId = piOrderId        NO-ERROR.

IF NOT AVAIL Order THEN 
   RETURN appl_err("OrderId is not available"). 

IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN 
   RETURN appl_err("Order is already closed or cancelled").

IF NOT fIsConvergenceTariff(Order.CLIType) THEN
   RETURN appl_err("OrderId provided is not mainline order").

FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand   = Syst.Var:gcBrand                   AND
           OrderCustomer.OrderId = Order.OrderId                      AND
           OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

IF NOT AVAIL OrderCustomer THEN 
   RETURN appl_err("OrderCustomer not available").

IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) EQ 0 THEN
   RETURN appl_err("Requested OrderId is not Direct channel order").

lcOrderList = STRING(piOrderId).

IF fIsTerminalOrder(Order.OrderId,
                    OUTPUT lcTerminalBillCode) THEN 
   liNoOfDevices = liNoOfDevices + 1.
ELSE DO: 
   IF Order.DeliverySecure EQ 0 THEN 
      liNoOfSims = liNoOfSims + 1.   
END.

RUN pCheckExtraLineOrders.

RUN pCheckAdditionalLineOrders.

PROCEDURE pCheckExtraLineOrders:

   IF fCLITypeIsMainLine(Order.CLIType) THEN   
   DO:
      FIND FIRST bOrder NO-LOCK WHERE 
                 bOrder.Brand        EQ Syst.Var:gcBrand                  AND 
                 bOrder.OrderId      EQ Order.MultiSimId                  AND 
                 bOrder.MultiSimId   EQ Order.OrderId                     AND
                 bOrder.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}         AND 
                 bOrder.StatusCode   EQ {&ORDER_STATUS_PENDING_MAIN_LINE} AND 
                 bOrder.OrderType    NE {&ORDER_TYPE_RENEWAL}             NO-ERROR.

      IF AVAIL bOrder AND 
               bOrder.DeliverySecure EQ 0 THEN  
         ASSIGN lcOrderList = lcOrderList + "," + STRING(bOrder.OrderId)
                liNoOfSims  = liNoOfSims + 1.
   END. 

END PROCEDURE.

PROCEDURE pCheckAdditionalLineOrders:

   FOR EACH bOrderCustomer NO-LOCK WHERE
            bOrderCustomer.Brand      EQ Syst.Var:gcBrand         AND
            bOrderCustomer.CustId     EQ OrderCustomer.CustId     AND
            bOrderCustomer.CustIdType EQ OrderCustomer.CustIdType AND
            bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bOrder NO-LOCK WHERE
            bOrder.Brand      EQ Syst.Var:gcBrand                  AND
            bOrder.OrderId    EQ bOrderCustomer.OrderId            AND
            bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_MAIN_LINE} AND 
            bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL},
      FIRST bCLIType WHERE bCLIType.Brand   EQ Syst.Var:gcBrand AND
                           bCLIType.CliType EQ bOrder.CLIType   NO-LOCK:

      IF bOrder.DeliverySecure > 0 THEN NEXT.

      IF LOOKUP(bOrder.CliType,{&ADDLINE_CLITYPES}) > 0 THEN DO:
      
          IF CAN-FIND(FIRST bOrderAction NO-LOCK WHERE
                            bOrderAction.Brand    = Syst.Var:gcBrand             AND
                            bOrderAction.OrderID  = bOrder.OrderId               AND
                            bOrderAction.ItemType = "AddLineDiscount"            AND 
                    (LOOKUP(bOrderAction.ItemKey,{&ADDLINE_DISCOUNTS_20}) > 0 OR 
                     LOOKUP(bOrderAction.ItemKey,{&ADDLINE_DISCOUNTS})    > 0 )) THEN 
             ASSIGN lcOrderList = lcOrderList + "," + STRING(bOrder.OrderId)
                    liNoOfSims  =  liNoOfSims + 1. 

      END.

   END. 

END PROCEDURE. 

lcResultStruct = add_struct(response_toplevel_id, "").

add_string(lcResultStruct,"OrderId",STRING(Order.OrderId)).
add_int(lcResultStruct,"NoOfSims",liNoOfSims).
add_int(lcResultStruct,"NoOfDevices",liNoOfDevices).
add_string(lcResultStruct,"OrderList",lcOrderList).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
END.
