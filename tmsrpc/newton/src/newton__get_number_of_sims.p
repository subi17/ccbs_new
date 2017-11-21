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

DEF VAR pcTenant    AS CHAR NO-UNDO.
DEF VAR piOrderId   AS INT  NO-UNDO.
DEF VAR lcError     AS CHAR NO-UNDO.
DEF VAR lcOrderList AS CHAR NO-UNDO. 

DEFINE BUFFER bOrder         FOR Order.
DEFINE BUFFER bOrderCustomer FOR OrderCustomer.


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

lcOrderList = STRING(piOrderId).

RUN pCheckExtraLineOrders.

RUN pCheckAdditionalLineOrders.

PROCEDURE pCheckExtraLineOrders:

DEF VAR lcExtraMainLineCLITypes AS CHAR NO-UNDO. 
DEF VAR lcExtraLineCLITypes     AS CHAR NO-UNDO. 
DEF VAR liMultiSimTypeValue     AS INT  NO-UNDO INITIAL 0. 

   ASSIGN lcExtraMainLineCLITypes = fCParam("DiscountType","Extra_MainLine_CLITypes")
          lcExtraLineCLITypes     = fCParam("DiscountType","ExtraLine_CLITypes").

   IF LOOKUP(Order.CLIType,lcExtraMainLineCLITypes) > 0 AND 
      Order.MultiSimId                             <> 0 AND 
      Order.MultiSimType                            = {&MULTISIMTYPE_PRIMARY} THEN 
      liMultiSimTypeValue = {&MULTISIMTYPE_EXTRALINE}.
   ELSE IF LOOKUP(Order.CLIType,lcExtraLineCLITypes) > 0 AND
      Order.MultiSimId                              <> 0 AND
      Order.MultiSimType                             = {&MULTISIMTYPE_EXTRALINE} THEN
      liMultiSimTypeValue = {&MULTISIMTYPE_PRIMARY}.      

   IF liMultiSimTypeValue EQ 0 THEN LEAVE.

   FIND FIRST bOrder NO-LOCK WHERE 
              bOrder.Brand        = Syst.Var:gcBrand    AND 
              bOrder.OrderId      = Order.MultiSimId    AND 
              bOrder.MultiSimId   = Order.OrderId       AND
              bOrder.MultiSimType = liMultiSimTypeValue NO-ERROR.

   IF NOT AVAIL bOrder THEN 
      RETURN appl_err("Extraline associated linked order is not available").

   lcOrderList = lcOrderList + "," + STRING(bOrder.OrderId).

END PROCEDURE.

PROCEDURE pCheckAdditionalLineOrders:

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand                   AND
              OrderCustomer.OrderId = Order.OrderId                      AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

   IF NOT AVAIL OrderCustomer THEN 
      RETURN appl_err("OrderCustomer not available").

   IF fIsConvergenceTariff(Order.CLIType)            OR 
      LOOKUP(bOrder.CliType,{&ADDLINE_CLITYPES}) > 0 THEN DO:

      FOR EACH bOrderCustomer NO-LOCK WHERE
               bOrderCustomer.Brand      EQ Syst.Var:gcBrand         AND
               bOrderCustomer.CustId     EQ OrderCustomer.CustId     AND
               bOrderCustomer.CustIdType EQ OrderCustomer.CustIdType AND
               bOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
          EACH bOrder NO-LOCK WHERE
               bOrder.Brand      EQ Syst.Var:gcBrand        AND
               bOrder.OrderId    EQ bOrderCustomer.OrderId  AND
               bOrder.OrderType  NE {&ORDER_TYPE_RENEWAL},
         FIRST bCLIType WHERE bCLIType.Brand   EQ Syst.Var:gcBrand AND
                              bCLIType.CliType EQ bOrder.CLIType   NO-LOCK:

         IF LOOKUP(bOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN NEXT.

         IF LOOKUP(bOrder.CliType,{&ADDLINE_CLITYPES}) > 0 THEN DO:
         
             IF CAN-FIND(FIRST bOrderAction NO-LOCK WHERE
                               bOrderAction.Brand    = Syst.Var:gcBrand             AND
                               bOrderAction.OrderID  = bOrder.OrderId               AND
                               bOrderAction.ItemType = "AddLineDiscount"            AND 
                       (LOOKUP(bOrderAction.ItemKey,{&ADDLINE_DISCOUNTS_20}) > 0 OR 
                        LOOKUP(bOrderAction.ItemKey,{&ADDLINE_DISCOUNTS})    > 0 OR 
                        LOOKUP(bOrderAction.ItemKey,{&ADDLINE_DISCOUNTS_HM}) > 0))  THEN 
                lcOrderList = lcOrderList + "," + STRING(bOrder.OrderId).

         END.

      END. 

   END. 

END PROCEDURE.

/* IF liRequestID = 0 THEN DO:
   RETURN appl_err(lcError).
END. */

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
END.
