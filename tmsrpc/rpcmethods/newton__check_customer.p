/**
 * Check if customer may do an order.
 *
 * @input   person_id;string;mandatory;
            id_type;string;mandatory;
            self_employed;bool;mandatory;
            orders;int;mandatory;
            clitype;string;optional;
 *          
 * @output   check_customer;struct;mandatory; response structure
 * @check_customer order_allowed;boolean;mandatory;
                   subscription_limit;int;mandatory;
                   reason;string;optional;possible fail reason, returned if order_allowed = false
                   additional_line_allowed;string;mandatory;OK,NO_MAIN_LINE,NO_SUBSCRIPTIONS (OK is returned also if there's no active main line but a pending main line order)
 */

{xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
{Func/orderchk.i}
{Syst/tmsconst.i}
gcBrand = "1".

/* Input parameters */
DEF VAR pcPersonId       AS CHAR NO-UNDO.
DEF VAR pcIdType         AS CHAR NO-UNDO.
DEF VAR plSelfEmployed   AS LOG  NO-UNDO.
DEF VAR piOrders         AS INT  NO-UNDO.
DEF VAR pcCliType        AS CHAR NO-UNDO.

/* Local variable */
DEF VAR llOrderAllowed   AS LOG  NO-UNDO.
DEF VAR lcReason         AS CHAR NO-UNDO.
DEF VAR lcReturnStruct   AS CHAR NO-UNDO.
DEF VAR liSubLimit       AS INT  NO-UNDO.
DEF VAR lisubs           AS INT NO-UNDO. 
DEF VAR lcAddLineAllowed AS CHAR NO-UNDO. 
DEF VAR liActLimit       AS INT  NO-UNDO.
DEF VAR liacts           AS INT NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,boolean,int,string") EQ ?
   THEN RETURN.

pcPersonId     = get_string(param_toplevel_id, "0").
pcIdType       = get_string(param_toplevel_id, "1").
plSelfEmployed = get_bool(param_toplevel_id, "2").
piOrders       = get_int(param_toplevel_id, "3").
pcCliType      = get_string(param_toplevel_id, "4").

IF gi_xmlrpc_error NE 0 THEN RETURN.

llOrderAllowed = fSubscriptionLimitCheck(
   pcPersonId,
   pcIdType,
   plSelfEmployed,
   piOrders,
   OUTPUT lcReason,
   OUTPUT liSubLimit,
   OUTPUT lisubs,
   OUTPUT liActLimit,
   OUTPUT liActs).

/* Removed legacy main-additional line code, as it is not 
   required any more to support it */ 

IF LOOKUP(pcCliType,{&ADDLINE_CLITYPES}) > 0 THEN DO:
   IF fCheckExistingConvergent(pcIdType,pcPersonId,pcCliType) THEN 
      lcAddLineAllowed = "OK".
   ELSE IF fCheckOngoingConvergentOrder(pcIdType,pcPersonId,pcCliType) THEN 
      lcAddLineAllowed = "OK".
   ELSE lcAddLineAllowed = "NO_MAIN_LINE".
END.

IF lcAddLineAllowed = "" THEN DO:
      
   FOR EACH OrderCustomer NO-LOCK WHERE   
            OrderCustomer.Brand      EQ gcBrand AND 
            OrderCustomer.CustId     EQ pcPersonId AND
            OrderCustomer.CustIdType EQ pcIdType AND
            OrderCustomer.RowType    EQ 1,
      EACH  Order NO-LOCK WHERE
            Order.Brand              EQ gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND 
            Order.OrderType          NE {&ORDER_TYPE_STC} AND 
            Order.SalesMan NE "GIFT" AND
            LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0,
       FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = gcBrand AND
             CLIType.CLIType = Order.CLIType AND
             CLIType.LineType > 0,
       EACH OrderAction NO-LOCK WHERE 
            OrderAction.Brand = Order.Brand AND
            OrderAction.OrderId = Order.OrderID AND
            OrderAction.ItemType = "BundleItem":

         IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.Brand    = gcBrand                   AND
                           CLIType.CLIType  = OrderAction.ItemKey       AND
                           CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:
         lcAddLineAllowed = "OK".
         LEAVE.
      END.
   END.

END.

IF lcAddLineAllowed EQ "" THEN lcAddLineAllowed = "NO_SUBSCRIPTIONS".

lcReturnStruct = add_struct(response_toplevel_id, "").
add_boolean(lcReturnStruct, 'order_allowed', llOrderAllowed).
add_int(lcReturnStruct, 'subscription_limit', liSubLimit).
IF NOT llOrderAllowed THEN add_string(lcReturnStruct, 'reason',lcReason).
add_string(lcReturnStruct, 'additional_line_allowed', lcAddLineAllowed).

IF liSubs >= liSubLimit THEN
   add_boolean(lcReturnStruct,"subscription_limit_reached",TRUE).
ELSE
   add_boolean(lcReturnStruct,"subscription_limit_reached",FALSE).
IF liActs >= liActLimit THEN
   add_boolean(lcReturnStruct,"activation_limit_reached",TRUE).
ELSE
   add_boolean(lcReturnStruct,"activation_limit_reached",FALSE).


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
