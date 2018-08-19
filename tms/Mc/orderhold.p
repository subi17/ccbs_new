/* ----------------------------------------------------------------------
  MODULE .......: orderhold.p 
  TASK .........: Release order from hold statuses
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 26.08.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
{Mc/orderfusion.i}
{Func/main_add_lines.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER icAction AS CHAR NO-UNDO.

FIND FIRST Order WHERE 
           Order.Brand   = Syst.Var:gcBrand and 
           Order.OrderID = iiOrder NO-LOCK NO-ERROR.

DEF VAR llOk AS LOG NO-UNDO.
DEF VAR lcMsg AS CHAR NO-UNDO. 
DEF VAR lcNewOrderStatus AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 

DEF BUFFER lbOrder FOR Order.

IF icAction NE "RELEASE_BATCH" THEN DO:

   CASE icAction:
      WHEN "RELEASE" THEN lcMsg = "Do you want to release order ?".
      WHEN "MODIFY" THEN lcMsg = "Do you want to change customer data ?".
      OTHERWISE DO:
         MESSAGE "Incorrect action:" icAction VIEW-AS ALERT-BOX ERROR.
         RETURN "ERROR:Incorrect action: " + icAction.
      END.
   END.

   llOk = FALSE.
   MESSAGE lcMsg
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN DO:
      RETURN "".
   END.
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               

FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

IF Order.StatusCode EQ {&ORDER_STATUS_RESIGNATION} THEN DO:
   CREATE ActionLog.
   ASSIGN
      ActionLog.Brand  = Syst.Var:gcBrand 
      ActionLog.TableName = "Order"
      ActionLog.KeyValue = STRING(Order.OrderID)
      ActionLog.ActionId = "RESIGNATION"
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
      ActionLog.ActionTS = Func.Common:mMakeTS().
END.

/* release fusion company order */
IF (Order.StatusCode EQ "20" OR
    Order.StatusCode EQ "21") AND
    Order.OrderChannel BEGINS "fusion" THEN DO:

   lcNewOrderStatus = {&ORDER_STATUS_PENDING_FIXED_LINE}.

END.
ELSE IF Order.OrderType = 2 THEN DO:
   
   IF Order.statuscode EQ {&ORDER_STATUS_RENEWAL_HOLD} THEN DO:
      FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                 OrderCustomer.Brand = Syst.Var:gcBrand AND
                 OrderCustomer.OrderId = Order.OrderId AND
                 OrderCustomer.RowType = 1 NO-ERROR.
      OrderCustomer.DataChecked = (icAction = "MODIFY").
      FIND CURRENT OrderCustomer NO-LOCK.
   END.
   /* renewal + stc */
   IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN DO:
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq = Order.MsSeq AND
                 MsRequest.ReqType = 0 AND
                 LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0
      NO-LOCK NO-ERROR.
      IF AVAIL MsRequest THEN DO:
         lcNewOrderStatus = {&ORDER_STATUS_RENEWAL_STC}.
         IF NOT MsRequest.ReqIParam2 > 0 THEN DO:
            FIND CURRENT MsRequest EXCLUSIVE-LOCK.
            MsRequest.ReqIParam2 = Order.OrderId.
            RELEASE MsRequest.
         END.
      END.
      ELSE lcNewOrderStatus = {&ORDER_STATUS_RENEWAL}.
   END.
   ELSE lcNewOrderStatus = {&ORDER_STATUS_RENEWAL}.
END.   
ELSE IF Order.MultiSIMId > 0 AND
        Order.MultiSIMType = {&MULTISIMTYPE_SECONDARY} THEN DO:

   FIND FIRST lbOrder NO-LOCK WHERE
              lbOrder.Brand = Syst.Var:gcBrand AND
              lbOrder.MultiSIMId = Order.MultiSIMId AND
              lbOrder.MultiSImType = {&MULTISIMTYPE_PRIMARY} NO-ERROR.
   IF AVAIL lbOrder AND
            lbOrder.StatusCode NE {&ORDER_STATUS_DELIVERED} THEN
      lcNewOrderStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
END.
ELSE IF Order.Ordertype < 2 AND
   CAN-FIND(FIRST CLIType NO-LOCK WHERE
                  CLIType.Brand       = Syst.Var:gcBrand       AND
                  CLIType.CLIType     = Order.CLIType AND
                 (CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
                  CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL})) AND
   NOT CAN-FIND(FIRST OrderAction WHERE
                     OrderAction.Brand = Syst.Var:gcBrand AND
                     OrderAction.OrderId = Order.OrderID AND
                     OrderAction.ItemType = "BundleItem" AND
       CAN-FIND(FIRST CLIType NO-LOCK WHERE
                      CLIType.Brand = Syst.Var:gcBrand AND
                      CLIType.CLIType = OrderAction.ItemKey AND
                      CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}))
                   THEN DO:
      
   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = 1 NO-ERROR.

   IF NOT fIsMainLineSubActive(
      OrderCustomer.CustIDType,
      OrderCustomer.CustId) THEN 
      lcNewOrderStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
END.
ELSE IF Order.Ordertype = {&ORDER_TYPE_MNP} AND
   Order.PortingDate <> ? THEN
   lcNewOrderStatus = {&ORDER_STATUS_MNP_ON_HOLD}.

IF lcNewOrderStatus > "" THEN DO:

   fSetOrderStatus(Order.OrderId,lcNewOrderStatus).
   fSetAllOrderProductsStatus(Order.OrderId,lcNewOrderStatus).
   
   IF lcNewOrderStatus EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:

      FIND OrderFusion NO-LOCK WHERE
           OrderFusion.Brand   = Order.Brand AND
           OrderFusion.OrderId = Order.OrderID NO-ERROR.

      IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_NEW} THEN DO:

         IF OrderFusion.FixedNumber EQ "" THEN
            fCreateFusionReserveNumberMessage(Order.OrderID,
                                              OUTPUT lcError).
         ELSE
            fCreateFusionCreateOrderMessage(Order.OrderId,
                                            OUTPUT lcError).
      
         IF lcError NE "" THEN 
            Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             0,
                             "Masmovil message creation failed",
                             lcError).
      END.
   END.
END.
ELSE IF order.mnpstatus NE 0 THEN
DO:
    fSetOrderStatus(Order.OrderId,"3").
    fSetAllOrderProductsStatus(Order.OrderId,lcNewOrderStatus).
END.
ELSE 
DO:
    fSetOrderStatus(Order.OrderId,"1").
    fSetAllOrderProductsStatus(Order.OrderId,lcNewOrderStatus).
END.     

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
fCleanEventObjects().

RETURN "".
