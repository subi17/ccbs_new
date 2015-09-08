/* ----------------------------------------------------------------------
  MODULE .......: orderhold.p 
  TASK .........: Release order from hold statuses
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 26.08.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{eventval.i}
{timestamp.i}
{forderstamp.i}
{orderfunc.i}
DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER icAction AS CHAR NO-UNDO.

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand and 
           Order.OrderID = iiOrder NO-LOCK NO-ERROR.

DEF VAR llOk AS LOG NO-UNDO.
DEF VAR lcMsg AS CHAR NO-UNDO. 
DEF VAR lcNewOrderStatus AS CHAR NO-UNDO. 

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
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               

FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

IF Order.StatusCode EQ {&ORDER_STATUS_RESIGNATION} THEN DO:
   CREATE ActionLog.
   ASSIGN
      ActionLog.Brand  = gcBrand 
      ActionLog.TableName = "Order"
      ActionLog.KeyValue = STRING(Order.OrderID)
      ActionLog.ActionId = "RESIGNATION"
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
      ActionLog.ActionTS = fMakeTS().
END.

/* release fusion company order */
IF (Order.StatusCode EQ "20" OR
    Order.StatusCode EQ "21") AND
    Order.OrderChannel BEGINS "fusion" AND 
    CAN-FIND(FIRST OrderCustomer NO-LOCK WHERE
                   OrderCustomer.Brand = gcBrand AND
                   OrderCustomer.OrderId = Order.OrderId AND
                   OrderCustomer.RowType = 1 AND
                   OrderCustomer.CustidType = "CIF") THEN DO:

   lcNewOrderStatus = {&ORDER_STATUS_PENDING_FIXED_LINE}.

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand   = Order.Brand AND
              OrderFusion.OrderId = Order.OrderID NO-ERROR.
   IF AVAIL OrderFusion AND
            OrderFusion.FusionStatus EQ "" THEN DO:

      FIND CURRENT OrderFusion EXCLUSIVE-LOCK.
      ASSIGN
         OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_NEW}
         OrderFusion.UpdateTS = fMakeTS().
      FIND CURRENT OrderFusion NO-LOCK.
   END.

END.
ELSE IF Order.OrderType = 2 THEN DO:
   
   IF Order.statuscode EQ {&ORDER_STATUS_RENEWAL_HOLD} THEN DO:
      FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                 OrderCustomer.Brand = gcBrand AND
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
              lbOrder.Brand = gcBrand AND
              lbOrder.MultiSIMId = Order.MultiSIMId AND
              lbOrder.MultiSImType = {&MULTISIMTYPE_PRIMARY} NO-ERROR.
   IF AVAIL lbOrder AND
            lbOrder.StatusCode NE {&ORDER_STATUS_DELIVERED} THEN
      lcNewOrderStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
END.
ELSE IF Order.Ordertype < 2 AND
   CAN-FIND(FIRST CLIType NO-LOCK WHERE
                  CLIType.Brand = gcBrand AND
                  CLIType.CLIType = Order.CLIType AND
                  CLIType.LineType > 0) AND
   NOT CAN-FIND(FIRST OrderAction WHERE
                     OrderAction.Brand = gcBrand AND
                     OrderAction.OrderId = Order.OrderID AND
                     OrderAction.ItemType = "BundleItem" AND
       CAN-FIND(FIRST CLIType NO-LOCK WHERE
                      CLIType.Brand = gcBrand AND
                      CLIType.CLIType = OrderAction.ItemKey AND
                      CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}))
                   THEN DO:
      
   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand = gcBrand AND
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

IF lcNewOrderStatus > "" THEN
   fSetOrderStatus(Order.OrderId,lcNewOrderStatus).
ELSE IF order.mnpstatus NE 0 THEN
   /* mnp order */
   fSetOrderStatus(Order.OrderId,"3").
ELSE 
   /* normal order */
   fSetOrderStatus(Order.OrderId,"1").

fMarkOrderStamp(Order.OrderID,
                "Change",
                0.0).
      
IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
fCleanEventObjects().

RETURN "".
