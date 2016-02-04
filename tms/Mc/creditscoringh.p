/* ----------------------------------------------------------------------
  MODULE .......: creditscoringh.p
  TASK .........: Handles corporate customer credit scoring request.
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 02.06.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/orderfunc.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/flimitreq.i}

DEF INPUT PARAMETER piOrderId AS INT NO-UNDO.

DEFINE VARIABLE llOk          AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liEmployees   AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcAnswerCodes AS CHARACTER NO-UNDO. 

DEFINE VARIABLE ldeTime AS DECIMAL NO-UNDO.
DEFINE VARIABLE lcMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustId AS CHARACTER NO-UNDO.
DEF VAR lcNewOrderStatus AS CHAR NO-UNDO. 

DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
DEFINE BUFFER bOrder FOR Order.
DEFINE BUFFER lbOrder FOR Order.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER "CreditScoring"

   {lib/eventlog.i}

   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER bOrder:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.

FIND Order WHERE
     Order.Brand = gcBrand AND
     Order.OrderId = piOrderId NO-LOCK NO-ERROR.

RUN Mc/creditscoring.p(
   piOrderId,
   (IF Order.OrderType = 2 THEN "RENEWAL_STC" ELSE "ORDER"),
   OUTPUT llOk,
   OUTPUT liEmployees,
   OUTPUT lcAnswerCodes).

IF liEmployees < 10 THEN liEmployees = 10.

ldeTime = fMakeTS().

FIND FIRST OrderCustomer WHERE 
   OrderCustomer.Brand = gcBrand AND
   OrderCustomer.OrderId = piOrderId AND
   OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

lcCustId = OrderCustomer.CustId.

IF Order.OrderType EQ 2 THEN DO:
   RUN pHandleOrder.
END.
ELSE DO:
   FOR EACH Order WHERE 
      Order.Brand = gcBrand AND
      Order.StatusCode = "20" NO-LOCK:
      RUN pHandleOrder.
   END.

   FOR EACH Order WHERE 
      Order.Brand = gcBrand AND
      Order.StatusCode = "21" NO-LOCK:
      RUN pHandleOrder.
   END.
END.
      
IF llDoEvent THEN fCleanEventObjects().

PROCEDURE pHandleOrder:

   FIND FIRST OrderCustomer WHERE 
      OrderCustomer.Brand   = gcBrand AND
      OrderCustomer.OrderId = Order.OrderId AND
      OrderCustomer.RowType = 1 AND
      OrderCustomer.CustId  = lcCustId NO-LOCK NO-ERROR.

   IF NOT AVAIL OrderCustomer THEN RETURN.
  
   FIND FIRST bOrder WHERE ROWID(bOrder) = ROWID(Order)
   EXCLUSIVE-LOCK NO-ERROR.
  
   ASSIGN 
      bOrder.CredOK = llOk.
      bOrder.CREventQty = bOrder.CREventQty + 1. 
   
   IF llOk THEN DO:

      CASE bOrder.StatusCode:
      
         WHEN "20" THEN DO:
         
            FIND bOrderCustomer WHERE 
               ROWID(bOrderCustomer) = ROWID(OrderCustomer)
            EXCLUSIVE-LOCK NO-ERROR.
            
            bOrderCustomer.SubQty = liEmployees.
            
            RELEASE bOrderCustomer.
         END.
      
         WHEN "21" THEN DO:
         
            FIND FIRST Customer WHERE
               Customer.Brand = gcBrand AND
               Customer.OrgId = OrderCustomer.CustId AND
               Customer.CustIdType = "CIF" AND
               Customer.Role NE "inactive" NO-LOCK NO-ERROR.
         
            IF AVAIL Customer THEN DO:
            
               FIND FIRST Limit WHERE
                          Limit.Custnum = Customer.Custnum AND
                          Limit.LimitType = {&LIMIT_TYPE_SUBQTY} AND
                          Limit.ToDate >= TODAY NO-LOCK NO-ERROR.
               IF NOT AVAIL Limit THEN DO:
                  fCreateLimit(Customer.Custnum,
                      0,
                      {&LIMIT_TYPE_SUBQTY},
                      liEmployees,
                      0,
                      0,
                      TODAY,
                      12/31/2049).
               END.
            END.

         END.

      END CASE.
      
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

      lcNewOrderStatus = "".

      IF bOrder.MultiSIMId > 0 AND
         bOrder.MultiSIMType = {&MULTISIMTYPE_SECONDARY} THEN DO:

         FIND FIRST lbOrder NO-LOCK WHERE
                    lbOrder.Brand = gcBrand AND
                    lbOrder.MultiSIMId = bOrder.MultiSIMId AND
                    lbOrder.MultiSImType = {&MULTISIMTYPE_PRIMARY} NO-ERROR.
         IF AVAIL lbOrder AND
                  lbOrder.StatusCode NE {&ORDER_STATUS_DELIVERED} THEN
            lcNewOrderStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
      END.
      ELSE IF bOrder.Ordertype < 2 AND
         CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.Brand = gcBrand AND
                        CLIType.CLIType = bOrder.CLIType AND
                        CLIType.LineType > 0) AND
         NOT CAN-FIND(FIRST OrderAction WHERE
                      OrderAction.Brand = gcBrand AND
                      OrderAction.OrderId = bOrder.OrderID AND
                      OrderAction.ItemType = "BundleItem" AND
                      CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                     CLIType.Brand = gcBrand AND
                                     CLIType.CLIType = OrderAction.ItemKey AND
                                     CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}))
                      THEN DO:
            
         IF NOT fIsMainLineSubActive(
            OrderCustomer.CustIDType,
            OrderCustomer.CustId) THEN 
            lcNewOrderStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
      END.
      ELSE IF bOrder.Ordertype = {&ORDER_TYPE_MNP} AND
         bOrder.PortingDate <> ? THEN
         lcNewOrderStatus = {&ORDER_STATUS_MNP_ON_HOLD}.

      IF bOrder.OrderChannel BEGINS "fusion" THEN
          fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_PENDING_FIXED_LINE}).
      ELSE IF lcNewOrderStatus > "" THEN
         fSetOrderStatus(bOrder.OrderId,lcNewOrderStatus).
      ELSE IF bOrder.MNPStatus NE 0 THEN
         fSetOrderStatus(bOrder.OrderId,"3").
      ELSE DO:
         IF bOrder.OrderType EQ 2 THEN DO:
            IF LOOKUP(bOrder.OrderChannel,"renewal_pos_stc,retention_stc") > 0
            THEN fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_RENEWAL_STC}).
            ELSE
               fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_RENEWAL}).
         END.
         ELSE fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_NEW}).
      END.

      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
   
   END.

   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = ldeTime
      Memo.Brand     = gcBrand 
      Memo.HostTable = "Order"
      Memo.KeyValue  = STRING(bOrder.OrderId)
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = katun 
      Memo.MemoTitle = "Credit scoring " + (IF llOk THEN "ok" ELSE "fail")
      Memo.MemoText  = lcAnswerCodes.

   RELEASE bOrder.
   
END PROCEDURE. 

IF llOk THEN lcMessage = "Credit scoring succeeded".
ELSE lcMessage = "Credit scoring not succeeded".

MESSAGE lcMessage VIEW-AS ALERT-BOX.
