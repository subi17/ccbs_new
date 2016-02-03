/* orderrelease.p     25.06.08/aam 
    
   release order to normal delivery process
*/
   
{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.

DEF VAR llOk     AS LOG  NO-UNDO.
DEF VAR lcStatus AS CHAR NO-UNDO.

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand AND 
           Order.OrderID = iiOrder
           EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE Order THEN DO:
    MESSAGE "Unknown order ID" iiOrder
    VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

IF LOOKUP(Order.StatusCode,"22") = 0 THEN DO:
   MESSAGE "Order is not in proper status"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

llOk = FALSE.

MESSAGE "Do You want to release order to delivery process?"
VIEW-AS ALERT-BOX QUESTION
BUTTONS YES-NO
TITLE " Order " + STRING(Order.OrderID) + " "
SET llOk.

IF NOT llOk THEN RETURN.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               


IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

/* Assign Order que according to MNP status */
IF Order.MNPStatus = 0 
THEN lcStatus = "1".
ELSE lcStatus = "3".

/* Check if Order must go to status 20 or status 21*/
IF Order.CREventQty = 0 AND 
   Order.CredOk = FALSE THEN DO: /* Credit scoring is not tried yet */
   
   FIND FIRST OrderCustomer WHERE
      OrderCustomer.Brand = gcBrand AND
      OrderCustomer.OrderId = Order.OrderId AND
      OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
   IF OrderCustomer.CustidType = "CIF" THEN DO:
      FIND FIRST Customer WHERE
         Customer.Brand = gcBrand AND 
         Customer.OrgId = OrderCustomer.CustId AND
         Customer.CustIdType = OrderCustomer.CustIdType NO-LOCK NO-ERROR. 
      IF AVAIL Customer THEN DO:
         FIND FIRST MobSub WHERE
                    MobSub.Brand   = gcBrand AND
                    MobSub.AgrCust = Customer.CustNum
              NO-LOCK NO-ERROR.
         IF NOT AVAIL MobSub THEN lcStatus = "20".
         ELSE lcStatus = "21".
      END. /* IF AVAIL Customer THEN DO: */
      ELSE lcStatus = "20".
   END. /* IF OrderCustomer.CustidType = "CIF" THEN DO: */
END.

fSetOrderStatus(Order.OrderId,lcStatus).
fMarkOrderStamp(Order.OrderID,
               "Change",
                0.0).

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).

RELEASE Order.

fCleanEventObjects().

