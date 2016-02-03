{Syst/commali.i}
{Syst/eventval.i}
{Func/orderfunc.i}
DEFINE INPUT PARAMETER piOrderId AS INTEGER NO-UNDO.

DEFINE VARIABLE lcStatus AS CHARACTER NO-UNDO.

FIND Order WHERE
     Order.Brand   = gcBrand AND
     Order.OrderId = piOrderId
NO-LOCK.

IF Order.StatusCode NE "73" THEN DO:
   
   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "Order" AND
              TMSCodes.FieldName = "StatusCode" AND
              TMSCodes.CodeGroup = "Orders" AND
              TMSCodes.CodeValue = Order.StatusCode
   NO-LOCK NO-ERROR.
   
   IF AVAIL TMSCodes THEN lcStatus = TMSCodes.CodeName.
   ELSE lcStatus = "".
   
   MESSAGE "Cannot create MNP process with order status " + 
      Order.StatusCode " " + lcStatus  VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND CURRENT Order EXCLUSIVE-LOCK.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).

END.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).
fSetOrderStatus(Order.OrderId,"3").
IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).

FIND CURRENT Order NO-LOCK.
MESSAGE "New MNP process has been created" VIEW-AS ALERT-BOX.

fCleanEventObjects().
