/* closeorder.p    

   changed:         22.11.06/aam fMarkOrderStamp, ask verification
                    30.11.06/aam validations
*/
   
{commali.i}
{eventval.i}
{orderfunc.i}
DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.

DEF VAR llOk AS LOG NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand AND 
           Order.OrderID = iiOrder NO-LOCK NO-ERROR.

IF not avail order THEN DO:
    MESSAGE
    "Unknown order ID " iiorder
    VIEW-aS ALERT-BOX.
    RETURN.
END.

llOk = FALSE.
MESSAGE "Do You want to mark this order in control?"
VIEW-AS ALERT-BOX QUESTION
BUTTONS YES-NO
TITLE " ORDER " + STRING(Order.OrderID) + " "
SET llOk.

IF NOT llOk THEN RETURN.

FIND CURRENT Order EXCLUSIVE-LOCK.

IF CURRENT-CHANGED Order THEN DO:
   MESSAGE {&MSG_RECORD_CHANGED}
   VIEW-AS ALERT-BOX ERROR.
   RETURN "".
END.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

fSetOrderStatus(Order.OrderId,"4").

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).

