/* orderneeddoc.p    

*/
   
{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER ilSilent AS LOG NO-UNDO.

DEF VAR llOk AS LOG NO-UNDO.

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand AND 
           Order.OrderID = iiOrder EXCLUSIVE-LOCK NO-ERROR.

IF not avail order THEN DO:
   IF NOT ilSilent THEN
      MESSAGE
      "Unknown order ID " iiorder
      VIEW-aS ALERT-BOX.
   RETURN "Unknown order ID " + STRING(iiOrder).
END.

IF NOT ilSilent THEN DO:

   llOk = FALSE.
   MESSAGE "Do You want to move this order to queue More documentation needed?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN RETURN "".
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

fSetOrderStatus(Order.OrderId,"44").

fMarkOrderStamp(Order.OrderID,
                "Change",
                0.0).

IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhOrder).
   fCleanEventObjects().
END.

RETURN "".
