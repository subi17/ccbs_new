{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/forderstamp.i}
{Func/orderfunc.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand AND 
           Order.OrderID = iiOrder EXCLUSIVE-LOCK NO-ERROR.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

fSetOrderStatus(Order.OrderId,"78").

fMarkOrderStamp(Order.OrderID,
                "Change",
                0.0).

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
