/* credithold.p             
   changed:             20.11.06/aam new db structure, ask verification
*/

{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER ilOrder AS LOG NO-UNDO.

DEF VAR llOk AS LOG NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand and 
           Order.OrderID = iiOrder EXCLUSIVE-LOCK NO-ERROR.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).


IF ilOrder  /* HOLD */ THEN DO:

   llOk = FALSE.
   MESSAGE "Do You want to mark this order to credit hold?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN RETURN.

  fSetOrderStatus(Order.OrderId,"10").
   
   fMarkOrderStamp(Order.OrderID,
                   "Change",
                   0.0).
                                
   RUN Mc/memo.p(INPUT Order.Custnum,
            INPUT "ORDER" ,
            INPUT STRING(Order.OrderID),
            INPUT "CreditHold").   
END.
ELSE DO:

   llOk = FALSE.
   MESSAGE "Do You want to mark this order checked?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN RETURN.
   
   fSetOrderStatus(Order.OrderId,"3").
 
   fMarkOrderStamp(Order.OrderID,
                   "Change",
                   0.0).
END.
      
IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).

