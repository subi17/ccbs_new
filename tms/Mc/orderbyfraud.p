/* orderbyfraud.p    

*/
   
{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
{Func/msisdn.i}
{Func/ordercancel.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER ilSilent AS LOG NO-UNDO.
DEF INPUT PARAMETER icOrderStatus AS CHAR NO-UNDO.

DEF VAR llOk AS LOG NO-UNDO.
DEF VAR lcMessage AS CHAR NO-UNDO.

FIND Order WHERE 
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
   IF icOrderStatus = {&ORDER_STATUS_AUTO_CLOSED} THEN
      lcMessage = "Do You want to mark this order in auto close?".
   ELSE
      lcMessage = "Do You want to mark this order in control by fraud?".

   MESSAGE lcMessage
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

FUNCTION fReleaseMSISDN RETURNS LOGICAL :

   FIND FIRST MSISDN USE-INDEX OrderID WHERE
              MSISDN.Brand = gcBrand AND
              MSISDN.OrderId   = Order.OrderId AND 
              MSISDN.StatusCode = 2 EXCLUSIVE-LOCK NO-ERROR.   

   IF AVAIL MSISDN THEN DO:
      fMakeMsidnHistory(RECID(MSISDN)).
      IF llDoEvent THEN DO:
         DEFINE VARIABLE lhMSISDN AS HANDLE NO-UNDO.
         lhMSISDN = BUFFER MSISDN:HANDLE.
         RUN StarEventInitialize(lhMSISDN).
         RUN StarEventSetOldBuffer(lhMSISDN).
      END.
      ASSIGN MSISDN.StatusCode = 1
             MSISDN.CustNum    = 0
             MSISDN.ValidTo    = 99999999.99999.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSISDN).
   END.
   RETURN TRUE.
END FUNCTION.

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

fSetOrderStatus(Order.OrderId,icOrderStatus).

fMarkOrderStamp(Order.OrderID,
                "Close",
                0.0).
   
/* YDR-70  and YOT-680 */
IF (LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0
    OR Order.OrderChannel = "Renewal_POS")
    AND Order.OrderType <= 2 THEN DO:

   fReleaseIMEI(Order.OrderId).

   IF Order.OrderType <= 1 THEN DO:
      FIND SIM WHERE
           SIM.ICC = Order.ICC AND
           SIM.SimStat <> 1 NO-LOCK NO-ERROR.
      IF AVAIL SIM AND SIM.Stock = "RETAILER" THEN
         fReleaseSIM(Order.OrderId).
   END. /* IF Order.OrderType NE ({&ORDER_TYPE_RENEWAL}) THEN DO: */

   IF Order.OrderType EQ ({&ORDER_TYPE_NEW}) THEN fReleaseMSISDN().
  
END.

IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).
IF llDoEvent THEN fCleanEventObjects().

RETURN "".
