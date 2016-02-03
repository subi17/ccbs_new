{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/orderfunc.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.

DEF VAR llOK AS LOG NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               


DEF BUFFER BufOrder FOR Order.

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand and 
           Order.OrderID = iiOrder EXCLUSIVE-LOCK NO-ERROR.

MESSAGE "Oletko varma että palvelusopimus allekirjoitettu "
VIEW-AS ALERT-BOX buttons YES-no update llok.
               
IF llOK THEN DO:
                        
   FIND FIRST MSISDN WHERE
              MSISDN.CLI = Order.CLI  NO-LOCK NO-ERROR.
 
   IF AVAIL MSISDN AND
            MSISDN.StatusCode NE 8 THEN DO:
                                
      llOK = FALSE.
                                              
      MESSAGE
      "MSISDN Number " Order.cli " NOT "                  SKIP
      "ready for rescue handling (wrong statuscode)"      SKIP
      "Are you sure that you want to activate this number?"
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE llOK.
   END.
   ELSE  DO:
      MESSAGE 
      "Unknown number:" Order.cli  SKIP
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

   IF not llOK THEN DO:
      LEAVE.
   END.

   FIND FIRST BufOrder WHERE
        RECID(BufOrder) = RECID(Order) EXCLUSIVE-LOCK.
   
   CREATE BufOrder.
   BUFFER-COPY order Except Orderid TO BufOrder.
                      
   ASSIGN
      BufOrder.CrStamp       = fMakeTS()
      BufOrder.Orderid       = NEXT-VALUE(OrderId)
      BufOrder.tupas         = 8
      BufOrder.CredOK        = TRUE
      BufOrder.source        = "RESCUE".

   /* Remove hardcoding and call the function to change the statuscode */
   fSetOrderStatus(BufOrder.OrderId,"3").

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).
      
      /* Remove hardcoding and call the function to change the statuscode */
      fSetOrderStatus(Order.OrderId,"79").

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrder).

END.   
