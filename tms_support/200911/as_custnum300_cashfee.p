{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Syst/eventval.i}

DEFINE var icFile AS CHARACTER NO-UNDO. 
icfile = "/apps/snet/200911/as_custnum300_problem.input".

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
END.

DEFINE VARIABLE ocError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liSourceCust AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTargetCust AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 
DEF VAR lcCalcObj       AS CHAR NO-UNDO INIT "CASHFEE".

input from value(icFile).

repeat:
   import unformatted lcLine.
   liMsSeq =     int(entry(1,lcLine,"|")).
   liSourceCust = int(entry(2,lcLine,"|")).
   liTargetCust = int(entry(3,lcLine,"|")).
   
   /* if invoice already created and subscription has now been created
      then transfer invoice to the actual customer */
   FIND Order WHERE 
        Order.MsSeq = liMsSeq NO-LOCK NO-ERROR.
   IF AVAILABLE Order AND Order.InvNum > 0 THEN DO:
   
      FIND MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
      FIND Invoice WHERE Invoice.InvNum = Order.InvNum NO-LOCK NO-ERROR.
   
      IF AVAILABLE MobSub AND AVAILABLE Invoice AND 
         Invoice.CustNum NE MobSub.InvCust
      THEN DO TRANS:

         IF llDoEvent THEN DO:
            RUN StarEventInitialize(lhInvoice).
            RUN StarEventInitialize(lhSingleFee).
         END.
       
         FOR EACH SingleFee EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
                  SingleFee.Brand     = gcBrand AND
                  SingleFee.HostTable = "Order" AND
                  SingleFee.KeyValue  = STRING(Order.OrderId) AND
                  SingleFee.CalcObj   = lcCalcObj:
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).
            SingleFee.CustNum = MobSub.InvCust.      
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSingleFee).
         END.

         FIND CURRENT Invoice EXCLUSIVE-LOCK.
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
         Invoice.CustNum = MobSub.InvCust.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
      
         FOR EACH Payment OF Invoice EXCLUSIVE-LOCK:
            Payment.CustNum = Invoice.CustNum.
         END.
        
         ocError = ocError + ", invoice transferred". 
     END.    
   END.

END.
fCleanEventObjects().

disp ocError.
