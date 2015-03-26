
 /* Display Order */
 DEFINE VARIABLE liOrderId AS INTEGER NO-UNDO. 
 DEFINE VARIABLE lcCurrentOfferId AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE lcNewOfferId AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE lcCurrentBillCode AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE lcNewBillCode AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE liInvNum AS INTEGER NO-UNDO. 

 liOrderId =  2217455.
 lcCurrentOfferId = "P075F23M2TVN0".
 lcCurrentBillCode = "TS0000001".

 lcNewOfferId = "P075F23M2TVP0".
 lcNewBillCode = "TS0000002".


/* check Order OfferId */
 FIND Order WHERE Order.Brand = "1" AND 
                  Order.OrderId = liOrderId NO-LOCK NO-ERROR.

/*  IF AVAIL Order THEN DISPLAY Order.Offer FORMAT "x(20)". */
 
 /* check cash single Fee of Order */
 FOR EACH  SingleFee USE-INDEX HostTable WHERE
           SingleFee.Brand     = "1" AND
           SingleFee.HostTable = "Order" AND
           SingleFee.KeyValue  = STRING(liOrderId) AND
           SingleFee.CalcObj   = "CASHFEE" NO-LOCK :

   /*   DISPLAY SingleFee.BillCode 
              SingleFee.FeeModel FORMAT "x(20)" .
   */
 END.

 /* Display Invoice rows of Order Invoice */
 FIND FIRST Invoice WHERE
            Invoice.InvNum = Order.InvNum NO-LOCK NO-ERROR.

 FOR EACH InvRow OF Invoice  NO-LOCK:

   DISPLAY InvRow WITH 1 COLUMN. 
 END.


 /* display sim status */


