
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


/* change Order OfferId */
 FIND Order WHERE Order.Brand = "1" AND 
                  Order.OrderId = liOrderId NO-LOCK NO-ERROR.

 IF AVAIL Order AND Order.Offer = lcCurrentOfferId THEN DO:
   liInvNum = Order.InvNum.
   FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN Order.Offer = lcNewOfferId.
   RELEASE Order.
 END.
 ELSE RETURN "Current OfferId is different !".
 

 /* change cash single Fee of Order */
 FOR EACH  SingleFee USE-INDEX HostTable WHERE
           SingleFee.Brand     = "1" AND
           SingleFee.HostTable = "Order" AND
           SingleFee.KeyValue  = STRING(liOrderId) AND
           SingleFee.CalcObj   = "CASHFEE" EXCLUSIVE-LOCK :

   ASSIGN  SingleFee.FeeModel = "O:" + lcNewOfferId.

   IF SingleFee.BillCode = lcCurrentBillCode THEN 
   ASSIGN SingleFee.BillCode = lcNewBillCode.

  /*    DISPLAY SingleFee.BillCode 
              SingleFee.FeeModel .
  */
/*      change all feemodel to the new one and in case of SIM change billcode */
  END.

 /* Display Invoice rows of Order Invoice */
 FIND FIRST Invoice WHERE
            Invoice.InvNum = liInvNum NO-LOCK NO-ERROR.

 FOR EACH InvRow OF Invoice WHERE 
          InvRow.BillCode = lcCurrentBillCode  EXCLUSIVE-LOCK:

     ASSIGN InvRow.BillCode = lcNewBillCode.
    /* DISPLAY InvRow WITH 1 COLUMN. */
 END.


 /* display sim status */


