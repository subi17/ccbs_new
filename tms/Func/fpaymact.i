/* fpaymact.i      29.09.06/aam

   actions resulting from payments
*/

/* check if deposit/adv.payment invoice is created from order or from
   owner change request */

{commali.i}
{msreqfunc.i}
{forderstamp.i}
{orderfunc.i}
FUNCTION fBalanceInvoicePaid RETURNS LOGIC
   (INPUT iiInvNum    AS INT,
    INPUT iiPaymState AS INT):

   DEF VAR llUpdated AS LOGIC NO-UNDO.
   DEF VAR lOrdStChg AS LOGIC NO-UNDO.
   
   llUpdated = FALSE.
   
   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.Brand  = gcBrand AND
            SingleFee.InvNum = iiInvNum:
                     
      IF SingleFee.HostTable = "Order" THEN DO:
                      
         FIND Order WHERE
              Order.Brand   = gcBrand AND
              Order.OrderID = INTEGER(SingleFee.KeyValue) NO-LOCK NO-ERROR.                    
         IF NOT AVAILABLE Order THEN NEXT.
                      
         /* fully paid -> order as checked */        
         IF iiPaymState = 2 THEN DO:
            IF Order.StatusCode = "5" THEN DO:
               lOrdStChg = fSetOrderStatus(Order.OrderId,"3").
               FIND Current Order EXCLUSIVE-LOCK.
               ASSIGN Order.CredOK     = TRUE
                      llUpdated        = TRUE.
            END.
         END. 
         
         /* payment cancelled/reversed */               
         ELSE IF Order.StatusCode = "3" THEN DO:
            lOrdStChg = fSetOrderStatus(Order.OrderId,"5").
            FIND CURRENT Order EXCLUSIVE-LOCK.
            ASSIGN Order.CredOK     = FALSE
                   llUpdated        = TRUE.
         END.
         /* Mark timestamp as change */
         if lOrdStChg then fMarkOrderStamp(Order.OrderID,"Change",0.0).
      END. 

      /* owner change */
      ELSE IF SingleFee.HostTable = "MsRequest" THEN DO:
      
         FIND MsRequest WHERE
              MsRequest.MsRequest = INTEGER(SingleFee.KeyValue) 
            NO-LOCK NO-ERROR.

         IF NOT AVAILABLE MsRequest THEN NEXT.

         /* fully paid */
         IF iiPaymState = 2 THEN DO:
            IF MsRequest.ReqStatus = 14 THEN DO:
               fReqStatus(15,"").
               llUpdated = TRUE.
            END.
         END.           
         
         /* payment cancelled / reversed */
         ELSE IF MsRequest.ReqStatus = 15 THEN DO:
            fReqStatus(14,"").
            llUpdated = TRUE.
         END.           

      END.
            
   END. 

   RETURN llUpdated.
   
END FUNCTION.
