/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing_logistics.p
  TASK .........: Update installment TF status based on logistics status
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 24.06.14
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/timestamp.i}

DEF STREAM sout.

DEF VAR lcLogDir AS CHAR NO-UNDO. 

DEF BUFFER bOrderDelivery FOR orderdelivery.
DEF BUFFER bOrderDelivery2 FOR orderdelivery.
DEF BUFFER bFixedFee FOR FixedFee.

DEF VAR lcTFStatus AS CHAR NO-UNDO. 
   
lcLogDir = fCParam("TermFinance","LogDir").
IF NOT lcLogDir > "" THEN RETURN.

DEF VAR lcFile AS CHAR NO-UNDO. 
lcFile = lcLogDir + "internal/terminal_financing_logistics_" + 
   STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + ".log".

OUTPUT STREAM sout TO VALUE(lcFile).
PUT STREAM SOUT UNFORMATTED "ORDER_ID;MSSEQ;MSISDN;ACTION/NEW_FF_STATUS" SKIP.

FUNCTION fLogToFile RETURN LOGICAL
   (icAction AS CHAR):
    
   PUT STREAM sout UNFORMATTED
      Order.OrderId ";"
      Order.MsSeq ";"
      Order.CLI ";"
      icAction SKIP.
END.

ORDER_LOOP:
FOR EACH Order NO-LOCK WHERE
         Order.Brand   = gcBrand AND
         Order.StatusCode = {&ORDER_STATUS_RESIGNATION}:

   FOR EACH OrderDelivery NO-LOCK WHERE
            OrderDelivery.Brand = gcBrand AND
            OrderDelivery.Orderid = Order.OrderId 
         BREAK BY OrderDelivery.OrderId
         BY OrderDelivery.LoTimeStamp DESC:

      /* YPR-1872 */
      IF FIRST-OF(OrderDelivery.OrderId) AND
         OrderDelivery.LoStatusId EQ 8 AND
         orderdelivery.LoTimeStamp < DATETIME(TODAY - 14,0) THEN DO:
         RUN orderhold.p(Order.OrderId, "RELEASE_BATCH").
         fLogToFile("RELEASED: delayed activation, delivered to customer more than 14 days ago").
         NEXT ORDER_LOOP.
      END.
      
      IF LOOKUP(STRING(orderdelivery.LoStatusId),
         {&DEXTRA_CANCELLED_STATUSES}) > 0 THEN NEXT ORDER_LOOP.
      
      IF LOOKUP(STRING(orderdelivery.LoStatusId),"8,12,100") = 0 THEN NEXT.

      IF orderdelivery.LoStatusId = 12 AND
         orderdelivery.LoTimeStamp < DATETIME(TODAY - 20,0) THEN DO:
         RUN closeorder.p(Order.OrderId, TRUE).
         fLogToFile("CLOSED (no final status for 12 status after 20 days):"
                     + STRING(RETURN-VALUE)).
      END.

      LEAVE.
   END.
END.

FF_LOOP:
FOR EACH FixedFee NO-LOCK WHERE
         FixedFee.FinancedResult = {&TF_STATUS_HOLD_SENDING},
   FIRST Order NO-LOCK WHERE
         Order.Brand = gcBrand AND
         Order.OrderID = FixedFee.OrderID:

   lcTFStatus = "".

   FOR EACH orderdelivery NO-LOCK WHERE
            orderdelivery.brand = gcBrand AND
            orderdelivery.OrderId = Order.OrderId
         BY orderdelivery.LoTimeStamp DESC:
      
      IF LOOKUP(STRING(orderdelivery.LoStatusId),
               {&DEXTRA_CANCELLED_STATUSES}) > 0 THEN NEXT FF_LOOP.

      IF LOOKUP(STRING(orderdelivery.LoStatusId),"8,12,100") = 0 THEN NEXT.

      IF orderdelivery.LoStatusId = 12 AND
         orderdelivery.LoTimeStamp < DATETIME(TODAY - 20,0) THEN DO:

         IF CAN-FIND(FIRST ActionLog NO-LOCK WHERE
                           ActionLog.Brand = gcBrand AND
                           ActionLog.TableName = "Order" AND
                           ActionLog.KeyValue = STRING(Order.OrderId) AND
                           ActionLog.ActionID = "LOCancel" AND
                           ActionLog.ActionStatus = 3) THEN DO:
            fLogToFile("SKIPPED: Cancellation already handled").
            NEXT FF_LOOP.
         END.

         RUN cancelorder.p(Order.OrderId,FALSE).
         fLogToFile("CANCELLED").

         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = gcBrand   
            ActionLog.TableName    = "Order"
            ActionLog.KeyValue     = STRING(Order.OrderID)
            ActionLog.ActionID     = "LOCancel"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.ActionStatus = 3
            ActionLog.ActionTS     = fMakeTS().

         RELEASE ActionLog.

         NEXT FF_LOOP.
      END.
      ELSE IF orderdelivery.LOStatusId EQ 8 AND 
              orderDelivery.LoTimeStamp < DATETIME(TODAY - 16,0) THEN DO:
            
         FIND FIRST borderdelivery NO-LOCK WHERE
                    borderdelivery.brand = gcBrand AND
                    borderdelivery.OrderId = OrderDelivery.OrderId AND
                    borderdelivery.LoTimeStamp >= OrderDelivery.LoTimeStamp AND
                   (borderdelivery.LoStatusId = 4000 OR
                    borderdelivery.LoStatusId = 4030) NO-ERROR.
         
         IF NOT AVAIL borderdelivery THEN 
            lcTFStatus = {&TF_STATUS_YOIGO_LOGISTICS}.
         ELSE CASE borderdelivery.LoStatusId:
            WHEN 4000 THEN lcTFStatus = {&TF_STATUS_WAITING_SENDING}.
            WHEN 4030 THEN lcTFStatus = {&TF_STATUS_YOIGO_LOGISTICS}.
            OTHERWISE NEXT FF_LOOP.
         END.
      END.
      ELSE IF OrderDelivery.LoStatusId EQ 100 THEN DO:
         
         FIND FIRST borderdelivery NO-LOCK WHERE
                    borderdelivery.brand = gcBrand AND
                    borderdelivery.OrderId = OrderDelivery.OrderId AND
                    borderdelivery.LoTimeStamp >= OrderDelivery.LoTimeStamp AND
                   (borderdelivery.LoStatusId = 3000 OR
                    borderdelivery.LoStatusId = 3100 OR
                    borderdelivery.LoStatusId = 3350 OR
                    borderdelivery.LoStatusId = 900) NO-ERROR.

         IF NOT AVAIL borderdelivery THEN DO:

            IF orderdelivery.LoTimeStamp < DATETIME(TODAY - 20,0) THEN
               lcTFStatus = {&TF_STATUS_YOIGO_LOGISTICS}.
            ELSE NEXT FF_LOOP.
         END.
         ELSE CASE borderdelivery.LoStatusId:
            WHEN 3000 THEN lcTFStatus = {&TF_STATUS_YOIGO_LOGISTICS}.
            WHEN 3100 THEN DO:
               RUN cancelorder.p(Order.OrderId, FALSE).
               fLogToFile("CANCELLED").
               NEXT FF_LOOP.
            END.
            WHEN 3350 OR WHEN 900 THEN DO:
               IF CAN-FIND(
                  FIRST borderdelivery2 NO-LOCK WHERE
                        borderdelivery2.brand = gcBrand AND
                        borderdelivery2.OrderId = bOrderDelivery.OrderId AND
                        borderdelivery2.LoTimeStamp <= bOrderDelivery.LoTimeStamp AND
                        borderdelivery2.LoStatusId = 4000)
               THEN lcTFStatus = {&TF_STATUS_WAITING_SENDING}.
               ELSE lcTFStatus = {&TF_STATUS_YOIGO_LOGISTICS}.
            END.
            OTHERWISE NEXT FF_LOOP.
         END.

      END.
      ELSE NEXT FF_LOOP.

      IF lcTFStatus EQ "" THEN NEXT.
      
      FIND bFixedFee EXCLUSIVE-LOCK WHERE
           ROWID(bFixedFee) = ROWID(FixedFee) NO-ERROR.
      bFixedFee.FinancedResult = lcTFStatus.
      RELEASE bFixedFee.
      fLogToFile(lcTFStatus).

      NEXT FF_LOOP.
   END.
      
END.

OUTPUT STREAM sout CLOSE.
