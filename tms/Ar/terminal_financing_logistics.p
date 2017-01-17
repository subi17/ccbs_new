/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing_logistics.p
  TASK .........: Update installment TF status based on logistics status
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 24.06.14
  Version ......: Yoigo
----------------------------------------------------------------------- */
{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{tmsconst.i}
{cparam2.i}
{timestamp.i}

DEFINE TEMP-TABLE ttDelivery NO-UNDO
   FIELD LoStatus AS INTEGER
   FIELD Stamp AS DATETIME
   INDEX LoStatus IS PRIMARY UNIQUE LoStatus
   INDEX Stamp Stamp
   .

DEF STREAM sout.

FUNCTION fGetDStamp RETURNS DATETIME
   (iiLoStatus AS INTEGER ):

   FOR ttDelivery WHERE ttDelivery.LoStatus = iiLoStatus:
      RETURN ttDelivery.Stamp.
   END.

   RETURN ?.

END FUNCTION.

FUNCTION fLogToFile RETURN LOGICAL
   (icAction AS CHAR):

   PUT STREAM sout UNFORMATTED
      Order.OrderId ";"
      Order.MsSeq ";"
      Order.CLI ";"
      icAction SKIP.
END.

FUNCTION fPopulateTTDelivery RETURNS LOGICAL
   (iiOrderID AS INTEGER):

   EMPTY TEMP-TABLE ttDelivery.

   FOR
      EACH OrderDelivery NO-LOCK WHERE
         OrderDelivery.brand = gcBrand AND
         OrderDelivery.OrderId = iiOrderId
      BY OrderDelivery.LoTimeStamp DESCENDING:

      IF LOOKUP(STRING(OrderDelivery.LoStatusId), {&DEXTRA_CANCELLED_STATUSES}) > 0
      THEN RETURN FALSE.

      IF LOOKUP(STRING(OrderDelivery.LoStatusId), "8,12,19,100,110,125,241,900,3000,3001,3100,3350,3351,4000,4030") = 0
      THEN NEXT.

      FIND ttDelivery WHERE ttDelivery.LoStatus = OrderDelivery.LoStatusId NO-ERROR.
      IF NOT AVAILABLE ttDelivery
      THEN DO:
         CREATE ttDelivery.
         ASSIGN
            ttDelivery.LoStatus = OrderDelivery.LoStatusId
            ttDelivery.Stamp  = OrderDelivery.LoTimeStamp
            .

        IF OrderDelivery.LoStatusId = 8 OR
           ((OrderDelivery.LoStatusId = 12 OR 
           OrderDelivery.LoStatusId = 125) AND fGetDStamp(8) EQ ?)
        THEN RETURN TRUE.
      END.
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fNextLoStatus RETURNS INTEGER
   (idtStamp AS DATETIME ):

   FOR FIRST ttDelivery WHERE ttDelivery.Stamp > idtStamp USE-INDEX Stamp:
      RETURN ttDelivery.LoStatus.
   END.

   RETURN ?.

END FUNCTION.


FUNCTION fCancelOrder RETURNS LOGICAL
   (iiOrderID AS INTEGER,
    ilCheckActionLog AS LOGICAL):

   IF ilCheckActionLog AND
      CAN-FIND(FIRST ActionLog NO-LOCK WHERE
                     ActionLog.Brand = gcBrand AND
                     ActionLog.TableName = "Order" AND
                     ActionLog.KeyValue = STRING(iiOrderId) AND
                     ActionLog.ActionID = "LOCancel" AND
                     ActionLog.ActionStatus = 3)
   THEN DO:
      fLogToFile("SKIPPED: Cancellation already handled").
      RETURN FALSE.
   END.

   RUN cancelorder.p(iiOrderId,FALSE).
   fLogToFile("CANCELLED").

   IF NOT ilCheckActionLog
   THEN RETURN FALSE.

   CREATE ActionLog.
   ASSIGN
      ActionLog.Brand        = gcBrand
      ActionLog.TableName    = "Order"
      ActionLog.KeyValue     = STRING(iiOrderID)
      ActionLog.ActionID     = "LOCancel"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 +
                               MONTH(TODAY)
      ActionLog.ActionStatus = 3
      ActionLog.ActionTS     = fMakeTS().

   RELEASE ActionLog.

   RETURN FALSE.

END FUNCTION.


FUNCTION fGetTFStatus RETURNS CHARACTER
   (iiOrderID AS INTEGER):

   DEFINE VARIABLE lcTFStatus   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldtStamp     AS DATETIME  NO-UNDO.

   EMPTY TEMP-TABLE ttDelivery.

   IF NOT fPopulateTTDelivery(iiOrderID)
   THEN RETURN "".

   ldtStamp = fGetDStamp(8).

   IF ldtStamp < DATETIME(TODAY - 16,0)
   THEN DO:
      CASE fNextLoStatus(ldtStamp):
         WHEN 4000 THEN lcTFStatus = {&TF_STATUS_WAITING_SENDING}.
         /*WHEN 4030 THEN lcTFStatus = {&TF_STATUS_YOIGO_LOGISTICS}.*/
         OTHERWISE lcTFStatus = {&TF_STATUS_YOIGO_LOGISTICS}.
      END CASE.

      ldtStamp = fGetDStamp(100).

      IF ldtStamp NE ?
      THEN
      CASE fNextLoStatus(INPUT ldtStamp):
         WHEN 3000
         THEN RETURN {&TF_STATUS_YOIGO_LOGISTICS}.
         WHEN 3100
         THEN DO:
            fCancelOrder(iiOrderId, FALSE).
            RETURN "".
         END.
         WHEN 3350 OR WHEN 900
         THEN RETURN lcTFStatus.
         OTHERWISE DO:
            IF ldtStamp < DATETIME(DATE(TODAY) - 20,0)
            THEN RETURN {&TF_STATUS_YOIGO_LOGISTICS}.
            RETURN "".
         END.
      END CASE.
   END.

   ELSE IF fGetDStamp(12) < DATETIME(TODAY - 20,0)
   THEN DO:
      fCancelOrder(iiOrderId, TRUE).
      RETURN "".
   END.
   ELSE IF fGetDStamp(19) NE ? THEN DO:
      ldtStamp = fGetDStamp(110).
      IF ldtStamp NE ?
      THEN
      CASE fNextLoStatus(INPUT ldtStamp):
         WHEN 3001 OR
         WHEN 3101 OR
         WHEN 3351 OR
         WHEN 241
         THEN DO:
            fCancelOrder(iiOrderId, TRUE).
            RETURN "".
         END.   
      END CASE.
   END.
   
   RETURN lcTFStatus.

END FUNCTION.

DEF VAR lcLogDir AS CHAR NO-UNDO. 

DEF BUFFER bFixedFee FOR FixedFee.

DEF VAR lcTFStatus AS CHAR NO-UNDO. 
   
lcLogDir = fCParam("TermFinance","LogDir").
IF NOT lcLogDir > "" THEN RETURN.

DEF VAR lcFile AS CHAR NO-UNDO. 
lcFile = lcLogDir + "internal/terminal_financing_logistics_" + 
   STRING(YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY)) + ".log".

OUTPUT STREAM sout TO VALUE(lcFile).
PUT STREAM SOUT UNFORMATTED "ORDER_ID;MSSEQ;MSISDN;ACTION/NEW_FF_STATUS" SKIP.

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
         OrderDelivery.LoTimeStamp < DATETIME(TODAY - 14,0) THEN DO:
         RUN orderhold.p(Order.OrderId, "RELEASE_BATCH").
         fLogToFile("RELEASED: delayed activation, delivered to customer more than 14 days ago").
         NEXT ORDER_LOOP.
      END.
      
      IF LOOKUP(STRING(OrderDelivery.LoStatusId),
         {&DEXTRA_CANCELLED_STATUSES}) > 0 THEN NEXT ORDER_LOOP.
      
      IF LOOKUP(STRING(OrderDelivery.LoStatusId),"8,12,19,100,125") = 0 THEN 
         NEXT.

      IF (OrderDelivery.LoStatusId = 12 OR OrderDelivery.LoStatusId = 125) AND
         OrderDelivery.LoTimeStamp < DATETIME(TODAY - 20,0) THEN DO:
         RUN closeorder.p(Order.OrderId, TRUE).
         fLogToFile("CLOSED (no final status for " + 
         STRING(OrderDelivery.LoStatusId) + " status after 20 days):" + 
         STRING(RETURN-VALUE)).
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

   lcTFStatus = fGetTFStatus(Order.OrderID).

   IF lcTFStatus > ""
   THEN DO:
      FIND bFixedFee EXCLUSIVE-LOCK WHERE
           ROWID(bFixedFee) = ROWID(FixedFee) NO-ERROR.
      bFixedFee.FinancedResult = lcTFStatus.
      RELEASE bFixedFee.
      fLogToFile(lcTFStatus).
   END.

END.

FINALLY:
   EMPTY TEMP-TABLE ttDelivery.
   OUTPUT STREAM sout CLOSE.
END FINALLY.
