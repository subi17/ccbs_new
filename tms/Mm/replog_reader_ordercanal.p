/* ----------------------------------------------------------------------
  Module .......: Mm/replog_reader_ordercanal.p
  Task .........: Replication Log Reader for OrderCanal Database
  Application ..: TMS
  Author .......: Vikas
  Created ......: 24.05.13
  Modified .....: 02.07.14/vekov Order table and tmsconst.i
                  03.07.14/vekov OrderCustomer
                                 OrderDelivery
                                 OrderFusion
                  07.07.14/vekov OrderAction
  Version ......: Yoigo
---------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{timestamp.i}
{log.i}
{ftransdir.i}
{replog_reader.i}
{tmsconst.i}

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "RepLog Qty:" liRepLogs FORMAT ">>>>>>>9" 
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " Replication Logs for OrderCanal Database"
FRAME frmLog.

/* Initialize the configurations */

RUN pInitialize(INPUT "ordercanal").

IF RETURN-VALUE > "" THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
   RETURN RETURN-VALUE.
END.

/* Call ActiveMQ Publisher class */
lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                    liTimeOut,"hpd.ordercanal",
                                    lcUserName,lcPassword).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
END.

FUNCTION fCommonMessage RETURNS CHAR:

   DEF VAR lcMessage AS CHAR NO-UNDO.

   lcMessage = fNotNull(Ordercanal.RepLog.TableName)         + lcDel +
               fNotNull(Ordercanal.RepLog.EventType)         + lcDel +
               fNotNull(STRING(Ordercanal.RepLog.RecordId))  + lcDel +
               fNotNull(Ordercanal.RepLog.KeyValue)          + lcDel +
               fNotNull(STRING(Ordercanal.RepLog.EventTS)).

   RETURN lcMessage.
END. /* FUNCTION fCommonMessage RETURNS CHAR: */

/* MAIN BLOCK */

RepLog:
DO WHILE TRUE :

   liLoop = liLoop + 1.
   PAUSE 0.
   DISP  liLoop  
         Today @ ldToday 
         liRepLogs
         string(time,"hh:mm:ss") @ lctime
   WITH FRAME frmLog.
   PAUSE 0.

   RUN pHandleRepLog(OUTPUT liAmount).

   liRepLogs = liRepLogs + liAmount.

   /* Monitoring */
   IF lcNagiosURL > "" THEN
      fKeepAlive("REPLOG_ORDER:OrderCanal Database Reader",lcNagiosURL).

   IF ldLogDate = ? THEN ldLogDate = TODAY.

   /* Replog handling statistics file */
   IF lcLogFileStat > "" AND ldLogDate <> TODAY THEN DO:

      lcStatLogFile = lcLogFileStat + "_ordercanal_" +
                      STRING(YEAR(ldLogDate)) + STRING(MONTH(ldLogDate),"99") +
                      STRING(DAY(ldLogDate),"99") + ".txt".

      OUTPUT STREAM sLogStat TO VALUE(lcStatLogFile).

      FOR EACH ttStat WHERE
               ttStat.ttDatabase = "OrderCanal" AND
               ttStat.ttDate     = ldLogDate EXCLUSIVE-LOCK:

         PUT STREAM sLogStat UNFORMATTED
             STRING(ttStat.ttDate) "|"
             STRING(ttStat.ttHour) "|"
             ttStat.ttDatabase     "|"
             ttStat.ttTable        "|"
             ttStat.ttEventType    "|"
             ttStat.ttEvents       SKIP.

         DELETE ttStat.
      END.

      OUTPUT STREAM sLogStat CLOSE.

      ldLogDate = TODAY.
   END. /* IF lcLogFileStat > "" AND ldLogDate <> TODAY THEN DO: */
  
   PUT SCREEN ROW 22 COL 1
   "F8 TO QUIT, OTHER KEYS START REPLOG IMMEDIATELLY".
   
   READKEY PAUSE 10.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE RepLog.

END.

RUN pFinalize(INPUT "ordercanal").

QUIT.

PROCEDURE pHandleRepLog:

   DEFINE OUTPUT PARAMETER oiHandled AS INTEGER   NO-UNDO.

   DEFINE VARIABLE llHandled         AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE liHour            AS INTEGER   NO-UNDO.

   DEFINE BUFFER bRepLog FOR OrderCanal.RepLog.

   IF lcDumpSpool > "" AND
      (ldDate <> TODAY OR ldeTimeStamp <= fMakeTS()) THEN
      RUN pDumpFileRotation(INPUT "ordercanal").

   LOOP:
   FOR EACH OrderCanal.RepLog NO-LOCK BY EventTS:

      /* Treshold value */
      IF oiHandled >= 10000 THEN LEAVE LOOP.

      IF LOOKUP("pHandle" + RepLog.TableName,
                THIS-PROCEDURE:INTERNAL-ENTRIES) = 0 THEN NEXT.

      IF llLogFileAct THEN DO:
         OUTPUT STREAM sDump to VALUE(lcDumpFile).
         llLogFileAct = FALSE.
      END. /* IF llLogFileAct THEN DO: */

      DO TRANSACTION:
         FIND FIRST bRepLog WHERE
                    ROWID(bRepLog) = ROWID(RepLog)
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAIL bRepLog THEN DO:

            RUN VALUE("pHandle" + bRepLog.TableName) (OUTPUT llHandled).
            IF llHandled THEN DO:

               liHour = Time / 3600.

               FIND FIRST ttStat WHERE
                          ttStat.ttDatabase  = "OrderCanal"      AND
                          ttStat.ttTable     = bRepLog.TableName AND
                          ttStat.ttEventType = bRepLog.EventType AND
                          ttStat.ttDate      = TODAY             AND
                          ttStat.ttHour      = liHour EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL ttStat THEN
                  ttStat.ttEvents = ttStat.ttEvents + 1.
               ELSE DO:
                  CREATE ttStat.
                  ASSIGN ttStat.ttDatabase  = "OrderCanal"
                         ttStat.ttTable     = bRepLog.TableName
                         ttStat.ttEventType = bRepLog.EventType
                         ttStat.ttDate      = TODAY
                         ttStat.ttHour      = liHour
                         ttStat.ttEvents    = ttStat.ttEvents + 1.
               END. /* ELSE DO: */

               DELETE bRepLog.
               oiHandled = oiHandled + 1.
            END. /* IF llHandled THEN DO: */
         END. /* IF AVAIL bRepLog THEN DO: */
      END. /* DO TRANSACTION: */
   END. /* FOR EACH RepLog NO-LOCK: */

END PROCEDURE.

PROCEDURE pHandleMobSub:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcTariffBundle    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOldCLIType      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOldtariffBundle AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaFromDate       AS DATE      NO-UNDO.

   IF AVAIL OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST MobSub WHERE
                       RECID(MobSub) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAIL MobSub THEN DO:

               CASE MobSub.TariffBundle:
                  WHEN "CONTDATA" THEN lcTariffBundle = "CONTRD1".
                  WHEN "CONTD2"   THEN lcTariffBundle = "CONTRD2".
                  WHEN "CONTD3"   THEN lcTariffBundle = "CONTRD3".
                  WHEN "CONTD4"   THEN lcTariffBundle = "CONTRD4".
                  OTHERWISE lcTariffBundle = MobSub.TariffBundle.
               END CASE.

               ldaFromDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).

               lcMessage = lcMessage                             + lcDel +
                           fNotNull(STRING(MobSub.MsSeq))        + lcDel +
                           fNotNull(STRING(MobSub.CustNum))      + lcDel +
                           fNotNull(MobSub.CLI)                  + lcDel +
                           fNotNull(MobSub.CLIType)              + lcDel +
                           fNotNull(lcTariffBundle)              + lcDel +
                           fNotNull(STRING(MobSub.PayType))      + lcDel +
                           fNotNull(STRING(MobSub.ActivationTS)) + lcDel +
                           fNotNull(STRING(MobSub.MultiSimID))   + lcDel +
                           fNotNull(STRING(MobSub.MultiSimType)) + lcDel +
                           fNotNull(MobSub.IMSI)                 + lcDel +
                           fNotNull(MobSub.BarrCode)             + lcDel +
                           fDateToString(MobSub.TariffActDate)   + lcDel.

               IF RepLog.EventType = "MODIFY" AND
                  MobSub.TariffActDate <> Mobsub.Activationdate AND
                  MobSub.TariffActDate > ldaFromDate THEN
                  FOR EACH MsOwner NO-LOCK WHERE
                           MsOwner.MsSeq = MobSub.MsSeq USE-INDEX MsSeq:

                     IF MsOwner.CLIType NE MobSub.CLIType OR
                        MsOwner.TariffBundle NE MobSub.TariffBundle THEN DO:
                        ASSIGN lcOldCLIType = MsOwner.CLIType
                               lcOldtariffBundle = MsOwner.TariffBundle.
                        LEAVE.
                     END.
                  END.
               ELSE
                  ASSIGN lcOldCLIType = ""
                         lcOldtariffBundle = "".

               lcMessage = lcMessage + lcOldCLIType + lcDel + lcOldtariffBundle.

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleOrder:

   DEFINE OUTPUT PARAMETER olHandled   AS LOGICAL    NO-UNDO.

   DEFINE VARIABLE lcMessage           AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcCustId            AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcCustIdType        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE liPaymentType       AS INTEGER    NO-UNDO INIT ?.
   DEFINE VARIABLE ldDeliveryTime      AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE ldLastUpdateStamp   AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE ldClosedTimeStamp   AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE lcImei              AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcTerminalBillCode  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcFormRequest       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcPortRequest       AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE ldPortingTime       AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE lcPayTermContr      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcTermContr         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE ldTermAmt           AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE ldTermDiscount      AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE ldTermLeasAmt       AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE ldMnpUpdateSt       AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE liMnpStatusCode     AS INTEGER    NO-UNDO INIT ?.
   DEFINE VARIABLE ldMnpCreateStamp    AS DECIMAL    NO-UNDO INIT ?.
   DEFINE VARIABLE lcMnpStatusReason   AS CHARACTER  NO-UNDO.


   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST Order WHERE
                       RECID(Order) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE Order THEN DO:
               
               /* custid custidtype */
               FIND FIRST OrderCustomer WHERE
                          OrderCustomer.Brand   = gcBrand AND
                          OrderCustomer.OrderId = Order.OrderId AND
                          OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
               IF AVAILABLE OrderCustomer THEN
                  ASSIGN lcCustId     = OrderCustomer.CustId
                         lcCustIdType = OrderCustomer.CustIdType.

               /* payment_type */
               FIND FIRST OrderPayment WHERE
                          OrderPayment.Brand   = gcBrand AND
                          OrderPayment.OrderId = Order.OrderId 
                          NO-LOCK NO-ERROR.
               IF AVAILABLE OrderPayment THEN
                  ASSIGN liPaymentType = OrderPayment.Method.

               /* delivery_time  */
               FIND FIRST OrderTimeStamp WHERE
                          OrderTimeStamp.Brand   = gcBrand AND
                          OrderTimeStamp.OrderId = Order.OrderId AND
                          OrderTimeStamp.RowType = {&ORDERTIMESTAMP_DELIVERY}
                          NO-LOCK NO-ERROR.
               IF AVAILABLE OrderTimeStamp THEN
                  ASSIGN ldDeliveryTime = OrderTimeStamp.Timestamp.

               /* last_update_stamp */
               FIND FIRST OrderTimeStamp WHERE
                          OrderTimeStamp.Brand   = gcBrand AND
                          OrderTimeStamp.OrderId = Order.OrderId AND
                          OrderTimeStamp.RowType = {&ORDERTIMESTAMP_CHANGE}
                          NO-LOCK NO-ERROR.
               IF AVAILABLE OrderTimeStamp THEN 
                  ASSIGN ldLastUpdateStamp = OrderTimeStamp.TimeStamp.
              
               /* closed_time */
               FIND FIRST OrderTimeStamp WHERE
                          OrderTimeStamp.Brand   = gcBrand AND
                          OrderTimeStamp.OrderId = Order.OrderId AND
                          OrderTimeStamp.RowType = {&ORDERTIMESTAMP_CLOSE}
                          NO-LOCK NO-ERROR.
               IF AVAILABLE OrderTimeStamp THEN
                  ASSIGN ldClosedTimeStamp = OrderTimeStamp.TimeStamp.

               /* IMEI */
               FIND FIRST OrderAccessory WHERE
                          OrderAccessory.Brand        = gcBrand AND
                          OrderAccessory.OrderId      = Order.OrderId AND
                          OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} 
                          NO-LOCK NO-ERROR.
               IF AVAILABLE OrderAccessory THEN
                  ASSIGN lcImei             = OrderAccessory.IMEI
                         lcTerminalBillCode = OrderAccessory.ProductCode
                         ldTermAmt          = OrderAccessory.Amount
                         ldTermDiscount     = OrderAccessory.Discount.
               /* MNP requests  */
               FOR EACH MNPProcess WHERE
                        MNPProcess.OrderId = Order.OrderId
                        NO-LOCK BY CrStamp DESC:
                  ASSIGN lcFormRequest     = MNPProcess.FormRequest
                         lcPortRequest     = MNPProcess.PortRequest
                         ldPortingTime     = MNPProcess.PortingTime
                         ldMnpUpdateSt     = MNPProcess.UpdateTS
                         liMnpStatusCode   = MNPProcess.StatusCode
                         ldMnpCreateStamp  = MNPProcess.CreatedTS
                         lcMnpStatusReason = MNPProcess.StatusReason.

                  LEAVE.
               END.

               /* contracts  */
               IF Order.Offer > "" THEN DO:

                  FIND FIRST OfferItem WHERE
                       OfferItem.Brand       = gcBrand AND
                       OfferItem.Offer       = Order.Offer AND
                       OfferItem.ItemType    = "PerContract" AND
                       OfferItem.ItemKey     BEGINS "PAYTERM" AND
                       OfferItem.EndStamp   >= Order.CrStamp  AND
                       OfferItem.BeginStamp <= Order.CrStamp  NO-LOCK NO-ERROR.
                       
                  IF AVAIL OfferItem THEN
                     ASSIGN lcPayTermContr = OfferItem.ItemKey
                            ldTermLeasAmt  = OfferItem.Amount.
                  
                  FIND FIRST OfferItem WHERE
                       OfferItem.Brand       = gcBrand AND
                       OfferItem.Offer       = Order.Offer AND
                       OfferItem.ItemType    = "PerContract" AND
                       OfferItem.ItemKey     BEGINS "TERM" AND
                       OfferItem.EndStamp   >= Order.CrStamp AND
                       OfferItem.BeginStamp <= Order.CrStamp NO-LOCK NO-ERROR.
                       
                  IF AVAIL OfferItem THEN
                     lcTermContr = OfferItem.ItemKey.
                  
               END.

               lcMessage = lcMessage                                + lcDel +
                           fNotNull(STRING(Order.OrderID))          + lcDel +
                           fNotNull(Order.CLI)                      + lcDel +
                           fNotNull(STRING(Order.MSSeq))            + lcDel +
                           fNotNull(STRING(Order.OrderType))        + lcDel +
                           fNotNull(Order.OrderChannel)             + lcDel +
                           fNotNull(Order.StatusCode)               + lcDel +
                           fNotNull(Order.ContractID)               + lcDel +
                           fNotNull(Order.Salesman)                 + lcDel +
                           fNotNull(Order.Campaign)                 + lcDel +
                           fNotNull(STRING(Order.CrStamp))          + lcDel +
                           fNotNull(Order.Referee)                  + lcDel +
                           fNotNull(STRING(liPaymentType))          + lcDel +
                           fNotNull(STRING(ldDeliveryTime))         + lcDel +
                           fNotNull(STRING(ldLastUpdateStamp))      + lcDel +
                           fNotNull(STRING(ldClosedTimeStamp))      + lcDel +
                           fNotNull(Order.ICC)                      + lcDel +
                           fNotNull(STRING(Order.PayType))          + lcDel +
                           fNotNull(Order.CurrOper)                 + lcDel +
                           fNotNull(Order.OldIcc)                   + lcDel +
                           fNotNull(STRING(Order.OldPayType))       + lcDel +
                           fNotNull(Order.CLIType)                  + lcDel +
                           fNotNull(Order.Offer)                    + lcDel +
                           fNotNull(lcPayTermContr)                 + lcDel +
                           fNotNull(lcTermContr)                    + lcDel +
                           fNotNull(STRING(Order.DeliveryType))     + lcDel +
                           fNotNull(STRING(Order.DeliverySecure))   + lcDel +
                           fNotNull(lcImei)                         + lcDel +
                           fNotNull(lcTerminalBillCode)             + lcDel +
                           fNotNull(STRING(liMnpStatusCode))        + lcDel +
                           fNotNull(lcFormRequest)                  + lcDel +
                           fNotNull(lcPortRequest)                  + lcDel +
                           fNotNull(STRING(ldPortingTime))          + lcDel +
                           fNotNull(lcCustId)                       + lcDel +
                           fNotNull(lcCustIdType)                   + lcDel +
                           fNotNull(STRING(ldTermAmt))              + lcDel +
                           fNotNull(STRING(ldTermDiscount))         + lcDel +
                           fNotNull(STRING(ldTermLeasAmt))          + lcDel +
                           fNotNull(STRING(Order.PortingDate))      + lcDel +
                           fNotNull(STRING(ldMnpUpdateSt))          + lcDel +
                           fNotNull(STRING(ldMnpCreateStamp))       + lcDel +
                           fNotNull(lcMnpStatusReason).


               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleOrderCustomer:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCompany         AS CHARACTER NO-UNDO.
   
   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST OrderCustomer WHERE
                       RECID(OrderCustomer) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE OrderCustomer THEN DO:

               lcMessage = lcMessage                                 + lcDel +
                           fNotNull(STRING(OrderCustomer.OrderId))   + lcDel +
                           fNotNull(STRING(OrderCustomer.RowType))   + lcDel +
                           fNotNull(OrderCustomer.CustTitle)         + lcDel +
                           fNotNull(OrderCustomer.FirstName)         + lcDel +
                           fNotNull(OrderCustomer.SurName1)          + lcDel +
                           fNotnull(OrderCustomer.SurName2)          + lcDel +
                           fNotNull(OrderCustomer.ZipCode)           + lcDel +
                           fNotNull(OrderCustomer.Region)            + lcDel +
                           fNotNull(OrderCustomer.Address)           + lcDel +
                           fNotNull(OrderCustomer.PostOffice)        + lcDel +
                           fNotNull(OrderCustomer.Company)           + lcDel +
                           fNotNull(STRING(OrderCustomer.DelType))   + lcDel +
                           fNotNull(OrderCustomer.KialaCode)         + lcDel +
                           fNotnull(OrderCustomer.CustId)            + lcDel +
                           fNotNull(OrderCustomer.CustIdType).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleOrderDelivery:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   
   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST OrderDelivery WHERE
                       RECID(OrderDelivery) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE OrderDelivery THEN DO:

               lcMessage = lcMessage                                  + lcDel +
                           fNotNull(STRING(OrderDelivery.OrderId))    + lcDel +
                           fNotNull(STRING(OrderDelivery.LOStatusId)) + lcDel +
                           fNotNull(STRING(OrderDelivery.CourierId))  + lcDel +
                           fNotNull(STRING(OrderDelivery.LOId))       + lcDel +
                           fNotNull(STRING(OrderDelivery.LOTimeStamp)) + lcDel +
                           fNotNull(OrderDelivery.CourierShippingId).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleSubsTerminal:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   
   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST SubsTerminal WHERE
                       RECID(SubsTerminal) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE SubsTerminal THEN DO:

               lcMessage = lcMessage                                  + lcDel +
                           fNotNull(STRING(SubsTerminal.MSSeq))       + lcDel +
                           fNotNull(STRING(SubsTerminal.OrderId))     + lcDel +
                           fNotNull(STRING(SubsTerminal.TerminalId))  + lcDel +
                           fNotNull(STRING(SubsTerminal.TerminalType)) + lcDel +
                           fNotNull(SubsTerminal.Model)               + lcDel +
                           fNotNull(SubsTerminal.IMEI)                + lcDel +
                           fNotNull(SubsTerminal.BillCode)            + lcDel +
                           fNotNull(STRING(SubsTerminal.PurchaseTS))  + lcDel +
                           fNotNull(STRING(SubsTerminal.PerContractID)).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleOrderFusion:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   
   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST OrderFusion WHERE
                       RECID(OrderFusion) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE OrderFusion THEN DO:

               lcMessage = lcMessage                               + lcDel +
                           fNotNull(STRING(OrderFusion.OrderId))   + lcDel +
                           fNotNull(OrderFusion.FusionStatus)      + lcDel +
                           fNotNull(OrderFusion.FixedNumber)       + lcDel +
                           fNotNull(OrderFusion.FixedNumberType)   + lcDel +
                           fNotNull(OrderFusion.FixedStatus).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleOrderAction:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcItemTypeDesc    AS CHARACTER NO-UNDO.
   
   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST OrderAction WHERE
                       RECID(OrderAction) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE OrderAction THEN DO:
               FIND FIRST Order WHERE
                          Order.Brand   = gcBrand AND
                          Order.OrderId = OrderAction.OrderId NO-LOCK NO-ERROR.

               lcItemTypeDesc = OrderAction.ItemType.

               IF OrderAction.ItemType = "BundleItem" THEN DO:
                  IF INDEX(OrderAction.ItemKey,"MDUB") > 0 OR
                     INDEX(OrderAction.ItemKey,"DATA") > 0 THEN
                     lcItemTypeDesc = "DataBundle".
                  ELSE IF AVAIL Order AND
                     INDEX(OrderAction.ItemKey,Order.CLIType) > 0 THEN
                     lcItemTypeDesc = "TariffBundle".
               END.

               lcMessage = lcMessage                                 + lcDel +
                           fNotNull(STRING(OrderAction.OrderId))     + lcDel +
                           fNotNull(OrderAction.ItemType)            + lcDel +
                           fNotNull(OrderAction.ItemKey)             + lcDel +
                           fNotNull(lcItemTypeDesc).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandlePrepaidRequest:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   
   IF NOT AVAILABLE OrderCanal.RepLog THEN RETURN.

   lcMessage = fCommonMessage().

   CASE RepLog.EventType:
      WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
         FIND PrepaidRequest WHERE
              RECID(PrepaidRequest) = RepLog.RecordId NO-LOCK NO-ERROR.
         IF AVAILABLE PrepaidRequest THEN DO:

            lcMessage = lcMessage                                        + lcDel +
                        fNotNull(STRING(PrepaidRequest.PPRequest))       + lcDel +
                        fNotNull(STRING(PrepaidRequest.MsSeq))           + lcDel +
                        fNotNull(PrepaidRequest.CLI)                     + lcDel +
                        fNotNull(PrepaidRequest.Request)                 + lcDel +
                        fNotNull(PrepaidRequest.Source)                  + lcDel +
                        fNotNull(STRING(PrepaidRequest.TSRequest))       + lcDel +
                        fNotNull(STRING(PrepaidRequest.RespCode))        + lcDel +
                        fNotNull(STRING(PrepaidRequest.PPStatus))        + lcDel +
                        fNotNull(STRING(PrepaidRequest.TopUpAmt / 100))  + lcDel +
                        fNotNull(STRING(PrepaidRequest.VatAmt / 100))    + lcDel +
                        fNotNull(PrepaidRequest.Reference).

            fWriteMessage(lcMessage).
         END.
         ELSE DO:
            olHandled = TRUE.
            fWriteMessage(lcMessage).
            RETURN.
         END.
      END.
      WHEN "DELETE" THEN fWriteMessage(lcMessage).
      OTHERWISE RETURN.
   END CASE.

   IF lMsgPublisher:send_message(lcMessage) THEN
      olHandled = TRUE.
   ELSE DO:
      olHandled = FALSE.
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleBarring:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   
   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST Barring  WHERE
                       RECID(Barring) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE Barring THEN DO:

               lcMessage = lcMessage                         + lcDel +
                           fNotNull(STRING(Barring.MsSeq))   + lcDel +
                           fNotNull(Barring.BarringCode)     + lcDel +
                           fNotNull(Barring.BarringStatus)   + lcDel +
                           fNotNull(Barring.UserCode)        + lcDel +
                           fNotNull(STRING(Barring.EventTS)).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.


PROCEDURE pHandleBarringConf:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.
   
   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.

   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST BarringConf  WHERE
                       RECID(BarringConf) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE BarringConf THEN DO:

               lcMessage = lcMessage                                  + lcDel +
                           fNotNull(STRING(BarringConf.BarringGroup)) + lcDel +
                           fNotNull(BarringConf.BarringCode)          + lcDel +
                           fNotNull(BarringConf.BarringStatus)        + lcDel +
                           fNotNull(BarringConf.BarringCode).       

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.

      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.

PROCEDURE pHandleTermReturn:

   DEFINE OUTPUT PARAMETER olHandled AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE lcMessage         AS CHARACTER NO-UNDO.
   
   IF AVAILABLE OrderCanal.RepLog THEN DO:

      lcMessage = fCommonMessage().

      CASE RepLog.EventType:
         WHEN "CREATE" OR WHEN "MODIFY" THEN DO:
            FIND FIRST TermReturn WHERE
                       RECID(TermReturn) = RepLog.RecordId NO-LOCK NO-ERROR.
            IF AVAILABLE TermReturn THEN DO:

               lcMessage = lcMessage                                    + lcDel +
                           fNotNull(TermReturn.IMEI)                    + lcDel + 
                           fNotNull(STRING(TermReturn.OrderId))         + lcDel +
                           fNotNull(TermReturn.BillCode)                + lcDel +
                           fNotNull(STRING(TermReturn.DeviceStart))     + lcDel +
                           fNotNull(STRING(TermReturn.DeviceScreen))    + lcDel +
                           fNotNull(TermReturn.ReturnChannel)           + lcDel +
                           fNotNull(STRING(TermReturn.ReturnTS)).

               fWriteMessage(lcMessage).
            END.
            ELSE DO:
               olHandled = TRUE.
               fWriteMessage(lcMessage).
               RETURN.
            END.
         END.
         WHEN "DELETE" THEN fWriteMessage(lcMessage).
         OTHERWISE RETURN.
      END CASE.

      IF lMsgPublisher:send_message(lcMessage) THEN
         olHandled = TRUE.
      ELSE DO:
         olHandled = FALSE.
         IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.
   END.

   CATCH anyError AS Progress.Lang.Error:
      olHandled = FALSE.
      LOG-MANAGER:WRITE-MESSAGE("Message failed was recovered: " + lcMessage,"DEBUG").
   END CATCH.

END PROCEDURE.
