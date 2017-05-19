/*-----------------------------------------------------------------------------
  MODULE .......: orderfunc.i
  FUNCTION .....: General functions for orders
  SOVELLUTUS ...: TMS
  AUTHOR .......: rafaeldv 
  CREATED ......: 
  CHANGED.. ....: 22.07.14  ivvekov - orderstamp is change in all cases
              
  Version ......: 
  -------------------------------------------------------------------------- */

&IF "{&orderfunc}" NE "YES"
&THEN

&GLOBAL-DEFINE orderfunc YES
{Syst/commali.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/forderstamp.i}
{Func/dextra.i}
{Func/cparam2.i}
{Func/main_add_lines.i}
{Func/msisdn.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhOrderStatusChange AS HANDLE NO-UNDO.
END.

/* set status of order */
FUNCTION fSetOrderStatus RETURNS LOGICAL
   (iOrderId AS INT,
    icStatus AS CHAR).

   DEF BUFFER bfOrder  FOR Order.
   DEF BUFFER bfOrder2 FOR Order.
   DEF BUFFER bfOrderCustomer FOR OrderCustomer.
   DEF BUFFER bfOrderCustomer2 FOR OrderCustomer.
   DEF BUFFER MobSub FOR MobSub.

   DEF VAR lcResult   AS CHAR    NO-UNDO. 
   DEF VAR llHardBook AS LOGICAL NO-UNDO INIT FALSE.
   DEF VAR llCancelFusion AS LOGICAL NO-UNDO INIT FALSE.
   DEF VAR lhMSISDN       AS HANDLE  NO-UNDO.

   DEF BUFFER OrderPayment FOR OrderPayment.
   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER OrderFusion FOR OrderFusion.

   ORDER_TRANS:
   DO TRANS:
      FIND bfOrder WHERE
           bfOrder.Brand = gcBrand AND
           bfOrder.OrderId = iOrderId
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bfOrder THEN DO:  
         bfOrder.StatusCode = icStatus.

         /* orders to status new should not be reported to ROI */
         IF LOOKUP(icStatus,"1,3,30") = 0 AND
            bfOrder.Ordertype NE {&ORDER_TYPE_STC} THEN 
            bfOrder.SendToROI  = {&ROI_HISTORY_TO_SEND}.

         /* Mark time stamp, if order statuscode is changed */
         case icStatus:
            when "6" then fMarkOrderStamp(bfOrder.OrderID,"Delivery",0.0).
            when "7" or when "8" or when "9" then do:
               fMarkOrderStamp(bfOrder.OrderID,"Close",0.0).

               FIND FIRST OrderAccessory OF bfOrder WHERE
                          OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}) 
                          NO-LOCK NO-ERROR.
               IF AVAILABLE OrderAccessory AND
                  LOOKUP(STRING(OrderAccessory.HardBook),"1,2") > 0 THEN
                  llHardBook = TRUE.

               FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
                          OrderFusion.Brand = gcBrand AND
                          OrderFusion.OrderID = bfOrder.OrderID AND
                   LOOKUP(OrderFusion.FusionStatus,SUBST("&1,&2",
                         {&FUSION_ORDER_STATUS_NEW},
                         {&FUSION_ORDER_STATUS_ERROR})) > 0 NO-ERROR.

               IF AVAIL OrderFusion THEN DO:
                  ASSIGN
                     OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_CANCELLED}
                     OrderFusion.UpdateTS = fMakeTS().
                  RELEASE OrderFusion.
               END.

               /* Convergent mobile part closing */
               IF fIsConvergenceTariff (bfOrder.CLIType) THEN DO:
                  /* Mark subscription partially terminated */
                  IF bfOrder.OrderType EQ {&ORDER_TYPE_MNP} OR
                     bfOrder.OrderType EQ {&ORDER_TYPE_NEW} THEN DO:
                     FIND FIRST MobSub EXCLUSIVE-LOCK WHERE
                                MobSub.MsSeq = bfOrder.MsSeq AND
                                MobSub.MsStatus = {&MSSTATUS_MOBILE_PROV_ONG}
                                NO-ERROR.
                     IF AVAIL MobSub THEN DO:

                        FIND FIRST MSISDN WHERE
                                   MSISDN.Brand = gcBrand AND
                                   MSISDN.CLI   = MobSub.CLI
                        EXCLUSIVE-LOCK NO-ERROR.
                        /* No mobile created. Release MSISDN */
                        IF AVAIL MSISDN AND MSISDN.StatusCode EQ 3 THEN DO:
                           IF llDoEvent THEN DO:
                              lhMSISDN  = BUFFER MSISDN:HANDLE.
                              RUN StarEventInitialize(lhMSISDN).
                           END.
                           fMakeMsidnHistory(INPUT RECID(MSISDN)).

                           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSISDN).
                           IF fIsYoigoCLI(MobSub.CLI) EQ FALSE THEN
                              MSISDN.StatusCode = {&MSISDN_ST_MNP_OUT_YOIGO}.
                           ELSE MSISDN.StatusCode = {&MSISDN_ST_ASSIGNED_TO_ORDER}.
                           MSISDN.CustNum    = 0.
                           MSISDN.ValidTo = fMakeTS().
                           IF llDoEvent THEN DO:
                              RUN StarEventMakeModifyEvent(lhMSISDN).
                              fCleanEventObjects().
                           END.
                        END.

                        ASSIGN
                           MobSub.CLI = MobSub.FixedNumber
                           MobSub.ICC = ""
                           MobSub.IMSI = ""
                           MobSub.MsStatus = {&MSSTATUS_MOBILE_NOT_ACTIVE}.
                        /* Update MSOwner accordingly */
                        &IF DEFINED(STAR_EVENT_USER) = 0 
                        &THEN 
                           &GLOBAL-DEFINE STAR_EVENT_USER "OrderClose"
                        &ENDIF
                        fUpdatePartialMSOwner(bfOrder.MsSeq, MobSub.FixedNumber).
                        RELEASE MobSub.
                     END.
                  END. /* IF bfOrder.OrderType EQ */
               END. /* IF fIsConvergenceTariff  */

               FIND FIRST FusionMessage EXCLUSIVE-LOCK WHERE
                          FusionMessage.orderID EQ bfOrder.OrderId AND
                          FusionMessage.MessageType EQ {&FUSIONMESSAGE_TYPE_LOGISTICS} 
               NO-ERROR.

               IF AVAIL FusionMessage THEN DO:

                  IF FusionMessage.MessageStatus NE {&FUSIONMESSAGE_STATUS_NEW} THEN
                     llCancelFusion = TRUE.

                  IF FusionMessage.MessageStatus NE {&FUSIONMESSAGE_STATUS_HANDLED} THEN
                     ASSIGN
                        FusionMessage.UpdateTS = fMakeTS()
                        FusionMessage.FixedStatusDesc = "Order closed"
                        FusionMessage.messageStatus = 
                           {&FUSIONMESSAGE_STATUS_CANCELLED}.
                  RELEASE FusionMessage.
               END.
               
               IF katun NE "Dextra" AND
                 (bfOrder.Logistics > "" OR llHardBook = TRUE OR
                 llCancelFusion = TRUE) THEN DO:
                  fLogisticsRequest(
                     bfOrder.MsSeq,
                     bfOrder.OrderId,
                     "CANCEL",
                     fMakeTS(),
                     {&REQUEST_SOURCE_ORDER_CANCELLATION},
                     OUTPUT lcResult).

                  IF lcResult > "" THEN 
                     DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                      "Order",
                                      STRING(bfOrder.OrderID),
                                      0,
                                      "Logistics cancel failed",
                                      lcResult).
               END.

               /* YDR-695 */
               IF bfOrder.InvNum = 0 AND
                  bfOrder.OrderChannel BEGINS "retention" THEN
                  FOR FIRST OrderPayment NO-LOCK WHERE
                            OrderPayment.Brand = gcBrand AND
                            OrderPayment.OrderId = bfOrder.OrderId AND
                           (OrderPayment.Method = {&ORDERPAYMENT_M_CREDIT_CARD}
                            OR
                            OrderPayment.Method = {&ORDERPAYMENT_M_PAYPAL}):

                     CREATE ActionLog.
                     ASSIGN
                        ActionLog.Brand     = gcBrand
                        ActionLog.ActionID  = "OrderCancelRetention"
                        ActionLog.ActionTS  = fMakeTS()
                        ActionLog.TableName = "Order"
                        ActionLog.KeyValue  = STRING(bfOrder.OrderId)
                        ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}.
                  END. /* FOR FIRST OrderPayment NO-LOCK WHERE */

               /* YTS-8312 */
               IF bfOrder.OrderType EQ {&ORDER_TYPE_RENEWAL} AND
                  bfOrder.ICC > "" AND
                  NOT bfOrder.OrderChannel BEGINS "Renewal_POS" AND
                  bfOrder.Logistics EQ "" THEN DO:
                  FIND MsRequest NO-LOCK WHERE
                       MsRequest.MsSeq = bfOrder.MsSeq AND
                       MsRequest.Reqtype = 15 AND
                       MsRequest.ReqStatus = 19 AND
                       MsRequest.ReqCParam2 = bfOrder.ICC AND
                       MsRequest.ReqSource = {&REQUEST_SOURCE_ICC_CHANGE_AUTO} 
                       NO-ERROR.
                  IF AVAIL MsRequest THEN
                     fChangeReqStatus(MsRequest.MsRequest,
                                      4, "Cancelled by order closing").
               END.

               /* close secondary multisim order */
               IF bfOrder.MultiSimType = {&MULTISIMTYPE_PRIMARY} THEN DO:

                  FOR FIRST bfOrder2 NO-LOCK WHERE
                            bfOrder2.Brand = gcBrand AND
                            bfOrder2.MultiSimID = bfOrder.MultiSimID AND
                            bfOrder2.MultiSimType =
                              {&MULTISIMTYPE_SECONDARY}:
                     RUN Mc/closeorder.p(bfOrder2.OrderId,TRUE).
                  END.

               END.

               /* release pending secondary line orders, YDR-1089 */
               IF CAN-FIND(FIRST OrderAction WHERE
                                 OrderAction.Brand = gcBrand AND
                                 OrderAction.OrderId = bfOrder.OrderID AND
                                 OrderAction.ItemType = "BundleItem" AND
                                 CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                                CLIType.Brand = gcBrand AND
                                                CLIType.CLIType = OrderAction.ItemKey AND
                                                CLIType.LineType = {&CLITYPE_LINETYPE_MAIN})) THEN DO:
               
                  FIND FIRST bfOrderCustomer OF bfOrder NO-LOCK WHERE
                             bfOrderCustomer.RowType EQ 1 NO-ERROR.

                  IF AVAIL bfOrderCustomer AND
                     NOT fIsMainLineOrderPending(
                        bfOrderCustomer.CustIdType,
                        bfOrderCustomer.CustId,
                        bfOrder.OrderID) THEN
                  FOR EACH bfOrderCustomer2 NO-LOCK WHERE   
                           bfOrderCustomer2.Brand      EQ gcBrand AND 
                           bfOrderCustomer2.CustId     EQ bfOrderCustomer.CustID AND
                           bfOrderCustomer2.CustIdType EQ bfOrderCustomer.CustIdType AND
                           bfOrderCustomer2.RowType    EQ 1,
                      EACH bfOrder2 NO-LOCK WHERE
                           bfOrder2.Brand              EQ gcBrand AND
                           bfOrder2.orderid            EQ bfOrderCustomer2.Orderid AND
                           ROWID(bfOrder2) NE ROWID(bfOrder) AND
                           (bfOrder2.statuscode        EQ {&ORDER_STATUS_PENDING_MAIN_LINE} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_ROI_LEVEL_1} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_ROI_LEVEL_2} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_ROI_LEVEL_3} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_MORE_DOC_NEEDED} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_MNP_ON_HOLD}),
                      FIRST CLIType NO-LOCK WHERE
                            CLIType.Brand = gcBrand AND
                            CLIType.CLIType = bfOrder2.CLIType AND
                            CLIType.LineType > 0:

                     IF CAN-FIND(FIRST OrderAction WHERE
                                       OrderAction.Brand = gcBrand AND
                                       OrderAction.OrderId = bfOrder2.OrderID AND
                                       OrderAction.ItemType = "BundleItem" AND
                              CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                             CLIType.Brand = gcBrand AND
                                             CLIType.CLIType = OrderAction.ItemKey AND
                                             CLIType.LineType = {&CLITYPE_LINETYPE_MAIN})) THEN NEXT.
                     RUN Mc/closeorder.p(bfOrder2.OrderId,TRUE).
                  END.
               END.

            end.
            
         end case. /* case icStatus: */

         fMarkOrderStamp(bfOrder.OrderID,"Change",0.0).

         RELEASE bfOrder.
      END. /* IF AVAILABLE bfOrder THEN DO: */
   END. /* DO TRANS: ORDER_TRANS: */
 
   RETURN TRUE.
    
END FUNCTION. 

/* set riskcode of order */
FUNCTION fSetOrderRiskCode RETURNS LOGICAL
   (iOrderId AS INT,
    icRiskCode AS CHAR).

   DEF BUFFER bfOrder  FOR Order.
   
   ORDER_TRANS:
   DO TRANS:
      FIND bfOrder WHERE
           bfOrder.Brand = gcBrand AND
           bfOrder.OrderId = iOrderId
         EXCLUSIVE-LOCK NO-ERROR.
      /* another process is handling this */
      IF LOCKED(bfOrder) THEN RETURN FALSE.
      
      ELSE IF AVAILABLE bfOrder THEN
         ASSIGN bfOrder.RiskCode   = icRiskCode
                bfOrder.SendToROI  = {&ROI_HISTORY_TO_SEND} WHEN
                        bfOrder.OrderType NE {&ORDER_TYPE_STC}.

      RELEASE bfOrder.
   END. /* DO TRANS: ORDER_TRANS: */
 
   RETURN TRUE.
    
END FUNCTION. 

/* Return SIM Only billing item */
FUNCTION fGetSIMBillItem RETURNS CHARACTER
   (icSIMType AS CHAR,
    ilPayTpe  AS LOG).

   CASE icSIMType:
      WHEN "Plug_IN" THEN DO:
         IF ilPayTpe THEN RETURN "TS00000R1".
         ELSE RETURN "TS00000R3".
      END.
      WHEN "Micro" THEN DO:
         IF ilPayTpe THEN RETURN "TS00000M1".
         ELSE RETURN "TS00000M3".
      END.
      WHEN "Nano" THEN DO:
         IF ilPayTpe THEN RETURN "TS00000N1".
         ELSE RETURN "TS00000N3".
      END.
      WHEN "Universal" THEN DO:
         IF ilPayTpe THEN RETURN "TS00000U1".
         ELSE RETURN "TS00000U3".
      END.
   END CASE.

   RETURN "".

END FUNCTION.

FUNCTION fSearchStock RETURNS CHARACTER
   (icStock AS CHAR,
   icZipCode AS CHAR).

   DEF VAR liLoop AS INT NO-UNDO. 

   FOR EACH Stock WHERE
            Stock.Brand   = gcBrand AND
            Stock.StoType = icStock NO-LOCK:
      DO liLoop = 1 TO NUM-ENTRIES(Stock.ZipCodeExp,","):
        IF icZipCode MATCHES
           ENTRY(liLoop, Stock.ZipCodeExp,",") THEN DO:
           RETURN  Stock.Stock.
        END.
      END.
   END.
   
   RETURN icStock.

END FUNCTION.

/* Function releases OR CLOSE Additional lines */
FUNCTION fReleaseORCloseAdditionalLines RETURN LOGICAL
   (INPUT icCustIDType  AS CHAR,
    INPUT icCustID      AS CHAR):

   DEF BUFFER labOrder         FOR Order.
   DEF BUFFER labOrderCustomer FOR OrderCustomer.

   DEF VAR lcNewOrderStatus AS CHAR NO-UNDO.

   FOR EACH labOrderCustomer NO-LOCK WHERE
            labOrderCustomer.Brand      EQ Syst.Parameters:gcBrand AND
            labOrderCustomer.CustId     EQ icCustID                AND
            labOrderCustomer.CustIdType EQ icCustIDType            AND
            labOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH labOrder NO-LOCK WHERE
            labOrder.Brand      EQ Syst.Parameters:gcBrand  AND
            labOrder.orderid    EQ labOrderCustomer.Orderid AND
            labOrder.statuscode EQ {&ORDER_STATUS_PENDING_MAIN_LINE}:

      IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.Brand      = Syst.Parameters:gcBrand           AND
                        CLIType.CLIType    = labOrder.CLIType                  AND
                        CLIType.LineType   = {&CLITYPE_LINETYPE_NONMAIN}       AND
                        CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY}) THEN DO: 
         
         CASE labOrder.OrderType:
            WHEN {&ORDER_TYPE_NEW} THEN
                 lcNewOrderStatus = IF labOrderCustomer.CustIdType EQ "CIF" THEN {&ORDER_STATUS_COMPANY_NEW}
                                    ELSE {&ORDER_STATUS_NEW}.
            WHEN {&ORDER_TYPE_MNP} THEN
                 lcNewOrderStatus = IF labOrderCustomer.CustIdType EQ "CIF" THEN {&ORDER_STATUS_COMPANY_MNP}
                                    ELSE {&ORDER_STATUS_MNP}.
            WHEN {&ORDER_TYPE_RENEWAL} THEN
                 lcNewOrderStatus = IF labOrderCustomer.CustIdType EQ "CIF" THEN {&ORDER_STATUS_RENEWAL_STC_COMPANY}
                                    ELSE {&ORDER_STATUS_RENEWAL_STC}.
            OTHERWISE.
         END CASE.

         IF lcNewOrderStatus > "" THEN DO:
            IF llDoEvent THEN DO:
               lhOrderStatusChange = BUFFER labOrder:HANDLE.
               RUN StarEventInitialize(lhOrderStatusChange).
               RUN StarEventSetOldBuffer(lhOrderStatusChange).
            END.

            fSetOrderStatus(labOrder.OrderId,lcNewOrderStatus).

            IF llDoEvent THEN DO:
               RUN StarEventMakeModifyEvent(lhOrderStatusChange).
               fCleanEventObjects().
            END.
         END.
      END.
   END.   
   
   RETURN TRUE.

END FUNCTION.   

&ENDIF.


