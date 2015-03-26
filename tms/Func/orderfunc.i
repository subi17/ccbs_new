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
{commali.i}
{timestamp.i}
{tmsconst.i}
{eventval.i}
{forderstamp.i}
{dextra.i}
{cparam2.i}
{main_add_lines.i}

DEF BUFFER bfOrder  FOR Order.
DEF BUFFER bfOrder2 FOR Order.
DEF BUFFER bfOrderCustomer FOR OrderCustomer.
DEF BUFFER bfOrderCustomer2 FOR OrderCustomer.

/* set status of order */
FUNCTION fSetOrderStatus RETURNS LOGICAL
   (iOrderId AS INT,
    icStatus AS CHAR).

   DEF VAR lcResult AS CHAR NO-UNDO. 
   DEF BUFFER OrderPayment FOR OrderPayment.
   DEF BUFFER CLIType FOR CLIType.

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
               
               IF bfOrder.Logistics > "" THEN DO:
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
                            OrderPayment.Method = 2: /* credit card */
                     CREATE ActionLog.
                     ASSIGN
                        ActionLog.Brand     = gcBrand
                        ActionLog.ActionID  = "OrderCancelRetention"
                        ActionLog.ActionTS  = fMakeTS()
                        ActionLog.TableName = "Order"
                        ActionLog.KeyValue  = STRING(bfOrder.OrderId)
                        ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}.
                  END. /* FOR FIRST OrderPayment NO-LOCK WHERE */

               /* close secondary multisim order */
               IF bfOrder.MultiSimType = {&MULTISIMTYPE_PRIMARY} THEN DO:

                  FOR FIRST bfOrder2 NO-LOCK WHERE
                            bfOrder2.Brand = gcBrand AND
                            bfOrder2.MultiSimID = bfOrder.MultiSimID AND
                            bfOrder2.MultiSimType =
                              {&MULTISIMTYPE_SECONDARY}:
                     RUN closeorder.p(bfOrder2.OrderId,TRUE).
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
                     RUN closeorder.p(bfOrder2.OrderId,TRUE).
                  END.
               END.

            end.
            
         end case. /* case icStatus: */

         fMarkOrderStamp(bfOrder.OrderID,"Change",0.0).

         FIND CURRENT bfOrder NO-LOCK NO-ERROR.
      END. /* IF AVAILABLE bfOrder THEN DO: */
   END. /* DO TRANS: ORDER_TRANS: */
 
   RETURN TRUE.
    
END FUNCTION. 

/* set riskcode of order */
FUNCTION fSetOrderRiskCode RETURNS LOGICAL
   (iOrderId AS INT,
    icRiskCode AS CHAR).
   
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

      FIND CURRENT bfOrder NO-LOCK NO-ERROR.
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
   END CASE.

   RETURN "".

END FUNCTION.


&ENDIF.


