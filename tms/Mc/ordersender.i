/* ordersender.i   

   changes:      05.12.06/aam print order confirmation
                 12.12.06/aam check prechosen icc, write memos
                 15.12.06/aam all checks before prinoconf 
                 21.12.06/tk  if cli = "" set orderstatus to 2
                 28.12.06/tk  check prechosen icc only for non-mnp orders
*/

            &IF "{&localvar}" NE "YES" &THEN
                &GLOBAL-DEFINE localvar YES
                DEF VAR llOrdStChg            AS LOG  NO-UNDO. 
                DEF VAR llReserveSimAndMsisdn AS LOG  NO-UNDO.
                DEF VAR llgMNPSimOnly         AS LOG  NO-UNDO INITIAL NO.  

                DEF VAR lh99Order AS HANDLE NO-UNDO.
                DEF VAR lh76Order AS HANDLE NO-UNDO.
                DEF VAR lh17Order AS HANDLE NO-UNDO.
                DEF VAR lh15Order AS HANDLE NO-UNDO.
            &ENDIF

            RELEASE SIM.
                
            /* Create separate activation request for NEW/MNP convergent fixed lines */
            IF fIsConvergenceTariff(Order.CLIType) AND
               (Order.OrderType EQ {&ORDER_TYPE_MNP} OR
                Order.OrderType EQ {&ORDER_TYPE_NEW}) AND
               NOT CAN-FIND(FIRST MsRequest WHERE
                                  MsRequest.MsSeq   = Order.MSSeq  AND
                                  MsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE})
               THEN DO:

               FIND OrderFusion NO-LOCK WHERE
                    OrderFusion.Brand   = Order.Brand AND
                    OrderFusion.OrderID = Order.OrderID NO-ERROR.

               IF NOT AVAIL OrderFusion OR
                  NOT OrderFusion.FixedInstallationTS > 0 THEN DO:

                  fSetOrderStatus(Order.OrderID, {&ORDER_STATUS_ERROR}).
                  
                  Func.Common:mWriteMemo("Order",
                                   STRING(Order.OrderID),
                                   0,
                                   "FIXED LINE ACTIVATION FAILED",
                                   "Missing fixed line installation timestamp").
                  NEXT {1}.
               END.
               
               fSubscriptionRequest(INPUT  Order.MSSeq,
                                    INPUT  Order.Cli,
                                    INPUT  Order.CustNum,
                                    INPUT  1,
                                    INPUT  Syst.Var:katun,
                                    INPUT  OrderFusion.FixedInstallationTS,
                                    INPUT  "CREATE-FIXED",
                                    INPUT  STRING(Order.OrderId),
                                    INPUT  "",
                                    INPUT  "", /*for old SIM*/
                                    INPUT  "", /*for Reason info*/
                                    INPUT  "", /*for ContractID*/
                                    INPUT  FALSE,
                                    INPUT  0,
                                    INPUT  {&REQUEST_SOURCE_NEWTON},
                                    OUTPUT ocResult).

               IF ocResult > "" THEN DO:
                  llOrdStChg = fSetOrderStatus(Order.OrderId,
                                               {&ORDER_STATUS_ERROR}).

                  Func.Common:mWriteMemo("Order",
                                   STRING(Order.OrderID),
                                   0,
                                   "FIXED LINE ACTIVATION FAILED",
                                   ocResult).
                  NEXT {1}.
               END.

               IF (Order.OrderType EQ {&ORDER_TYPE_NEW} OR
                   Order.OrderType EQ {&ORDER_TYPE_MNP}) AND
                   CAN-FIND(FIRST Memo NO-LOCK WHERE
                                  Memo.Brand = Syst.Var:gcBrand AND
                                  Memo.HostTable = "Order" AND
                                  Memo.Keyvalue = STRING(Order.OrderID) AND
                                  Memo.MemoText = "Fixed Cancellation failed because installation was already in place") THEN DO:
                  fSetOrderStatus(Order.OrderID,
                                  {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}).
                  NEXT {1}.
               END.

               /* NOTE: this check is also in orderinctrl.p */
               IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
                  Order.ICC EQ "" THEN DO:
                  fSetOrderStatus(Order.OrderID,
                                  {&ORDER_STATUS_PENDING_MOBILE_LINE}).
                  NEXT {1}.
               END.
               
               IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) GT 0 AND
                  Func.ValidateOrder:mIsFiberType(Order.CLIType)          AND 
                  Order.ICC       EQ     ""                               AND 
                  Order.OrderType NE     {&ORDER_TYPE_STC}                THEN DO:

                  IF llDoEvent THEN DO:
                     lh17Order = BUFFER Order:HANDLE.
                     RUN StarEventInitialize(lh17Order).
                     RUN StarEventSetOldBuffer(lh17Order).
                  END.

                  IF Order.DeliverySecure > 0 THEN 
                     fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_SENDING_TO_LO}).
                  ELSE fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}).

                  IF llDoEvent THEN DO:
                     RUN StarEventMakeModifyEvent(lh17Order).
                     fCleanEventObjects().
                  END.

                  NEXT {1}.
               END.   
               
               /* NOTE: this check is also in orderinctrl.p */
               IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
                  Order.PortingDate <> ? THEN DO:
                  fSetOrderStatus(Order.OrderID,
                                  {&ORDER_STATUS_MNP_ON_HOLD}).
                  NEXT {1}.
               END.
            END. /* fIsConvergenceTariff AND ...*/

            ASSIGN llOrdStChg = no.
            
            IF (Order.OrderType EQ {&ORDER_TYPE_MNP} OR
                Order.OrderType EQ {&ORDER_TYPE_NEW}) AND
                LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
                Order.ICC EQ "" THEN NEXT {1}.
            
            /* Move Additional line or Extra line discount Mobile only tariff order to 76 queue, 
               if associated customer main line considered order is in ongoing status  */
            IF Order.StatusCode NE {&ORDER_STATUS_ONGOING}                           AND
               CAN-FIND(FIRST CLIType NO-LOCK WHERE 
                              CLIType.Brand      = Syst.Var:gcBrand                           AND 
                              CLIType.CLIType    = Order.CLIType                     AND 
                              CLIType.PayType    = {&CLITYPE_PAYTYPE_POSTPAID}       AND
                              CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY}) THEN DO: 
 
               FIND FIRST OrderCustomer NO-LOCK WHERE
                          OrderCustomer.Brand   = Syst.Var:gcBrand                            AND
                          OrderCustomer.OrderId = Order.OrderId                      AND
                          OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

               IF AVAIL OrderCustomer THEN
               DO:
                  /* Additional lines Mobile only tariffs */ 
                  IF (CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                                     OrderAction.Brand    = Syst.Var:gcBrand           AND
                                     OrderAction.OrderID  = Order.OrderId     AND
                                     OrderAction.ItemType = "AddLineDiscount" AND
                              LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS}) > 0)    
                      AND
                      NOT fCheckExistingConvergent(OrderCustomer.CustIDType,
                                                   OrderCustomer.CustID,
                                                   Order.CLIType)                       
                      AND
                      fCheckOngoingConvergentOrder(OrderCustomer.CustIdType,
                                                   OrderCustomer.CustId,
                                                   Order.CLIType)               
                      AND 
                      NOT fCheckFixedLineStatusForMainLine(OrderCustomer.CustIdType,
                                                           OrderCustomer.CustId,
                                                           Order.CLIType))              OR
                     (CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                                     OrderAction.Brand    = Syst.Var:gcBrand           AND
                                     OrderAction.OrderID  = Order.OrderId     AND
                                     OrderAction.ItemType = "AddLineDiscount" AND
                              LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_20}) > 0) 
                      AND
                      NOT fCheckExisting2PConvergent(OrderCustomer.CustIDType,
                                                     OrderCustomer.CustID,
                                                     Order.CLIType)                     
                      AND
                      fCheckOngoing2PConvergentOrder(OrderCustomer.CustIdType,
                                                     OrderCustomer.CustId,
                                                     Order.CLIType)                       
                      AND 
                      NOT fCheckFixedLineStatusForMainLine(OrderCustomer.CustIdType,
                                                           OrderCustomer.CustId,
                                                           Order.CLIType))              OR      
                     (CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                                     OrderAction.Brand    = Syst.Var:gcBrand           AND
                                     OrderAction.OrderID  = Order.OrderId     AND
                                     OrderAction.ItemType = "AddLineDiscount" AND
                             LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0) 
                      AND
                      NOT fCheckExistingMobileOnly(OrderCustomer.CustIDType,
                                                   OrderCustomer.CustID,
                                                   Order.CLIType) 
                      AND
                      fCheckOngoingMobileOnly(OrderCustomer.CustIdType,
                                              OrderCustomer.CustId,
                                              Order.CLIType)) THEN DO:
                     IF llDoEvent THEN DO:
                        lh76Order = BUFFER Order:HANDLE.
                        RUN StarEventInitialize(lh76Order).
                        RUN StarEventSetOldBuffer(lh76Order).
                     END.

                     fSetOrderStatus(Order.OrderID,
                                     {&ORDER_STATUS_PENDING_MAIN_LINE}).

                     IF llDoEvent THEN DO:
                        RUN StarEventMakeModifyEvent(lh76Order).
                        fCleanEventObjects().
                     END.

                     NEXT {1}.
                  END.

                  /* While processing Extra line mobile only orders, check if its 
                     associated main line (fixed line) is installed. If it is installed 
                     THEN don't move extra line order to 76 status */
                  IF Order.MultiSimId     <> 0                                 AND 
                     Order.MultiSimType   = {&MULTISIMTYPE_EXTRALINE}          AND 
                     CAN-FIND(FIRST OrderAction NO-LOCK WHERE 
                                    OrderAction.Brand    = Syst.Var:gcBrand    AND 
                                    OrderAction.OrderID  = Order.OrderID       AND
                                    OrderAction.ItemType = "ExtraLineDiscount" AND  
                                    OrderAction.ItemKey  = Order.CLIType + "DISC") THEN 
                  DO:
                     
                     /* Check Mainline Convergent is delivered or closed, 
                        if true then don't move extraline to 76 (Pending mainline status) */
                     IF NOT fCheckMainLineOrderStatus(Order.MultiSimId) THEN   /* Mainline Order Id */
                     DO:
                        IF llDoEvent THEN DO:
                           lh76Order = BUFFER Order:HANDLE.
                           RUN StarEventInitialize(lh76Order).
                           RUN StarEventSetOldBuffer(lh76Order).
                        END.

                        fSetOrderStatus(Order.OrderID,
                                        {&ORDER_STATUS_PENDING_MAIN_LINE}).

                        IF llDoEvent THEN DO:
                           RUN StarEventMakeModifyEvent(lh76Order).
                           fCleanEventObjects().
                        END.

                        NEXT {1}.
                     END.

                  END.
               END.
            END.    
            
            /* Check if order is MNP SimOnly */
            llgMNPSimOnly = Order.OrderType EQ 1                                   AND
                            Order.CrStamp   >= 20150616.40200                      AND
                            Order.MNPStatus EQ 1                                   AND
                     LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0        AND
               NOT CAN-FIND(FIRST OrderAccessory NO-LOCK WHERE
                                  OrderAccessory.Brand   EQ Syst.Var:gcBrand AND
                                  OrderAccessory.OrderId EQ Order.OrderID)         AND
               NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                                 OrderTimeStamp.Brand   EQ Syst.Var:gcBrand   AND
                                 OrderTimeStamp.OrderID EQ Order.OrderID      AND
                                 OrderTimeStamp.RowType EQ {&ORDERTIMESTAMP_SIMONLY}).

            /* New change in SIM reservation logic, when order is placed from telesalses
               or web then order will be moved to order queue 15 0r 16. When order sent to LO,
               LO will pick and reserve the SIM through external API (dextra_update_order_status) */
            IF (Order.OrderType EQ {&ORDER_TYPE_NEW} OR
               (Order.OrderType EQ {&ORDER_TYPE_MNP} AND 
                  (Order.StatusCode EQ {&ORDER_STATUS_ONGOING} OR llgMNPSimOnly))) AND
         LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 AND
                Order.ICC EQ ""                                 THEN DO:

               IF llDoEvent THEN DO:
                  lh15Order = BUFFER Order:HANDLE.
                  RUN StarEventInitialize(lh15Order).
                  RUN StarEventSetOldBuffer(lh15Order).
               END.

               IF CAN-FIND(FIRST OrderGroup NO-LOCK WHERE
                                 OrderGroup.OrderId        EQ Order.OrderId            AND
                                 OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
                         ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN
                  fSetOrderStatus(Order.OrderID,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).
               ELSE fSetOrderStatus(Order.OrderID,{&ORDER_STATUS_SENDING_TO_LO}).

               IF llDoEvent THEN DO:
                  RUN StarEventMakeModifyEvent(lh15Order).
                  fCleanEventObjects().
               END.

               NEXT {1}.

            END. 

            /* YDR-1825 MNP SIM ONLY Orders
              Additional ordertimestamp is to prevent infinitive loop */
            IF llgMNPSimOnly THEN DO:
                  
                  /* Event logging of 99 status setups */ 
                  IF llDoEvent THEN DO:
                     lh99Order = BUFFER Order:HANDLE.
                     RUN StarEventInitialize(lh99Order).
                     RUN StarEventSetOldBuffer(lh99Order).
                  END.
                  
                  /* Set Order status into MNP_SIM_ONLY aka 99 */
                  llOrdStChg = fSetOrderStatus(Order.OrderId,
                                               {&ORDER_STATUS_SIM_ONLY_MNP_IN}).
                  IF llDoEvent THEN DO:
                     RUN StarEventMakeModifyEvent(lh99Order).
                     fCleanEventObjects().
                  END.

                  /* Set additional OrderStamp to avoid infinitive loop */
                  fMarkOrderStamp(Order.OrderID,"SimOnly",0.0).

                  NEXT {1}.
            END. /*MNP SIM ONLY Orders from direct channel*/

               
            /* Renove handling */ 
            IF Order.OrderType = {&ORDER_TYPE_RENEWAL} THEN DO:
              
               /* YBP-588 */
               /* prevent duplicate renove request creation */
               IF Order.OrderChannel BEGINS "Renewal_POS" AND
               CAN-FIND(FIRST MsRequest WHERE
                              MsRequest.MsSeq   = Order.MSSeq  AND
                              MsRequest.ReqType = 46 AND
                              MsRequest.ReqIParam1 = Order.OrderID)
               THEN DO:
                /* if order status is not ONGOING aftersales request handling
                   will fail */
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"12").
                  NEXT {1}.
               END.
               
               /* YBP-593 */ 
               /* if order status is not ONGOING aftersales request handling
                  will fail */
               IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT_RENEWAL}) > 0 THEN
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"78").   /* Waiting sending to LO */
               ELSE llOrdStChg = fSetOrderStatus(Order.OrderId,"12"). /* Ongoing */

               /* YBP-595 */
               IF Order.OrderChannel BEGINS "Renewal_POS" THEN DO:
                  fAfterSalesRequest(
                     Order.MsSeq,
                     Order.OrderId,
                     Syst.Var:katun,
                     Func.Common:mMakeTS(),
                     "7",
                     OUTPUT ocResult
                     ).
                   
                  IF ocResult > "" THEN DO:
                     Func.Common:mWriteMemo("Order",
                                      STRING(Order.OrderID),
                                      0,
                                      "After Sales Request creation failed",
                                      ocResult).
                     llOrdStChg = fSetOrderStatus(Order.OrderId,"4").
                  END.
               END.
                 
               RELEASE Order.
               NEXT {1}.
            END. /* renewal / IF Order.OrderType = 2 THEN DO: */

            IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN DO:
 
               /* YBP-596 */ 
               RUN Mm/fusion_stc.p(Order.OrderID, OUTPUT liRequestID).
               
               IF liRequestID > 0 THEN
                  /* YBP-597 */ 
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"12").
               ELSE DO:
                  /* YBP-598 */ 
                  Func.Common:mWriteMemo("Order",
                                   STRING(Order.OrderID),
                                   0,
                                   "STC request creation failed",
                                   RETURN-VALUE).
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"4").
               END.

               RELEASE Order.
               NEXT {1}.
            END.
            
            IF Order.OrderType EQ {&ORDER_TYPE_ACC} THEN DO:

               RUN Mm/acc_order.p(Order.OrderID, OUTPUT liRequestID).
               
               IF liRequestID > 0 THEN
                  /* YBP-597 */ 
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"12").
               ELSE DO:
                  /* YBP-598 */ 
                  Func.Common:mWriteMemo("Order",
                                   STRING(Order.OrderID),
                                   0,
                                   "ACC request creation failed",
                                   RETURN-VALUE).
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"4").
               END.

               RELEASE Order.
               NEXT {1}.
            
            END.
             
             /* YBP-594 */ 
             IF Order.OrderType NE {&ORDER_TYPE_ROLLBACK} AND
                CAN-FIND(FIRST MsRequest WHERE
                               MsRequest.MsSeq   = Order.MSSeq  AND
                               MsRequest.ReqType = 13 AND
                               MsRequest.ReqStatus NE {&REQUEST_STATUS_CANCELLED})
             THEN DO: 
                NEXT.
             END.

             IF LOCKED(Order) THEN NEXT.
             
             IF order.CLI = "" THEN DO:

                   /* YBP-599 */ 
                   Func.Common:mWriteMemo("Order",
                                    STRING(Order.OrderID),
                                    0,
                                    "MSISDN",
                                    "MSISDN is missing").
                   /* YBP-600 */ 
                   llOrdStChg = fSetOrderStatus(Order.OrderId,"2"). /*added by jaakko 8.12.06*/

                   NEXT {1}.
             END.
             
             DEC(order.cli) NO-ERROR.
             IF ERROR-STATUS:ERROR THEN DO:

                llOrdStChg = fSetOrderStatus(Order.OrderId,"2"). 

                Func.Common:mWriteMemo("Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "MSISDN",
                                 "MSISDN is invalid").

                NEXT {1}.
             END.

             IF Order.MSSeq = 0 THEN DO:
                Order.MSSeq = NEXT-VALUE(Mobsub).
             END.

             /* YBP-601 */ 
             FOR EACH OrderCustomer OF Order EXCLUSIVE-LOCK:
                IF OrderCustomer.Language = "0" OR
                   OrderCustomer.Language = ""
                THEN OrderCustomer.Language = "1".
             END.
              
             /* YBP-602 */ 
             IF Order.ResignationPeriod THEN DO:
                llReserveSimAndMsisdn =  
                NOT CAN-FIND(
                FIRST ActionLog NO-LOCK WHERE
                      ActionLog.Brand  = Syst.Var:gcBrand AND
                      ActionLog.TableName = "Order" AND
                      ActionLog.KeyValue = STRING(Order.OrderID) AND
                      ActionLog.ActionId = "RESIGNATION" AND
                      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}).
             END.
             ELSE llReserveSimAndMsisdn = TRUE.

             /* check that chosen icc is available */    
             IF Order.ICC > "" THEN DO:

                FIND FIRST Sim WHERE Sim.ICC = Order.ICC NO-LOCK NO-ERROR.
                   
                IF NOT AVAILABLE SIM THEN DO:

                   llOrdStChg = fSetOrderStatus(Order.OrderId,"2").  /* error */
                   /* YBP-603 */ 
                   Func.Common:mWriteMemo("Order",
                                    STRING(Order.OrderID),
                                    0,
                                    "Order Process Error",
                                    "No valid ICC could be found").
                   NEXT {1}.                  
                END.
               
             END.

             IF LOOKUP(Order.Statuscode,{&ORDER_INACTIVE_STATUSES} + ",4,74") > 0 THEN DO:
                NEXT {1}.
             END.
                
             IF order.clitype = "" THEN DO:
                /* YBP-608 */
                llOrdStChg = fSetOrderStatus(Order.OrderId,"4").
                /* YBP-609 */
                Func.Common:mWriteMemo("Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "CLI Type",
                                 "CLI type is missing").
                NEXT {1}.  
             END.

             FIND FIRST clitype WHERE
                        clitype.brand   = Syst.Var:gcBrand and
                        clitype.clitype = order.clitype NO-LOCK NO-ERROR.
             
             IF NOT AVAIL clitype THEN DO:

                llOrdStChg = fSetOrderStatus(Order.OrderId,"2"). /* ERROR */
                /* YBP-610 */
                Func.Common:mWriteMemo("Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "CLI Type",
                                 "Unknown CLI type").
                
                NEXT {1}.
             END.

             IF NOT fIsConvergenceTariff(Order.CLIType) AND
                CAN-FIND(FIRST MobSub WHERE
                               MobSub.CLI = Order.CLI) THEN DO:          

                llOrdStChg = fSetOrderStatus(Order.OrderId,"2"). /* error */
                /* YBP-611 */
                Func.Common:mWriteMemo("Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "Subscription",
                                 "Subscription already exists with MSISDN" +
                                    " " + Order.CLI).
                NEXT {1}.
             END.
             
            /* YBP-612 */
            FIND FIRST OrderCustomer WHERE
                       OrderCustomer.Brand = Syst.Var:gcBrand AND
                       OrderCustomer.OrderId = Order.OrderId AND
                       OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

 
             /* print order confirmation, this is done also for mnp orders
                but not for gift or preactivated or vip orders
             */  
             IF LOOKUP(Order.OrderChannel,"Yoigo,Pre-act,vip,migration,migration_ore") = 0 AND
                OrderCustomer.Email NE "" AND
                Order.OrderType <> 3 AND Order.OrderType <> 4 AND
                (Order.MnpStatus = 0 OR Order.StatusCode = "3") THEN DO:  
                
                /* YBP-613 */
                RUN Mc/prinoconf.p (Order.OrderID).
                
                IF RETURN-VALUE BEGINS "ERROR" THEN DO:
                   Func.Common:mWriteMemo("Order",
                                    STRING(Order.OrderID),
                                    0,
                                    "Order Confirmation Failed",
                                    RETURN-VALUE).
                END.
             END.
  
             IF Order.StatusCode EQ {&ORDER_STATUS_MNP} /*3*/ THEN DO:
                
                IF Order.SalesMan EQ "order_correction_mnp" AND
                   LOOKUP(Order.OrderChannel,
                          "telesales,fusion_telesales,pos,fusion_pos," +
                          "telesales_pro,fusion_telesales_pro,pos_pro," +
                          "fusion_pos_pro") > 0 THEN
                   /* YBP-620 */
                   Order.MNPStatus = 6. /* fake mnp process (ACON) */
                /*MB_Migration has special MNP/Migration handler*/
                ELSE IF Order.Orderchannel BEGINS "migration" THEN DO:
                   Order.StatusCode = {&ORDER_STATUS_MIGRATION_PENDING}. /*60*/
                END.
                ELSE IF Order.MNPStatus NE 6 THEN DO: /* Confirmed MNP */
                   /* YBP-621 */
                   RUN Mnp/mnprequestnc.p(order.orderid).
                   /* YBP-622 */
                   IF RETURN-VALUE EQ "ERROR:AREC CUPO4" THEN
                   llOrdStChg = fSetOrderStatus(Order.OrderId,
                                                {&ORDER_STATUS_MNP_PENDING}).
                   ELSE llOrdStChg = fSetOrderStatus(Order.OrderId,"12").
                   NEXT {1}.
                END.
                
             END.
             
             /* new mnp */
             IF llReserveSimAndMsisdn THEN DO:

               IF Order.ICC > "" THEN DO:
                      
                  FIND Sim WHERE Sim.ICC = Order.ICC 
                  EXCLUSIVE-LOCK NO-ERROR.

                  /* free sim was not available */
                  IF NOT AVAILABLE Sim THEN NEXT {1}.
               END.
               ELSE RELEASE SIM.
             
               FIND FIRST MSISDNNUMBER WHERE 
                          MSISDNNumber.CLI = Order.CLI NO-LOCK NO-ERROR.

               IF NOT AVAIL MSISDNNumber THEN DO:
                  CREATE msisdnNumber.
                  MSISDNNumber.CLI = Order.CLI.
               END.
                
               FIND FIRST MSISDN WHERE
                          MSISDN.Brand = Syst.Var:gcBrand AND
                          MSISDN.CLI   = Order.Cli NO-LOCK NO-ERROR.
                          
               IF NOT AVAIL MSISDN THEN DO:
                  CREATE MSISDN.
                  ASSIGN
                     MSISDN.ActionDate = TODAY 
                     msisdn.ValidFrom  = Func.Common:mMakeTS()
                     Msisdn.cli        = order.cli 
                     Msisdn.StatusCode = 22
                     MSISDN.MSSeq      = Order.MSSeq
                     MSISDN.Brand      = Syst.Var:gcBrand.
               END.          

               /*MM Migration: Subscription creation will be done after NC
                 response. */
               IF Order.Orderchannel BEGINS "migration" THEN NEXT {1}.

               IF NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                          OrderTimeStamp.Brand   = Syst.Var:gcBrand   AND
                          OrderTimeStamp.OrderID = Order.OrderID AND
                          OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY}) THEN DO:
                  IF (LOOKUP(Order.OrderChannel,
                      "pos,cc,pre-act,vip,fusion_pos,fusion_cc," +
                      "pos_pro,fusion_pos_pro") > 0 AND
                      Order.ICC > "") OR Order.OrderType = 3 
                  THEN SIM.SimStat = 4.
               END.

               IF AVAIL SIM THEN
                  SIM.MsSeq = Order.MsSeq.
               
             END. /* IF NOT llReserveSimAndMsisdn THEN DO: */
             
             IF Order.ResignationPeriod AND 
                llReserveSimAndMsisdn EQ TRUE THEN DO:
                fSetOrderStatus(Order.OrderId, {&ORDER_STATUS_RESIGNATION}).
                NEXT {1}.
             END.

             ldeSwitchTS = 0.
             IF Order.MNPStatus > 0 THEN DO:
                
                FOR EACH MNPProcess NO-LOCK WHERE
                      MNPProcess.OrderId = Order.OrderID AND
                      MNPProcess.StatusCode < 6 AND
                      MNPProcess.StatusCode NE 4 AND
                      MNPProcess.StatusCode NE 0,
                  FIRST MNPSub NO-LOCK WHERE
                        MNPSub.MNPSeq = MNPProcess.MNPSeq:
                         /* 6 ported, 7 canceled, 4 rejected, 0 new */      

                   ldeSwitchTS = MNPSub.PortingTime.      
                END.
             END.
             ELSE ldeSwitchTS = Func.Common:mMakeTS().

             
             IF Order.OrderType = 3 THEN
                fReactivationRequest(INPUT Order.MsSeq,
                                     INPUT Order.OrderId,
                                     INPUT ldeSwitchTS,
                                     INPUT Syst.Var:katun,
                                     {&REQUEST_SOURCE_NEWTON},
                                     OUTPUT ocResult).
             ELSE
                fSubscriptionRequest(INPUT  Order.MSSeq,
                                  INPUT  Order.Cli,
                                  INPUT  Order.CustNum,
                                  INPUT  1,
                                  INPUT  Syst.Var:katun,
                                  INPUT  ldeSwitchTS,
                                  INPUT  "CREATE",
                                  INPUT  STRING(Order.OrderId),
                                  INPUT  "",
                                  INPUT  "", /*for old SIM*/
                                  INPUT  "", /*for Reason info*/
                                  INPUT  "", /*for ContractID*/
                                  INPUT  FALSE,
                                  INPUT  0,
                                  INPUT  {&REQUEST_SOURCE_NEWTON},
                                  OUTPUT ocResult).
             
             IF ocResult > "" THEN DO:
                llOrdStChg = fSetOrderStatus(Order.OrderId,"2"). /* error */

                Func.Common:mWriteMemo("Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "ACTIVATION FAILED",
                                 ocResult).
             END.
            
             /* ongoing */
             ELSE llOrdStChg = fSetOrderStatus(Order.OrderId,"12").

             /* Mark timestamp as change */
             if llOrdStChg then fMarkOrderStamp(Order.OrderID,"Change",0.0).

 
