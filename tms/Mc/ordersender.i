/* ordersender.i   

   changes:      05.12.06/aam print order confirmation
                 12.12.06/aam check prechosen icc, write memos
                 15.12.06/aam all checks before prinoconf 
                 21.12.06/tk  if cli = "" set orderstatus to 2
                 28.12.06/tk  check prechosen icc only for non-mnp orders
*/

            &IF "{&localvar}" NE "YES" &THEN
                &GLOBAL-DEFINE localvar YES
                define variable llOrdStChg   as logical   no-undo.
                DEF VAR llReserveSimAndMsisdn AS LOG NO-UNDO. 
                DEFINE VARIABLE lh99Order AS HANDLE NO-UNDO.

            &ENDIF
                
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
                  
                  DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                   "Order",
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
                                    INPUT  katun,
                                    INPUT  OrderFusion.FixedInstallationTS,
                                    INPUT  "CREATE-FIXED",
                                    INPUT  STRING(Order.OrderId),
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

                  DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                   "Order",
                                   STRING(Order.OrderID),
                                   0,
                                   "FIXED LINE ACTIVATION FAILED",
                                   ocResult).
                  NEXT {1}.
               END.

               IF (Order.OrderType EQ {&ORDER_TYPE_NEW} OR
                   Order.OrderType EQ {&ORDER_TYPE_MNP}) AND
                   CAN-FIND(FIRST Memo NO-LOCK WHERE
                                  Memo.Brand = gcBrand AND
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
               
               /* NOTE: this check is also in orderinctrl.p */
               IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
                  Order.PortingDate <> ? THEN DO:
                  fSetOrderStatus(Order.OrderID,
                                  {&ORDER_STATUS_MNP_ON_HOLD}).
                  NEXT {1}.
               END.
            END.

            ASSIGN llOrdStChg = no.
            
            /* Move Mobile only tariff order to 76 queue, if customer 
               has ongoing convergent order */
            IF (Order.OrderType EQ {&ORDER_TYPE_NEW}  OR
                Order.OrderType EQ {&ORDER_TYPE_MNP}) AND 
               CAN-FIND(FIRST CLIType NO-LOCK WHERE 
                              CLIType.Brand      = gcBrand       AND 
                              CLIType.CLIType    = Order.CLIType AND 
                              CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY}) THEN DO: 
 
               FIND FIRST OrderCustomer NO-LOCK WHERE
                          OrderCustomer.Brand   = gcBrand       AND
                          OrderCustomer.OrderId = Order.OrderId AND
                          OrderCustomer.RowType = 1             NO-ERROR.
               
               IF fCheckOngoingConvergentOrder(OrderCustomer.CustIdType,
                                               OrderCustomer.CustId) THEN DO:
                  fSetOrderStatus(Order.OrderID,
                                  {&ORDER_STATUS_PENDING_MAIN_LINE}).
                  NEXT {1}.
               END.

            END.    
            
            /* YDR-1825 MNP SIM ONLY Orders
              Additional ordertimestamp is to prevent infinitive loop */
            IF lcSIMonlyMNP EQ "true" AND
               Order.OrderType = 1 AND
               Order.CrStamp >= 20150616.40200 AND
               Order.MNPStatus = 1 AND
               LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 AND
               NOT CAN-FIND(FIRST OrderAccessory NO-LOCK WHERE
                             OrderAccessory.Brand = gcBrand AND
                             OrderAccessory.OrderId = Order.OrderID) AND
               NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                             OrderTimeStamp.Brand   = gcBrand   AND
                             OrderTimeStamp.OrderID = Order.OrderID AND
                             OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY})
               THEN DO:
                  
                  FIND FIRST OrderCustomer WHERE
                             OrderCustomer.Brand = gcBrand AND
                             OrderCustomer.OrderId = Order.OrderId AND
                             OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
                  SEARCHSIM:
                  REPEAT:
                        
                     IF Order.ICC > "" THEN DO:
                        
                        FIND Sim WHERE Sim.ICC = Order.ICC 
                        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

                     END.
                     ELSE DO:
                        /* determine correct ICC stock */
                        lcStock = (IF Order.MnpStatus > 0 THEN "MNP" ELSE "NEW").

                        lcStock = fSearchStock(lcStock,OrderCustomer.ZipCode).

                        RELEASE SIM.

                        FOR EACH bSIM USE-index simstat WHERE
                                 bSIM.Brand   = gcBrand  AND
                                 bSIM.Stock   = lcStock  AND
                                 bSIM.simstat = 1        AND
                                 bSIM.SimArt  = "universal" NO-LOCK:

                           /* one week ICC quarantine time check, YOT-924 */
                           IF bSIM.MsSeq > 0 THEN DO:

                              FIND FIRST bOldOrder WHERE
                                         bOldOrder.MsSeq = bSIM.MsSeq AND
                                         bOldOrder.OrderType NE 2
                              NO-LOCK USE-INDEX MsSeq NO-ERROR.

                              IF AVAIL bOldOrder AND
                                fOffSet(bOldOrder.CrStamp, 24 * 7) > fMakeTS()
                                THEN NEXT.
                           END.

                           FIND SIM WHERE
                                ROWID(SIM) = ROWID(bSIM)
                           EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                           LEAVE.
                        END.
                     END.
         
                     IF LOCKED(SIM) THEN DO:
                        PAUSE 5.
                        NEXT.
                     END.

                     ELSE IF NOT LOCKED(sim) AND avail sim 
                     THEN LEAVE SEARCHSIM.
                     
                     /* no free sims available */
                     ELSE IF NOT AVAILABLE Sim THEN LEAVE SEARCHSIM.  
                  END.

                  /* free sim was not available */
                  IF NOT AVAILABLE Sim THEN NEXT {1}.
                  
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

                  IF Order.ICC = "" THEN
                     ASSIGN
                        Order.ICC = SIM.ICC
                        SIM.SimStat = 20
                        SIM.MsSeq = Order.MsSeq.
                  NEXT {1}.
            END.
               
            /* Renove handling */ 
            IF Order.OrderType = 2 THEN DO:
              
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

               /* find the SIM Type other than Renewal POS order if required */
               IF Order.ICC = "" AND
                  NOT Order.OrderChannel BEGINS "Renewal_POS" THEN DO:
                  
                  FIND FIRST OrderAction WHERE
                             OrderAction.Brand    = gcBrand AND
                             OrderAction.OrderId  = Order.OrderId AND
                             OrderAction.ItemType = "SIMType" NO-LOCK NO-ERROR.
                  IF AVAIL OrderAction AND OrderAction.ItemKey > "" THEN DO:
                     /* YBP-589 */ 
                     FIND FIRST OrderCustomer WHERE
                                OrderCustomer.Brand = gcBrand AND
                                OrderCustomer.OrderId = Order.OrderId AND
                                OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
                     
                     RELEASE SIM.

                     /* YBP-591 */ 
                     SEARCHSIM:
                     REPEAT:
                        /* YBP-589 */ 
                        /* determine correct ICC stock */
                        lcStock = "NEW".

                        lcStock = fSearchStock(lcStock,OrderCustomer.ZipCode).

                        FOR EACH bSIM USE-index simstat WHERE
                                 bSIM.Brand   = gcBrand  AND
                                 bSIM.Stock   = lcStock  AND
                                 bSIM.simstat = 1        AND
                                 bSIM.SimArt  = "universal" NO-LOCK:

                           IF bSIM.MsSeq > 0 THEN DO:
                              FIND FIRST bOldOrder WHERE
                                         bOldOrder.MsSeq = bSIM.MsSeq
                                   NO-LOCK USE-INDEX MsSeq NO-ERROR.
                              IF AVAIL bOldOrder AND
                                 fOffSet(bOldOrder.CrStamp, 24 * 7) > fMakeTS()
                              THEN NEXT.
                           END.

                           FIND SIM WHERE
                                ROWID(SIM) = ROWID(bSIM)
                                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                           LEAVE.
                        END.

                        IF LOCKED(SIM) THEN DO:
                           PAUSE 5.
                           NEXT.
                        END.

                        ELSE IF NOT LOCKED(SIM) AND AVAIL SIM
                        THEN LEAVE SEARCHSIM.

                        /* no free sims available */
                        ELSE IF NOT AVAILABLE SIM THEN LEAVE SEARCHSIM.
                     END.

                     /* free sim was not available */
                     IF NOT AVAILABLE SIM THEN NEXT {1}.

                     /* YBP-592 */ 
                     SIM.SimStat = 13. /* Reserved for ICC change */

                     IF Order.ICC = "" THEN Order.ICC = SIM.ICC.
                  END. /* IF AVAIL OrderAction THEN DO: */
               END. /* IF Order.ICC = "" AND */
               
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
                     katun,
                     fMakeTS(),
                     "7",
                     OUTPUT ocResult
                     ).
                   
                  IF ocResult > "" THEN DO:
                     DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                      "Order",
                                      STRING(Order.OrderID),
                                      0,
                                      "After Sales Request creation failed",
                                      ocResult).
                     llOrdStChg = fSetOrderStatus(Order.OrderId,"4").
                  END.
               END.
                 
               RELEASE Order.
               NEXT {1}.
            END. /* IF Order.OrderType = 2 THEN DO: */

            IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN DO:
 
               /* YBP-596 */ 
               RUN Mm/fusion_stc.p(Order.OrderID, OUTPUT liRequestID).
               
               IF liRequestID > 0 THEN
                  /* YBP-597 */ 
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"12").
               ELSE DO:
                  /* YBP-598 */ 
                  DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                   "Order",
                                   STRING(Order.OrderID),
                                   0,
                                   "STC request creation failed",
                                   RETURN-VALUE).
                  llOrdStChg = fSetOrderStatus(Order.OrderId,"4").
               END.

               RELEASE Order.
               NEXT {1}.
            END.
             
             /* YBP-594 */ 
             IF Order.OrderType <> 3 AND
                CAN-FIND(FIRST MsRequest WHERE
                               MsRequest.MsSeq   = Order.MSSeq  AND
                               MsRequest.ReqType = 13)
             THEN DO: 
                NEXT.
             END.

             IF LOCKED(Order) THEN NEXT.
             
             IF order.CLI = "" THEN DO:

                   /* YBP-599 */ 
                   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                    "Order",
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

                DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                 "Order",
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
                      ActionLog.Brand  = gcBrand AND
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
                   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                    "Order",
                                    STRING(Order.OrderID),
                                    0,
                                    "Order Process Error",
                                    "No valid ICC could be found").
                   NEXT {1}.                  
                END.
                
                IF LOOKUP(Order.OrderChannel,"fusion_pos,pos,vip") = 0 AND
                   llReserveSimAndMsisdn AND
                   Sim.SimStat NE 1 AND
                   Order.OrderType <> 3 AND
                   NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                             OrderTimeStamp.Brand   = gcBrand   AND
                             OrderTimeStamp.OrderID = Order.OrderID AND
                             OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY}) THEN DO:
                      /* YBP-605 */
                      llOrdStChg = fSetOrderStatus(Order.OrderId,"4"). /* in control */
                      /* YBP-605 */
                      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                       "Order",
                                       STRING(Order.OrderID),
                                       0,
                                       "ICC In Use",
                                       "ICC is already in use").
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
                DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                 "Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "CLI Type",
                                 "CLI type is missing").
                NEXT {1}.  
             END.

             FIND FIRST clitype WHERE
                        clitype.brand   = gcBrand and
                        clitype.clitype = order.clitype NO-LOCK NO-ERROR.
             
             IF NOT AVAIL clitype THEN DO:

                llOrdStChg = fSetOrderStatus(Order.OrderId,"2"). /* ERROR */
                /* YBP-610 */
                DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                 "Order",
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
                DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                 "Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "Subscription",
                                 "Subscription already exists with MSISDN" +
                                    " " + Order.CLI).
                NEXT {1}.
             END.
             
            /* YBP-612 */
            FIND FIRST OrderCustomer WHERE
                       OrderCustomer.Brand = gcBrand AND
                       OrderCustomer.OrderId = Order.OrderId AND
                       OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

 
             /* print order confirmation, this is done also for mnp orders
                but not for gift or preactivated or vip orders
             */  
             IF LOOKUP(Order.OrderChannel,"Yoigo,Pre-act,vip") = 0 AND
                OrderCustomer.Email NE "" AND
                Order.OrderType <> 3 AND Order.OrderType <> 4 AND
                (Order.MnpStatus = 0 OR Order.StatusCode = "3") THEN DO:  
                
                /* YBP-613 */
                RUN Mc/prinoconf.p (Order.OrderID).
                
                IF RETURN-VALUE BEGINS "ERROR" THEN DO:
                   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                    "Order",
                                    STRING(Order.OrderID),
                                    0,
                                    "Order Confirmation Failed",
                                    RETURN-VALUE).
                END.
             END.
  
             IF Order.StatusCode = "3" THEN DO:
                
                IF Order.SalesMan EQ "order_correction_mnp" AND
                   LOOKUP(Order.OrderChannel,
                          "telesales,fusion_telesales,pos,fusion_pos") > 0 THEN
                   /* YBP-620 */
                   Order.MNPStatus = 6. /* fake mnp process (ACON) */
                ELSE DO:
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
             SEARCHSIM:
             REPEAT:
                   
                IF Order.ICC > "" THEN DO:
                   
                   FIND Sim WHERE Sim.ICC = Order.ICC 
                   EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

                END.
                ELSE DO:
                   /* determine correct ICC stock */
                   lcStock = (IF Order.MnpStatus > 0 THEN "MNP" ELSE "NEW").

                   lcStock = fSearchStock(lcStock,OrderCustomer.ZipCode).

                   RELEASE SIM.

                   FOR EACH bSIM USE-index simstat WHERE
                            bSIM.Brand   = gcBrand  AND
                            bSIM.Stock   = lcStock  AND
                            bSIM.simstat = 1        AND
                            bSIM.SimArt  = "universal" NO-LOCK:

                      /* one week ICC quarantine time check, YOT-924 */
                      IF bSIM.MsSeq > 0 THEN DO:

                         FIND FIRST bOldOrder WHERE
                                    bOldOrder.MsSeq = bSIM.MsSeq AND
                                    bOldOrder.OrderType NE 2
                         NO-LOCK USE-INDEX MsSeq NO-ERROR.

                         IF AVAIL bOldOrder AND
                           fOffSet(bOldOrder.CrStamp, 24 * 7) > fMakeTS()
                           THEN NEXT.
                      END.

                      FIND SIM WHERE
                           ROWID(SIM) = ROWID(bSIM)
                      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                      LEAVE.

                   END.

                END.
   
                IF LOCKED(SIM) THEN DO:
                   PAUSE 5.
                   NEXT.
                END.

                ELSE IF NOT LOCKED(sim) AND avail sim 
                THEN LEAVE SEARCHSIM.
                
                /* no free sims available */
                ELSE IF NOT AVAILABLE Sim THEN LEAVE SEARCHSIM.  
             END.

             /* free sim was not available */
             IF NOT AVAILABLE Sim THEN NEXT {1}.
             
             FIND FIRST MSISDNNUMBER WHERE 
                        MSISDNNumber.CLI = Order.CLI NO-LOCK NO-ERROR.

             IF NOT AVAIL MSISDNNumber THEN DO:
                CREATE msisdnNumber.
                MSISDNNumber.CLI = Order.CLI.
             END.
              
             FIND FIRST MSISDN WHERE
                        MSISDN.Brand = gcBrand AND
                        MSISDN.CLI   = Order.Cli NO-LOCK NO-ERROR.
                        
             IF NOT AVAIL MSISDN THEN DO:
                CREATE MSISDN.
                ASSIGN
                   MSISDN.ActionDate = TODAY 
                   msisdn.ValidFrom  = fmakeTS()
                   Msisdn.cli        = order.cli 
                   Msisdn.StatusCode = 22
                   MSISDN.MSSeq      = Order.MSSeq
                   MSISDN.Brand      = gcBrand.
             END.          
      
             IF NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                        OrderTimeStamp.Brand   = gcBrand   AND
                        OrderTimeStamp.OrderID = Order.OrderID AND
                        OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY}) THEN DO:
                IF (LOOKUP(Order.OrderChannel,
                    "pos,cc,pre-act,vip,fusion_pos,fusion_cc") > 0 AND
                    Order.ICC > "") OR Order.OrderType = 3 
                THEN SIM.SimStat = 4.
                ELSE SIM.SimStat = 20.
             END.

             IF SIM.SimStat = 20 AND
                Order.MNPStatus = 6 AND
                Order.SalesMan EQ "order_correction_mnp" AND
                LOOKUP(Order.OrderChannel,
                       "telesales,fusion_telesales,pos,fusion_pos") > 0 THEN
                SIM.SimStat = 21.

             IF Order.ICC = "" THEN Order.ICC = Sim.ICC.
             
             SIM.MsSeq = Order.MsSeq.
                
             IF LOOKUP(Order.OrderChannel,"pos,cc,vip,fusion_pos,fusion_cc") = 0 AND
                Order.OrderType <> 3 THEN DO:
                CREATE SimDeliveryhist.
           
                ASSIGN
                   SimDeliveryHist.OrderID    = Order.OrderID
                   SimDeliveryHist.MSSeq      = Order.MSSeq 
                   SimDeliveryHist.StatusCode = 2.
                   SimDeliveryHist.TimeStamp = fMakeTS().
             END.
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
             ELSE ldeSwitchTS = fMakeTS().
             
             IF Order.OrderType = 3 THEN
                fReactivationRequest(INPUT Order.MsSeq,
                                     INPUT Order.OrderId,
                                     INPUT ldeSwitchTS,
                                     INPUT katun,
                                     {&REQUEST_SOURCE_NEWTON},
                                     OUTPUT ocResult).
             ELSE
                fSubscriptionRequest(INPUT  Order.MSSeq,
                                  INPUT  Order.Cli,
                                  INPUT  Order.CustNum,
                                  INPUT  1,
                                  INPUT  katun,
                                  INPUT  ldeSwitchTS,
                                  INPUT  "CREATE",
                                  INPUT  STRING(Order.OrderId),
                                  INPUT  "", /*for old SIM*/
                                  INPUT  "", /*for Reason info*/
                                  INPUT  "", /*for ContractID*/
                                  INPUT  FALSE,
                                  INPUT  0,
                                  INPUT  {&REQUEST_SOURCE_NEWTON},
                                  OUTPUT ocResult).
             
             IF ocResult > "" THEN DO:
                llOrdStChg = fSetOrderStatus(Order.OrderId,"2"). /* error */

                DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                 "Order",
                                 STRING(Order.OrderID),
                                 0,
                                 "ACTIVATION FAILED",
                                 ocResult).
             END.
            
             /* ongoing */
             ELSE llOrdStChg = fSetOrderStatus(Order.OrderId,"12").

             /* Mark timestamp as change */
             if llOrdStChg then fMarkOrderStamp(Order.OrderID,"Change",0.0).

 
