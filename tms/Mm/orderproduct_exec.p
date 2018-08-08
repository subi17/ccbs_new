
/*------------------------------------------------------------------------
    File        : orderproduct_exec.p
    Purpose     : Order Product Processing (ordersender.i/ordersender.p re-write via order products for product catalogue project)

    Syntax      :

    Description : 

    Author(s)   : Koundinya Maddali
    Created     : Thu Jul 05 19:32:40 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */



{Syst/tmsconst.i}
{Func/cparam2.i}
{Syst/commali.i}
{Syst/eventval.i}
{Func/fmakemsreq.i}
{Func/heartbeat.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
{Func/freacmobsub.i}
{Func/fixedlinefunc.i}
{Func/multitenantfunc.i}

DEFINE INPUT  PARAMETER iiOrderID AS INTEGER NO-UNDO.

&IF "{&localvar}" NE "YES" &THEN
    &GLOBAL-DEFINE localvar YES
    DEFINE VARIABLE llOrdStChg            AS LOG    NO-UNDO. 
    DEFINE VARIABLE llReserveSimAndMsisdn AS LOG    NO-UNDO.
    DEFINE VARIABLE llgMNPSimOnly         AS LOG    NO-UNDO INITIAL NO.  
    
    DEFINE VARIABLE lh99Order             AS HANDLE NO-UNDO.
    DEFINE VARIABLE lh76Order             AS HANDLE NO-UNDO.
    DEFINE VARIABLE lh17Order             AS HANDLE NO-UNDO.
    DEFINE VARIABLE lh15Order             AS HANDLE NO-UNDO.
&ENDIF

RELEASE SIM.

DEFINE VARIABLE lcCLI             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcICC             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedNumber     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeSwitchTS       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liMnpStatus       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ocResult          AS CHAR      NO-UNDO.
DEFINE VARIABLE lgHoldOrder       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcSubscriptionICC AS CHARACTER NO-UNDO.
DEFINE VARIABLE liRequestID AS INTEGER NO-UNDO. 

DEFINE BUFFER bfOrder              FOR Order.
DEFINE BUFFER bfOrderProductParent FOR OrderProduct.

/* ***************************  Functions  *************************** */

FUNCTION fValidateSIM RETURNS CHARACTER():
        
    IF lcICC > "" AND
       Func.ValidateOrder:misValidICC(lcICC) = FALSE THEN 
    DO:        
        RUN pMarkErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "Order Process Error",
                               "No valid ICC could be found").        
    END. /*IF lcICC > ""*/    
    RETURN "".
    
END FUNCTION.

FUNCTION fValidateMSISDN RETURNS CHARACTER():
    
    DEFINE VARIABLE ldeCLI AS DECIMAL NO-UNDO.
    
    IF lcCLI = ""  THEN 
    DO:        
        RUN pMarkErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,      
                               "MSISDN",
                               "MSISDN is missing").
                               
        IF lgHoldOrder THEN 
            RETURN.            
    END. /*IF lcCLI = ""*/
    
    ASSIGN 
        ldeCLI = DECIMAL(lcCLI) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        RUN pMarkErrorStatus.
        Func.Common:mWriteMemo("Order",
            STRING(bfOrder.OrderID),
            0,
            "MSISDN",
            "MSISDN is invalid").   
    END.    
    RETURN "".
    
END FUNCTION.   

FUNCTION fValidateSubscriptionType RETURNS CHARACTER ():
    
    IF Func.ValidateOrder:misCLITypeExists(lcCLIType) = FALSE THEN 
    DO:
        RUN pMarkErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "CLI Type",
                               "Invalid CLIType to the order").
        
    END.
    
    IF fIsConvergenceTariff(lcCLIType) = NO  AND
         CAN-FIND(FIRST MobSub WHERE MobSub.CLI = lcCLI) THEN 
    DO:          

        RUN pMarkErrorStatus. 
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "Subscription",
                               "Subscription already exists with MSISDN" + " " + lcCLI).
        
    END.
    
    RETURN "".
    
END FUNCTION. 

/* ***************************  Main Block  *************************** */

DO ON ERROR UNDO, RETURN ERROR:
    
    FIND FIRST bfOrder WHERE bfOrder.OrderID = iiOrderID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    IF NOT AVAILABLE bfOrder THEN
        RETURN.
    
    RUN pInitialise.

    RUN pProcess.

    IF lgHoldOrder = TRUE THEN 
        RETURN.
        
    RUN pCheckOrderStatus.

    IF lgHoldOrder = TRUE THEN 
        RETURN.

    RUN pProcessOtherOrders.

    IF lgHoldOrder = TRUE THEN 
        RETURN.

    RUN pOrderValidations.
    
    IF lgHoldOrder = TRUE THEN 
        RETURN.

    RUN pUpdateOrder.
        
    RUN pCreateMobileSubscriptionRequest.    
    
END.

PROCEDURE pInitialise :
    
    /*Assigning the main variables*/    
    ASSIGN 
        liMnpStatus   = INTEGER(Func.OrderProductsData:mGetMobileNumberType(INPUT iiOrderID) = {&TYPE_MNP})     
        llgMNPSimOnly = bfOrder.OrderType EQ {&ORDER_TYPE_MNP}   AND
                    bfOrder.CrStamp   >= 20150616.40200      AND
                    liMnpStatus       EQ 1                   AND
                    LOOKUP(bfOrder.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 AND
                    NOT CAN-FIND(FIRST OrderAccessory NO-LOCK WHERE
                                       OrderAccessory.Brand   EQ Syst.Var:gcBrand AND
                                       OrderAccessory.OrderId EQ bfOrder.OrderID)   AND
                    NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                                      OrderTimeStamp.Brand   EQ Syst.Var:gcBrand   AND
                                      OrderTimeStamp.OrderID EQ bfOrder.OrderID      AND
                                      OrderTimeStamp.RowType EQ {&ORDERTIMESTAMP_SIMONLY}).
                                      
    IF bfOrder.ResignationPeriod THEN 
    DO:
        llReserveSimAndMsisdn =  NOT CAN-FIND(FIRST ActionLog NO-LOCK WHERE
            ActionLog.Brand  = Syst.Var:gcBrand AND
            ActionLog.TableName = "Order" AND
            ActionLog.KeyValue = STRING(bfOrder.OrderID) AND
            ActionLog.ActionId = "RESIGNATION" AND
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}).
    END.
    ELSE ASSIGN llReserveSimAndMsisdn = TRUE.
    
END PROCEDURE.

PROCEDURE pProcess:
    
    FOR EACH  OrderProduct NO-LOCK 
        WHERE OrderProduct.OrderID  = iiOrderID 
        AND OrderProduct.ParentID > 0 
        BREAK BY OrderProduct.ParentID DESCENDING:     
        
        RUN pOrderProduct(INPUT OrderProduct.ActionType , INPUT OrderProduct.ParentID).            
        IF lgHoldOrder = TRUE THEN 
            RETURN.     
    
        IF LAST-OF(OrderProduct.ParentID) THEN 
        DO:        
            FIND FIRST bfOrderProductParent WHERE 
                bfOrderProductParent.OrderID        = OrderProduct.OrderID AND
                bfOrderProductParent.OrderProductID = OrderProduct.ParentID
                NO-LOCK NO-ERROR.
                               
            IF AVAILABLE bfOrderProductParent THEN 
            DO:
                RUN pOrderProduct(INPUT bfOrderProductParent.ActionType,INPUT bfOrderProductParent.OrderProductID).
                IF lgHoldOrder = TRUE THEN 
                    RETURN.                         
            END.
        END. /* IF LAST-OF(OrderProduct.ParentID): */
    END. /* For each OrderProduct*/

END PROCEDURE.    

PROCEDURE pOrderProduct:
    
    DEFINE INPUT  PARAMETER icProductType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iiProductID   AS INTEGER NO-UNDO.
    
    CASE icProductType :
        
        WHEN {&ORDER_PRODUCT_SIM} THEN 
        DO:            
            RUN pAccessory(INPUT icProductType , INPUT iiProductID).            
        END.
        WHEN {&ORDER_PRODUCT_SUBSCRIPTION} THEN 
        DO:            
            RUN pSubscription(icProductType , INPUT iiProductID).            
        END.
        WHEN {&ORDER_PRODUCT_FIXED_LINE} THEN 
        DO:            
            RUN pSubscription(icProductType , INPUT iiProductID).            
        END.
        
    END CASE.
    
END PROCEDURE.  

PROCEDURE pAccessory :
    
    DEFINE INPUT  PARAMETER icProductType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iiProductID   AS INTEGER NO-UNDO.
    
   CASE icProductType :
       
       WHEN {&ORDER_PRODUCT_SIM} THEN 
       DO:           
           RUN pSIM(INPUT OrderProduct.ParentID).           
       END.      
       
   END CASE.
    
END PROCEDURE.    

PROCEDURE pSubscription :
    
    DEFINE INPUT  PARAMETER icProductType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iiProductID   AS INTEGER   NO-UNDO.
    
    CASE icProductType :
        
        WHEN {&ORDER_PRODUCT_SUBSCRIPTION} THEN 
        DO:
            RUN pMobile(INPUT iiProductID).
        END.    
        WHEN {&ORDER_PRODUCT_FIXED_LINE} THEN 
        DO:
            RUN pFixedLine(INPUT iiProductID).
        END.    
        
    END CASE.       
    
END PROCEDURE.    
  

PROCEDURE pCheckOrderStatus :
    
    IF LOOKUP(bfOrder.Statuscode,{&ORDER_INACTIVE_STATUSES} + ",4,74") > 0  THEN 
        ASSIGN lgHoldOrder = TRUE.
    
END PROCEDURE.    

PROCEDURE pSIM:
    
    DEFINE INPUT  PARAMETER iiSIMParentID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bfParentProduct FOR OrderProduct.

    ASSIGN 
        lcICC               =   Func.OrderProductsData:mGetOrderICC(INPUT iiOrderID)
        lcSubscriptionICC   =   Func.OrderProductsData:mGetOrderMobileICC(INPUT iiOrderID).
         
    IF lcICC EQ "" AND lcSubscriptionICC = "" THEN 
    DO:
        
        IF LOOKUP(bfOrder.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 THEN 
        DO:            
            IF bfOrder.OrderType EQ {&ORDER_TYPE_MNP} OR 
               bfOrder.OrderType EQ {&ORDER_TYPE_NEW} THEN 
            DO:
                ASSIGN 
                    lgHoldOrder = TRUE.
                RETURN.    
            END.               
        END.
        
        /* New change in SIM reservation logic, when order is placed from telesalses
           or web then order will be moved to order queue 15 0r 16. When order sent to LO,
           LO will pick and reserve the SIM through external API (dextra_update_order_status) */  
        
        IF LOOKUP(bfOrder.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 THEN 
        DO:
            
            IF (bfOrder.OrderType  EQ {&ORDER_TYPE_NEW} OR
               (bfOrder.OrderType  EQ {&ORDER_TYPE_MNP} AND 
               (bfOrder.StatusCode EQ {&ORDER_STATUS_ONGOING} OR llgMNPSimOnly)))THEN 
            DO:
                 
                IF llDoEvent THEN 
                DO:
                    ASSIGN lh15Order = BUFFER bfOrder:HANDLE.
                    RUN StarEventInitialize(lh15Order).
                    RUN StarEventSetOldBuffer(lh15Order).
                END.
                
                IF CAN-FIND(FIRST OrderGroup NO-LOCK WHERE
                    OrderGroup.OrderId        EQ bfOrder.OrderId          AND
                    OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
                    ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN 
                        fSetOrderStatus(bfOrder.OrderID,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).
                ELSE 
                    fSetOrderStatus(bfOrder.OrderID,{&ORDER_STATUS_SENDING_TO_LO}).
                
                IF llDoEvent THEN 
                DO:
                    RUN StarEventMakeModifyEvent(lh15Order).
                    fCleanEventObjects().
                END. 
                ASSIGN lgHoldOrder = TRUE.
                RETURN.
                
            END.
        END.
    END.
    
    IF llgMNPSimOnly THEN 
    DO:                    
        /* Event logging of 99 status setups */ 
        IF llDoEvent THEN 
        DO:
            ASSIGN lh99Order = BUFFER bfOrder:HANDLE.
            RUN StarEventInitialize(lh99Order).
            RUN StarEventSetOldBuffer(lh99Order).
        END.
        
        ASSIGN llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_SIM_ONLY_MNP_IN}).
        IF llDoEvent THEN 
        DO:
            RUN StarEventMakeModifyEvent(lh99Order).
            fCleanEventObjects().
        END.

        /* Set additional OrderStamp to avoid infinitive loop */
        fMarkOrderStamp(bfOrder.OrderID,"SimOnly",0.0).

        ASSIGN lgHoldOrder  = TRUE.
        RETURN.
        
    END. /*MNP SIM ONLY Orders from direct channel*/
    
    FIND FIRST bfParentProduct WHERE bfParentProduct.OrderProductID  =  iiSIMParentID NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfParentProduct THEN 
    DO:
       
        RUN pMarkErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "OrderMobile",
                               "Parent product to sim is not available.").
    END.
    ELSE RUN pCreateOrderMobile(INPUT bfParentProduct.OrderProductID).
            
END PROCEDURE.

PROCEDURE pMobile:
    
    DEFINE INPUT  PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    
    ASSIGN 
        lcCLIType = Func.OrderProductsData:mGetOrderCLIType(INPUT iiOrderID)
        lcCLI     = Func.OrderProductsData:mGetOrderCLI(INPUT iiOrderID).
        
    RUN pCreateOrderMobile(INPUT iiOrderProductID).
    
    IF Func.OrderProductsData:mHasInitialTopup(INPUT iiOrderID) THEN 
        RUN pCreateOrderTopup(iiOrderProductID , INPUT {&INITIAL_TOPUP}).
        
END PROCEDURE.

PROCEDURE pFixedLine:
    
    DEFINE INPUT  PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    
    ASSIGN 
        lcCLIType     = Func.OrderProductsData:mGetOrderCLIType(INPUT iiOrderID)
        lcCLI         = Func.OrderProductsData:mGetOrderCLI(INPUT iiOrderID)
        lcFixedNumber = Func.OrderProductsData:mGetFixNumber(INPUT iiOrderID).
    
    RUN pCreateOrderFusion(INPUT iiOrderProductID).
END.    

PROCEDURE pOrderValidations:
    
    fValidateSIM().
    
    IF lgHoldOrder = TRUE THEN 
        RETURN.
        
    fValidateMSISDN().
    
    IF lgHoldOrder = TRUE THEN 
        RETURN.
        
    fValidateSubscriptionType().

END PROCEDURE.   

PROCEDURE pProcessOtherOrders:
    
    CASE bfOrder.OrderType :        
        
        WHEN {&ORDER_TYPE_RENEWAL} THEN 
        DO:
            RUN pSetRenewalOrderStatus.
        END.
        WHEN {&ORDER_TYPE_STC} THEN 
        DO:
            RUN pSetSTCOrderStatus.
        END.
        WHEN {&ORDER_TYPE_ACC} THEN 
        DO:
             RUN pSetACCOrderStatus.
        END.
        WHEN {&ORDER_TYPE_ROLLBACK} THEN 
        DO:
             RUN pCheckRollbackOrders.
        END.

    END CASE .
     
END PROCEDURE. 

PROCEDURE pMarkErrorStatus :
    
    ASSIGN 
        llOrdStChg  = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ERROR})
        lgHoldOrder = TRUE.
    
END PROCEDURE.    

PROCEDURE pUpdateOrder:
    
    IF bfOrder.MSSeq = 0  THEN 
        ASSIGN bfOrder.MSSeq = NEXT-VALUE(Mobsub).
        
    FOR EACH OrderCustomer OF bfOrder EXCLUSIVE-LOCK:
        IF OrderCustomer.Language = "0" OR
           OrderCustomer.Language = "" THEN 
            ASSIGN OrderCustomer.Language = "1".
    END.
    
    IF llReserveSimAndMsisdn = TRUE THEN 
        RUN pSIMReservationUpdate.
    
END PROCEDURE.

PROCEDURE pSIMReservationUpdate :
    
    RUN pReserveSIM.    
    IF lgHoldOrder = TRUE THEN RETURN. 
       
    RUN pReserveMSISDN.     
    IF bfOrder.Orderchannel BEGINS "migration" THEN 
    DO:
        ASSIGN lgHoldOrder  = TRUE.
        RETURN.        
    END.    
    RUN pDelayedOrder.    
    
END PROCEDURE.

PROCEDURE pReserveSIM:
    
    IF lcSubscriptionICC > "" THEN 
    DO:
                      
        FIND FIRST Sim WHERE Sim.ICC = lcSubscriptionICC EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

        IF NOT AVAILABLE Sim THEN 
        DO: 
            ASSIGN 
                lgHoldOrder = TRUE.
            RETURN.
        END.
                 
    END.
    ELSE RELEASE SIM.
    
    IF NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                         OrderTimeStamp.Brand   = Syst.Var:gcBrand   AND
                         OrderTimeStamp.OrderID = bfOrder.OrderID AND
                         OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY}) THEN 
    DO:
        IF (LOOKUP(bfOrder.OrderChannel,"pos,cc,pre-act,vip,fusion_pos,fusion_cc,pos_pro,fusion_pos_pro") > 0 AND
            lcSubscriptionICC > "")   OR 
            bfOrder.OrderType = {&ORDER_TYPE_ROLLBACK} THEN 
                ASSIGN SIM.SimStat = 4.
    END.

    IF AVAIL SIM THEN
        ASSIGN SIM.MsSeq = bfOrder.MsSeq.    
    
END PROCEDURE.    

PROCEDURE pReserveMSISDN:
    
    FIND FIRST MSISDNNUMBER WHERE 
        MSISDNNumber.CLI = lcCLI NO-LOCK NO-ERROR.

    IF NOT AVAIL MSISDNNumber THEN 
    DO:
        CREATE msisdnNumber.
        MSISDNNumber.CLI = bfOrder.CLI.
    END.
            
    FIND FIRST MSISDN WHERE
               MSISDN.Brand = Syst.Var:gcBrand AND
               MSISDN.CLI   = bfOrder.Cli NO-LOCK NO-ERROR.
                      
    IF NOT AVAIL MSISDN THEN 
    DO:
        CREATE MSISDN.
        ASSIGN
            MSISDN.ActionDate = TODAY 
            msisdn.ValidFrom  = Func.Common:mMakeTS()
            Msisdn.cli        = bfOrder.cli 
            Msisdn.StatusCode = 22
            MSISDN.MSSeq      = bfOrder.MSSeq
            MSISDN.Brand      = Syst.Var:gcBrand.
    END.         
    
END PROCEDURE.

PROCEDURE pDelayedOrder:
    
    IF bfOrder.ResignationPeriod THEN 
    DO:
        fSetOrderStatus(bfOrder.OrderId, {&ORDER_STATUS_RESIGNATION}).
        ASSIGN 
            lgHoldOrder = TRUE.
        RETURN.
    END.
    
END PROCEDURE.    

PROCEDURE pGetTimeStamp :
    
    IF liMnpStatus > 0 THEN 
        ASSIGN ldeSwitchTS = fGetMNPTS().
    ELSE 
        ASSIGN ldeSwitchTS = Func.Common:mMakeTS().
    
END PROCEDURE.   

FUNCTION fGetMNPTS RETURNS DECIMAL():
    
    DEFINE VARIABLE ldeMNPSwitchTS AS DECIMAL NO-UNDO.
    
    FOR EACH MNPProcess NO-LOCK WHERE
             MNPProcess.OrderId = bfOrder.OrderID AND
             MNPProcess.StatusCode < 6 AND
             MNPProcess.StatusCode NE 4 AND
             MNPProcess.StatusCode NE 0,
             FIRST MNPSub NO-LOCK WHERE
             MNPSub.MNPSeq = MNPProcess.MNPSeq:
            /* 6 ported, 7 canceled, 4 rejected, 0 new */
        ASSIGN 
            ldeMNPSwitchTS = MNPSub.PortingTime.      
    END.
        
    RETURN ldeMNPSwitchTS.   
    
END PROCEDURE.    

PROCEDURE pCreateOrderMobile:
    
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bfOrderMobile FOR OrderMobile.
    
    IF CAN-FIND(FIRST bfOrderMobile WHERE bfOrderMobile.OrderID         =  iiOrderID  
                                      AND bfOrderMobile.OrderProductID  =  iiOrderProductID ) THEN 
    DO:
        FIND FIRST bfOrderMobile WHERE bfOrderMobile.OrderID            = iiOrderID 
                                   AND bfOrderMobile.OrderProductID     = iiOrderProductID
                                   EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                                         
        IF LOCKED(bfOrderMobile) OR 
            NOT AVAILABLE (bfOrderMobile) THEN 
        DO:
            Func.Common:mWriteMemo("Order",
                                    STRING(bfOrder.OrderID),
                                    0,
                                    "OrderMobile",
                                    "OrderMobile Record is locked.").
                                    
             ASSIGN lgHoldOrder = TRUE.
             RETURN.
         END. 
    END.
    ELSE DO:
        CREATE bfOrderMobile.
    END.
    
    ASSIGN 
        bfOrderMobile.Brand          = Syst.Var:gcBrand                                              WHEN bfOrderMobile.Brand          =   ""
        bfOrderMobile.OrderID        = iiOrderID                                                     WHEN bfOrderMobile.OrderID        =   0
        bfOrderMobile.OrderProductID = iiOrderProductID                                              WHEN bfOrderMobile.OrderProductID =   0
        bfOrderMobile.MSSeq          = bfOrder.MSSeq                                                 WHEN bfOrderMobile.MSSeq          =   0
        bfOrderMobile.Product        = lcCLIType                                                     WHEN bfOrderMobile.Product        =   ""
        bfOrderMobile.CLI            = lcCLI                                                         WHEN bfOrderMobile.CLI            =   ""
        bfOrderMobile.NumberType     = Func.OrderProductsData:mGetMobileNumberType(INPUT iiOrderID)  WHEN bfOrderMobile.NumberType     =   ""
        bfOrderMobile.CurrOper       = Func.OrderProductsData:mGetCurrentOperator(INPUT iiOrderID)   WHEN bfOrderMobile.CurrOper       =   ""
        bfOrderMobile.StatusCode     = bfOrder.StatusCode                                            WHEN bfOrderMobile.StatusCode     <>  ""
        bfOrderMobile.CreatedTS      = bfOrder.crstamp    
        bfOrderMobile.UpdatedTS      = Func.Common:mMakeTS()
        bfOrderMobile.ICC            = lcICC                                                         WHEN bfOrderMobile.ICC            =   ""
        bfOrderMobile.PayType        = LOGICAL(Func.ValidateOrder:mGetPayType(bfOrderMobile.Product))          
       .
           
END PROCEDURE.    

PROCEDURE pCreateMobileSubscriptionRequest :
    
    RUN pGetTimeStamp.
    
    IF bfOrder.OrderType = {&ORDER_TYPE_ROLLBACK} THEN 
        fReactivationRequest(INPUT bfOrder.MsSeq,
                             INPUT bfOrder.OrderId,
                             INPUT ldeSwitchTS,
                             INPUT Syst.Var:katun,
                             {&REQUEST_SOURCE_NEWTON},
                             OUTPUT ocResult).
    ELSE 
        fSubscriptionRequest(INPUT  bfOrder.MSSeq,
                             INPUT  lcCLI,
                             INPUT  bfOrder.CustNum,
                             INPUT  1,
                             INPUT  Syst.Var:katun,
                             INPUT  ldeSwitchTS,
                             INPUT  "CREATE",
                             INPUT  STRING(bfOrder.OrderId),
                             INPUT  "", /*for old SIM*/
                             INPUT  "", /*for Reason info*/
                             INPUT  "", /*for ContractID*/
                             INPUT  FALSE,
                             INPUT  0,
                             INPUT  {&REQUEST_SOURCE_NEWTON},
                             OUTPUT ocResult).
    IF ocResult > "" THEN 
    DO:
        llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ERROR}). 

        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "ACTIVATION FAILED",
                               ocResult).
                               
    END.            
    ELSE llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ONGOING}).
    /* Mark timestamp as change */
    IF llOrdStChg THEN fMarkOrderStamp(bfOrder.OrderID,"Change",0.0).                       
    
END PROCEDURE.    

PROCEDURE pCreateOrderTopup :
    
    DEFINE INPUT  PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iiTopUpType      AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bfOrderTopup FOR OrderTopup.
    
     FIND FIRST bfOrderTopup WHERE bfOrderTopup.OrderID           =  iiOrderID 
                               AND bfOrderTopUp.OrderProductID    =  iiOrderProductID
                               AND bfOrderTopUp.MSSeq             =  bfOrder.MsSeq
                               AND bfOrderTopUp.TopupType         =  iiTopUpType
                               NO-LOCK NO-ERROR.
     
     IF NOT AVAILABLE bfOrderTopup THEN 
     DO:
         CREATE bfOrderTopup.
     END.
     ELSE DO:
         
         FIND CURRENT bfOrderTopup EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         
         IF LOCKED(bfOrderTopup) OR NOT AVAILABLE bfOrderTopup THEN 
         DO:             
            ASSIGN 
                lgHoldOrder = TRUE.
                 
            Func.Common:mWriteMemo("Order",
                                   STRING(bfOrder.OrderID),
                                   0,
                                   "Initial Topup",
                                   "Order Topup updation failed.").
        END. 
    END.     
     
    ASSIGN 
        bfOrderTopUp.OrderID        = iiOrderID         WHEN  bfOrderTopUp.OrderID        = 0
        bfOrderTopUp.OrderProductID = iiOrderProductID  WHEN  bfOrderTopUp.OrderProductID = 0   
        bfOrderTopup.MsSeq          = bfOrder.MsSeq     WHEN  bfOrderTopup.MsSeq          = 0
        bfOrderTopup.TopupType      = iiTopUpType       WHEN  bfOrderTopup.TopupType      = 0   
        bfOrderTopup.Amount         = Func.OrderProductsData:mGetTopupAmount(INPUT iiOrderID ,
                                                                             INPUT iiOrderProductID,
                                                                             INPUT {&ORDER_PRODUCT_PARAM_INITIAL_TOPUP}) 
                                      WHEN bfOrderTopup.Amount = 0
        bfOrderTopup.DisplayAmount  = Func.OrderProductsData:mGetTopupAmount(INPUT iiOrderID ,
                                                                             INPUT iiOrderProductID,
                                                                             INPUT {&ORDER_PRODUCT_PARAM_INITIAL_TOPUP_DISPLAY}) 
                                      WHEN bfOrderTopup.Amount = 0   
        bfOrderTopup.Brand          = Syst.Var:gcBrand                                         
        .                                                              
    
END PROCEDURE.


PROCEDURE pSetRenewalOrderStatus :
               
    IF bfOrder.OrderChannel BEGINS "Renewal_POS" AND
        CAN-FIND(FIRST MsRequest WHERE
        MsRequest.MsSeq   = bfOrder.MSSeq                AND
        MsRequest.ReqType = {&REQTYPE_AFTER_SALES_ORDER} AND
        MsRequest.ReqIParam1 = bfOrder.OrderID) THEN 
    DO:
        ASSIGN 
            llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ONGOING}).
            lgHoldOrder = TRUE.
        RETURN.
    END.
               
    IF LOOKUP(bfOrder.OrderChannel,{&ORDER_CHANNEL_DIRECT_RENEWAL}) > 0 THEN
        ASSIGN 
            llOrdStChg  = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_WAITING_SENDING_LO}).  
    ELSE 
        ASSIGN 
            llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ONGOING}). 

    IF bfOrder.OrderChannel BEGINS "Renewal_POS" THEN 
    DO:
        fAfterSalesRequest(bfOrder.MsSeq,
                           bfOrder.OrderId,
                           Syst.Var:katun,
                           Func.Common:mMakeTS(),
                           "7",
                           OUTPUT ocResult).
                   
        IF ocResult > "" THEN 
        DO:
            Func.Common:mWriteMemo("Order",
                                   STRING(bfOrder.OrderID),
                                   0,
                                   "After Sales Request creation failed",
                                   ocResult).
            ASSIGN llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_IN_CONTROL}).
        END.
    END.
                 
    ASSIGN 
        lgHoldOrder = TRUE.
        
    RELEASE bfOrder NO-ERROR.

END PROCEDURE.

PROCEDURE pSetSTCOrderStatus :
    
    RUN Mm/fusion_stc.p(bfOrder.OrderID, OUTPUT liRequestID).
               
    IF liRequestID > 0 THEN
        ASSIGN 
            llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ONGOING}).            
    ELSE 
    DO:
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "STC request creation failed",
                               RETURN-VALUE).
        ASSIGN 
            llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_IN_CONTROL}).
    END.

    RELEASE bfOrder NO-ERROR.
    ASSIGN 
        lgHoldOrder = TRUE.
    
END PROCEDURE.

PROCEDURE pSetACCOrderStatus :
    
    RUN Mm/acc_order.p(bfOrder.OrderID, OUTPUT liRequestID).
               
    IF liRequestID > 0 THEN
        ASSIGN 
            llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ONGOING}).
    ELSE 
    DO:            
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
            "ACC request creation failed",
            RETURN-VALUE).
        ASSIGN 
            llOrdStChg = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_IN_CONTROL}).
    END.

    RELEASE bfOrder NO-ERROR.
    
    ASSIGN 
        lgHoldOrder = TRUE.

END PROCEDURE.    

PROCEDURE pCheckRollbackOrders :
    
    IF CAN-FIND(FIRST MsRequest WHERE
        MsRequest.MsSeq     EQ bfOrder.MSSeq                  AND
        MsRequest.ReqType   EQ {&REQTYPE_SUBSCRIPTION_CREATE} AND
        MsRequest.ReqStatus NE {&REQUEST_STATUS_CANCELLED}) THEN 
    DO: 
        ASSIGN 
            lgHoldOrder = TRUE.
    END.
    
END PROCEDURE.    
