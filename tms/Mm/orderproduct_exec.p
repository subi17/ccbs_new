
/*------------------------------------------------------------------------
    File        : orderproduct_exec.p
    Purpose     : Order Product Processing (ordersender.i/ordersender.p re-write via order products)

    Syntax      :

    Description : 

    Author(s)   : Koundinya Maddali
    Created     : Thu Jul 05 19:32:40 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
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

DEFINE BUFFER bfOrder FOR Order.

FIND bfOrder WHERE bfOrder.OrderID = iiOrderID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

IF NOT AVAILABLE bfOrder THEN RETURN.

DEFINE BUFFER bfOrderProductParent FOR OrderProduct.

/*Assigning the main variables*/    
ASSIGN 
    liMnpStatus  =  INTEGER(Func.OrderProductsData:mGetMobileNumberType(INPUT iiOrderID) = {&TYPE_MNP})     
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
                                      OrderTimeStamp.RowType EQ {&ORDERTIMESTAMP_SIMONLY})
                                         .

MAIN-BLOCK:
FOR EACH OrderProduct NO-LOCK 
    WHERE OrderProduct.OrderID  = iiOrderID 
      AND OrderProduct.ParentID > 0 
      BREAK BY OrderProduct.ParentID DESCENDING: 
    
    IF OrderProduct.ActionType  = {&ORDER_PRODUCT_SIM}
    THEN DO:      
   
        RUN ipProcessSIM(INPUT OrderProduct.ParentID).
        IF lgHoldOrder = TRUE THEN RETURN.
       
    END. 
    
    IF LAST-OF(OrderProduct.ParentID) 
    THEN DO:
        
        FIND FIRST bfOrderProductParent WHERE bfOrderProductParent.OrderID         =   OrderProduct.OrderID
                                          AND bfOrderProductParent.OrderProductID  =   OrderProduct.ParentID
                                       NO-LOCK NO-ERROR.
                               
        IF AVAILABLE bfOrderProductParent 
        THEN DO:
            
            IF bfOrderProductParent.ActionType =  {&ORDER_PRODUCT_SUBSCRIPTION}
            THEN DO:
                
                RUN ipProcessMobileSubscription(INPUT bfOrderProductParent.OrderProductID). 
                IF lgHoldOrder = TRUE THEN RETURN.
                
            END.
            ELSE IF bfOrderProductParent.ActionType = {&ORDER_PRODUCT_FIXED_LINE}
            THEN DO:
                
                RUN ipProcessFixedLine(INPUT bfOrderProductParent.OrderProductID).
                IF lgHoldOrder = TRUE THEN RETURN.
                
            END.            
        END.
    END. /* IF LAST-OF(OrderProduct.ParentID): */
END. /* For each OrderProduct*/

IF LOOKUP(bfOrder.Statuscode,{&ORDER_INACTIVE_STATUSES} + ",4,74") > 0  
THEN RETURN.

RUN ipOrderValidations.
    
IF lgHoldOrder = TRUE 
THEN RETURN.

RUN ipUpdateOrderData.
        
RUN ipCreateSubscriptionRequest.

PROCEDURE ipProcessSIM:
    
    DEFINE INPUT  PARAMETER iiSIMParentID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bfParentProduct FOR OrderProduct.

    ASSIGN 
        lcICC               =   Func.OrderProductsData:mGetOrderICC(INPUT iiOrderID)
        lcSubscriptionICC   =   Func.OrderProductsData:mGetOrderSubscriptionICC(INPUT iiOrderID).
         
    IF lcICC EQ "" AND lcSubscriptionICC = ""
    THEN DO:
        
        IF LOOKUP(bfOrder.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0
        THEN DO:
            
            IF bfOrder.OrderType EQ {&ORDER_TYPE_MNP} OR 
               bfOrder.OrderType EQ {&ORDER_TYPE_NEW}  
            THEN lgHoldOrder = TRUE.
            
        END.
        
        /* New change in SIM reservation logic, when order is placed from telesalses
           or web then order will be moved to order queue 15 0r 16. When order sent to LO,
           LO will pick and reserve the SIM through external API (dextra_update_order_status) */  
        
        IF LOOKUP(bfOrder.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0
        THEN DO:
            
            IF (bfOrder.OrderType  EQ {&ORDER_TYPE_NEW} OR
               (bfOrder.OrderType  EQ {&ORDER_TYPE_MNP} AND 
               (bfOrder.StatusCode EQ {&ORDER_STATUS_ONGOING} OR llgMNPSimOnly)))
            THEN DO:
                 
                IF llDoEvent  
                THEN DO:
                    lh15Order = BUFFER bfOrder:HANDLE.
                    RUN StarEventInitialize(lh15Order).
                    RUN StarEventSetOldBuffer(lh15Order).
                END.
                
                IF CAN-FIND(FIRST OrderGroup NO-LOCK WHERE
                                  OrderGroup.OrderId        EQ bfOrder.OrderId          AND
                                  OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
                          ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) 
                THEN fSetOrderStatus(bfOrder.OrderID,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).
                ELSE fSetOrderStatus(bfOrder.OrderID,{&ORDER_STATUS_SENDING_TO_LO}).
                
                IF llDoEvent  
                THEN DO:
                    RUN StarEventMakeModifyEvent(lh15Order).
                    fCleanEventObjects().
                END. 
                
                ASSIGN 
                    lgHoldOrder = TRUE.
                
            END.
        END.
    END.
    
    FIND FIRST bfParentProduct WHERE bfParentProduct.OrderProductID  =  iiSIMParentID NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfParentProduct THEN 
    DO:
       
        RUN ipChangeOrderToErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "OrderSubscription",
                               "Parent product to sim is not available.").
    END.
    ELSE RUN ipCreateOrderSubscriptionData(INPUT bfParentProduct.OrderProductID).
            
END PROCEDURE.

PROCEDURE ipProcessMobileSubscription:
    
    DEFINE INPUT  PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    
    ASSIGN 
        lcCLIType = Func.OrderProductsData:mGetOrderCLIType(INPUT iiOrderID)
        lcCLI     = Func.OrderProductsData:mGetOrderCLI(INPUT iiOrderID).
        
    RUN ipCreateOrderSubscriptionData(INPUT iiOrderProductID).
    
    IF Func.OrderProductsData:mHasInitialTopup(INPUT iiOrderID) THEN 
        RUN ipCreateOrderTopup(iiOrderProductID , INPUT {&INITIAL_TOPUP}).
        
END PROCEDURE.

PROCEDURE ipProcessFixedLine:
    
    DEFINE INPUT  PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    
    ASSIGN 
        lcCLIType       =  Func.OrderProductsData:mGetOrderCLIType(INPUT iiOrderID)
        lcCLI           =  Func.OrderProductsData:mGetOrderCLI(INPUT iiOrderID)
        lcFixedNumber   =  Func.OrderProductsData:mGetFixNumber(INPUT iiOrderID).
    
    RUN ipCreateOrderSubscriptionData(INPUT iiOrderProductID).
END.    

PROCEDURE ipOrderValidations:
    
    DEFINE VARIABLE ldeCLI AS DECIMAL NO-UNDO.
    
    IF lcCLI = ""  
    THEN DO:
        
        RUN ipChangeOrderToErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "MSISDN",
                               "MSISDN is missing").
                               
    END. /*IF lcCLI = ""*/
    
    IF lcICC > ""
    AND Func.ValidateOrder:misValidICC(lcICC) = FALSE
    THEN DO:
        
        RUN ipChangeOrderToErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "Order Process Error",
                               "No valid ICC could be found").
        
    END. /*IF lcICC > ""*/
    
    ASSIGN ldeCLI   =  DECIMAL(lcCLI) NO-ERROR.
    IF ERROR-STATUS:ERROR 
    THEN DO:
        
        RUN ipChangeOrderToErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "MSISDN",
                               "MSISDN is invalid").   
        
    END.
    
    IF Func.ValidateOrder:misCLITypeExists(lcCLIType) = FALSE 
    THEN DO:

        RUN ipChangeOrderToErrorStatus.
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "CLI Type",
                               "Invalid CLIType to the order").
        
    END.
    
    IF fIsConvergenceTariff(lcCLIType) = NO  
    AND CAN-FIND(FIRST MobSub WHERE MobSub.CLI = lcCLI)  
    THEN DO:          

        RUN ipChangeOrderToErrorStatus. 
        Func.Common:mWriteMemo("Order",
                               STRING(bfOrder.OrderID),
                               0,
                               "Subscription",
                                "Subscription already exists with MSISDN" +
                                " " + lcCLI).
        
    END.

END PROCEDURE.    

PROCEDURE ipChangeOrderToErrorStatus :
    
    ASSIGN 
        llOrdStChg  = fSetOrderStatus(bfOrder.OrderId,{&ORDER_STATUS_ERROR})
        lgHoldOrder = TRUE.
    
END PROCEDURE.    

PROCEDURE ipUpdateOrderData:
    
    IF bfOrder.MSSeq = 0  
    THEN bfOrder.MSSeq = NEXT-VALUE(Mobsub).
        
    FOR EACH OrderCustomer OF bfOrder EXCLUSIVE-LOCK:
        IF OrderCustomer.Language = "0" OR
           OrderCustomer.Language = ""
        THEN OrderCustomer.Language = "1".
    END.
    
END PROCEDURE.

PROCEDURE ipGetTimeStamp :
    
    IF liMnpStatus > 0
    THEN DO:
        
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
    ELSE ldeSwitchTS  =  Func.Common:mMakeTS().
    
END PROCEDURE.   


PROCEDURE ipCreateOrderSubscriptionData:
    
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bfOrderSubscription FOR OrderSubscription.
    
    IF CAN-FIND(FIRST bfOrderSubscription WHERE bfOrderSubscription.OrderID         =  iiOrderID  
                                            AND bfOrderSubscription.OrderProductID  =  iiOrderProductID )
    THEN DO:
        FIND FIRST bfOrderSubscription WHERE bfOrderSubscription.OrderID            = iiOrderID 
                                         AND bfOrderSubscription.OrderProductID     = iiOrderProductID
                                         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                                         
        IF LOCKED(bfOrderSubscription) OR 
            NOT AVAILABLE (bfOrderSubscription) THEN 
        DO:
            Func.Common:mWriteMemo("Order",
                                    STRING(bfOrder.OrderID),
                                    0,
                                    "OrderSubscription",
                                    "OrderSubscription Record is locked.").
                                    
             ASSIGN lgHoldOrder = TRUE.
             RETURN.
         END. 
    END.
    ELSE DO:
        CREATE bfOrderSubscription.
    END.
    
    ASSIGN bfOrderSubscription.OrderID            =   iiOrderID         WHEN bfOrderSubscription.OrderID        =   0
           bfOrderSubscription.OrderProductID     =   iiOrderProductID  WHEN bfOrderSubscription.OrderProductID =   0
           bfOrderSubscription.MSSeq              =   bfOrder.MSSeq     WHEN bfOrderSubscription.MSSeq          =   0
           bfOrderSubscription.CLIType            =   lcCLIType         WHEN bfOrderSubscription.CLIType        =   ""
           bfOrderSubscription.CLI                =   lcCLI             WHEN bfOrderSubscription.CLI            =   ""
           bfOrderSubscription.FixedNumber        =   lcFixedNumber     WHEN bfOrderSubscription.FixedNumber    =   ""
           bfOrderSubscription.NumberType         =   Func.OrderProductsData:mGetMobileNumberType(INPUT iiOrderID)  WHEN bfOrderSubscription.NumberType = ""
           bfOrderSubscription.CurrOper           =   Func.OrderProductsData:mGetCurrentOperator(INPUT iiOrderID)   WHEN bfOrderSubscription.CurrOper    = ""
           bfOrderSubscription.StatusCode         =   bfOrder.StatusCode WHEN bfOrderSubscription.StatusCode  <> ""
           bfOrderSubscription.CreatedTS          =   bfOrder.crstamp    
           bfOrderSubscription.UpdatedTS          =   Func.Common:mMakeTS()
           bfOrderSubscription.ICC                =   lcICC              WHEN bfOrderSubscription.ICC = ""
           .
           
END PROCEDURE.    

PROCEDURE ipCreateSubscriptionRequest :
    
    RUN ipGetTimeStamp.
    
    IF bfOrder.OrderType = {&ORDER_TYPE_ROLLBACK} 
    THEN fReactivationRequest(INPUT bfOrder.MsSeq,
                              INPUT bfOrder.OrderId,
                              INPUT ldeSwitchTS,
                              INPUT Syst.Var:katun,
                              {&REQUEST_SOURCE_NEWTON},
                              OUTPUT ocResult).
    ELSE fSubscriptionRequest(INPUT  bfOrder.MSSeq,
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

PROCEDURE ipCreateOrderTopup :
    
    DEFINE INPUT  PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iiTopUpType      AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bfOrderTopup FOR OrderTopup.
    
     FIND FIRST bfOrderTopup WHERE bfOrderTopup.OrderID           =  iiOrderID 
                               AND bfOrderTopUp.OrderProductID    =  iiOrderProductID
                               AND bfOrderTopUp.MSSeq             =  bfOrder.MsSeq
                               AND bfOrderTopUp.TopupType         =  iiTopUpType
                               NO-LOCK NO-ERROR.
     
     IF NOT AVAILABLE bfOrderTopup
     THEN DO:
         CREATE bfOrderTopup.
     END.
     ELSE DO:
         
         FIND CURRENT bfOrderTopup EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         
         IF LOCKED(bfOrderTopup) OR NOT AVAILABLE bfOrderTopup THEN 
         DO:
             
             ASSIGN lgHoldOrder = TRUE.
                 
             Func.Common:mWriteMemo("Order",
                                    STRING(bfOrder.OrderID),
                                    0,
                                    "Initial Topup",
                                    "Order Topup updation failed.").
         END. 
     END.     
     
    ASSIGN bfOrderTopUp.OrderID         =  iiOrderID          WHEN  bfOrderTopUp.OrderID        = 0
           bfOrderTopUp.OrderProductID  =  iiOrderProductID   WHEN  bfOrderTopUp.OrderProductID = 0   
           bfOrderTopup.MsSeq           =  bfOrder.MsSeq      WHEN  bfOrderTopup.MsSeq          = 0
           bfOrderTopup.TopupType       =  iiTopUpType        WHEN  bfOrderTopup.TopupType      = 0   
           bfOrderTopup.Amount          =  Func.OrderProductsData:mGetTopupAmount(INPUT iiOrderID ,
                                                                                  INPUT iiOrderProductID,
                                                                                  INPUT {&ORDER_PRODUCT_PARAM_INITIAL_TOPUP}) 
                                           WHEN bfOrderTopup.Amount = 0
          bfOrderTopup.DisplayAmount    =  Func.OrderProductsData:mGetTopupAmount(INPUT iiOrderID ,
                                                                                  INPUT iiOrderProductID,
                                                                                  INPUT {&ORDER_PRODUCT_PARAM_INITIAL_TOPUP_DISPLAY}) 
                                           WHEN bfOrderTopup.Amount = 0   
         bfOrderTopup.Brand             =  Syst.Var:gcBrand                                         
        .                                                              
    
END PROCEDURE.



