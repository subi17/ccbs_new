
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

DEFINE VARIABLE lcCLI        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcICC        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSIMonlyMNP AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeSwitchTS  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liMnpStatus  AS INTEGER   NO-UNDO.
DEFINE VARIABLE ocResult     AS CHAR      NO-UNDO.
DEFINE VARIABLE lgHoldOrder  AS LOGICAL   NO-UNDO.

DEFINE BUFFER bfOrder FOR Order.

FIND bfOrder WHERE bfOrder.OrderID = iiOrderID EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE bfOrder THEN RETURN.

DEFINE BUFFER bfOrderProduct FOR OrderProduct.

/*Assigning the main variables*/    
ASSIGN 
    lcSIMonlyMNP  = TRIM(fCParamC("SIMonlyMNPorder"))
    liMnpStatus  =  INTEGER(Func.OrderProductsData:mGetMobileNumberType(INPUT iiOrderID) = {&TYPE_MNP})     
    llgMNPSimOnly = lcSIMonlyMNP      EQ "true"   AND
                    bfOrder.OrderType EQ 1        AND
                    bfOrder.CrStamp   >= 20150616.40200 AND
                    liMnpStatus       EQ 1        AND
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
   
        RUN ipProcessSIM.
        IF lgHoldOrder = TRUE THEN RETURN.
       
    END. 
    
    IF LAST-OF(OrderProduct.ParentID) 
    THEN DO:
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID         =   OrderProduct.OrderID
                                    AND bfOrderProduct.OrderProductID  =   OrderProduct.ParentID
                                  NO-LOCK NO-ERROR.
                               
        IF AVAILABLE bfOrderProduct 
        THEN DO:
            
            IF bfOrderProduct.ActionType =  {&ORDER_PRODUCT_SUBSCRIPTION}
            THEN DO:
                RUN ipProcessMobileSubscription. 
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
    
    ASSIGN 
        lcICC = Func.OrderProductsData:mGetOrderICC(INPUT iiOrderID).
         
    IF lcICC EQ "" 
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
                
                ASSIGN lgHoldOrder  =  TRUE.
                
             END.
        END.
    END.
            
END PROCEDURE.

PROCEDURE ipProcessMobileSubscription:
    
    ASSIGN 
        lcCLIType = Func.OrderProductsData:mGetOrderCLIType(INPUT iiOrderID)
        lcCLI     = Func.OrderProductsData:mGetOrderCLI(INPUT iiOrderID).
        
END PROCEDURE.

PROCEDURE ipProcessFixedLine:
    
    ASSIGN 
        lcCLIType = Func.OrderProductsData:mGetOrderCLIType(INPUT iiOrderID)
        lcCLI     = Func.OrderProductsData:mGetOrderCLI(INPUT iiOrderID).
    
    
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

PROCEDURE ipCreateSubscriptionRequest :
    
    RUN ipGetTimeStamp.
    
    IF Order.OrderType = {&ORDER_TYPE_ROLLBACK} 
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
