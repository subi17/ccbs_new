
/*------------------------------------------------------------------------
    File        : orderproduct_exec.p
    Purpose     : Order Product Processing Logic

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

DEFINE INPUT PARAMETER iiOrderID AS INTEGER NO-UNDO.

DEFINE VARIABLE lgHoldOrder     AS LOGICAL           NO-UNDO.
DEFINE VARIABLE loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.

DEFINE BUFFER bf_Order FOR Order.

/* ***************************  Functions  *************************** */
FUNCTION fGetParentProductIDBasedOnChild RETURNS INT
    (INPUT iiOrderID             AS INT,
     INPUT iiChildOrderProductID AS INT):

    DEFINE BUFFER bf_OrderProduct FOR OrderProduct.

    FIND FIRST bf_OrderProduct WHERE bf_OrderProduct.OrderId        = iiOrderID             AND 
                                     bf_OrderProduct.OrderProductID = iiChildOrderProductID NO-LOCK NO-ERROR.
    IF AVAIL bf_OrderProduct THEN 
    DO:
        IF bf_OrderProduct.ParentID > 0 THEN 
           RETURN bf_OrderProduct.ParentID.
        ELSE
           RETURN bf_OrderProduct.OrderProductId.     
    END.    

    RETURN 0.

END FUNCTION.

FUNCTION fIsSIMNumberAssigned RETURNS LOGICAL
    (INPUT iiOrderID        AS INT,
     INPUT iiOrderProductId AS INT,
     OUTPUT ocICC    AS CHAR):

    DEFINE VARIABLE lcICC             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcSubscriptionICC AS CHARACTER NO-UNDO.

    ASSIGN 
        lcICC             = Func.OrderProductsData:mGetOrderICC(iiOrderID)
        lcSubscriptionICC = Func.OrderProductsData:mGetOrderMobileICC(iiOrderID, iiOrderProductId)
        ocICC             = lcICC.

    IF ocICC = "" THEN
        ocICC = lcSubscriptionICC.

    RETURN (lcICC > "" OR lcSubscriptionICC > "").

END FUNCTION.    

FUNCTION fGetMobileNumberPortingDate RETURNS DATE
    (INPUT iiOrderID        AS INT,
     INPUT iiOrderProductID AS INT):

    DEFINE BUFFER bf_OrderMobile FOR OrderMobile.
    
    FIND FIRST bf_OrderMobile WHERE bf_OrderMobile.OrderId        = iiOrderID        AND 
                                    bf_OrderMobile.OrderProductID = iiOrderProductID NO-LOCK NO-ERROR.
    IF AVAIL bf_OrderMobile THEN 
        RETURN bf_OrderMobile.RequestedPortingDate.
        
    RETURN ?.

END FUNCTION.

FUNCTION fIsOrderMobileInitialized RETURNS LOGICAL
    (INPUT iiOrderID AS INT):
  
    RETURN CAN-FIND(FIRST OrderMobile WHERE OrderMobile.OrderId = iiOrderID NO-LOCK).

END FUNCTION.

FUNCTION fValidateSIM RETURNS LOGICAL
    (INPUT icICC AS CHAR):
  
    IF icICC > "" AND Func.ValidateOrder:misValidICC(icICC) = FALSE THEN 
        RETURN FALSE.

    RETURN TRUE.
    
END FUNCTION.

FUNCTION fValidateMSISDN RETURNS LOGICAL
    (INPUT icMSISDN AS CHAR):
    
    DEFINE VARIABLE ldeCLI AS DECIMAL NO-UNDO.
    
    ASSIGN ldeCLI = DECIMAL(icMSISDN) NO-ERROR.

    IF icMSISDN = ""      OR 
       ERROR-STATUS:ERROR OR 
       CAN-FIND(FIRST MobSub WHERE MobSub.CLI = icMSISDN NO-LOCK) THEN 
       RETURN FALSE.

    RETURN TRUE.

END FUNCTION.   

FUNCTION fValidateSubscriptionType RETURNS LOGICAL 
    (INPUT icCliType AS CHAR):
    
    IF NOT Func.ValidateOrder:misCLITypeExists(icCliType) THEN 
        RETURN FALSE.
    
    RETURN TRUE.
    
END FUNCTION. 

FUNCTION fGetOrderProductStatus RETURNS CHAR
    (INPUT iiOrderID     AS INT,
     INPUT icProductType AS CHAR):

    DEFINE BUFFER bf_OrderProduct FOR OrderProduct.

    FIND FIRST bf_OrderProduct WHERE 
               bf_OrderProduct.OrderID    = iiOrderID     AND
               bf_OrderProduct.ActionType = icProductType NO-LOCK NO-ERROR.
    IF AVAIL bf_OrderProduct THEN 
        RETURN bf_OrderProduct.StatusCode.

    RETURN "".

END FUNCTION.    

FUNCTION fGetPortabilitySwitchTS RETURNS DECIMAL():
    
    DEFINE VARIABLE ldeMNPSwitchTS AS DECIMAL NO-UNDO.
    
    FOR EACH MNPProcess NO-LOCK WHERE
             MNPProcess.OrderId    EQ bf_Order.OrderID AND
             MNPProcess.StatusCode LE 6 AND
             MNPProcess.StatusCode NE 4 AND
             MNPProcess.StatusCode NE 0,
             FIRST MNPSub WHERE MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:
        ASSIGN ldeMNPSwitchTS = MNPSub.PortingTime.  /* 6 ported, 7 canceled, 4 rejected, 0 new */    
    END.
        
    RETURN ldeMNPSwitchTS.   
    
END FUNCTION.

FUNCTION fIsMNPOrder RETURNS LOGICAL
   (iiType AS INT):
    
    IF iiType EQ {&ORDER_TYPE_MNP} THEN
       RETURN TRUE. 

    RETURN FALSE.
    
END FUNCTION.

FUNCTION fGetFixedLineInstallationTS RETURNS DECIMAL():

    FIND OrderFusion WHERE OrderFusion.Brand   = bf_Order.Brand   AND
                           OrderFusion.OrderID = bf_Order.OrderID NO-LOCK NO-ERROR.

    RETURN OrderFusion.FixedInstallationTS.

END FUNCTION.

FUNCTION fIsFixedLineProvisioningDone RETURNS LOGICAL():

    FIND OrderFusion WHERE OrderFusion.Brand   = bf_Order.Brand   AND
                           OrderFusion.OrderID = bf_Order.OrderID NO-LOCK NO-ERROR.
    IF (NOT AVAIL OrderFusion) OR (NOT OrderFusion.FixedInstallationTS > 0) THEN 
       RETURN FALSE.    

    RETURN TRUE.

END FUNCTION.

FUNCTION fIsADSLFixedLineRouterShipped RETURNS LOGICAL():

    FIND OrderFusion WHERE OrderFusion.Brand   = bf_Order.Brand   AND
                           OrderFusion.OrderID = bf_Order.OrderID NO-LOCK NO-ERROR.

    FIND FIRST CliType WHERE
               Clitype.brand   EQ Syst.Var:gcBrand AND
               Clitype.clitype EQ OrderFusion.Product NO-LOCK NO-ERROR.
    IF AVAIL CliType THEN 
    DO:
        IF CliType.FixedLineType EQ {&FIXED_LINE_TYPE_ADSL} THEN 
        DO: 
            IF OrderFusion.SerialNumber NE "" AND OrderFusion.SerialNumber NE ? THEN
                RETURN TRUE.
        END.    
        ELSE 
           RETURN TRUE.    

    END.
              
    RETURN FALSE.

END FUNCTION.

FUNCTION fIsConvergentSubscriptionSIMShipped RETURNS LOGICAL():
  
    DEFINE VARIABLE lcSIMStatus AS CHAR NO-UNDO.

    FIND OrderFusion WHERE OrderFusion.Brand   = bf_Order.Brand   AND
                           OrderFusion.OrderID = bf_Order.OrderID NO-LOCK NO-ERROR.

    FIND FIRST CliType WHERE
               CliType.brand   EQ Syst.Var:gcBrand AND
               CliType.clitype EQ OrderFusion.Product NO-LOCK NO-ERROR.
    IF AVAIL CliType THEN 
    DO:
        IF CliType.TariffType EQ {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN 
        DO: 
            ASSIGN lcSIMStatus = fGetOrderProductStatus(bf_Order.OrderId, {&ORDER_PRODUCT_SIM}).

            IF lcSIMStatus = {&ORDER_STATUS_DELIVERED} THEN
                RETURN TRUE.
        END.    
        ELSE 
           RETURN TRUE.  /* Fixed only */
    END.
              
    RETURN FALSE.

END FUNCTION.

FUNCTION fIsSimOnlyMNPNonPosOrder RETURNS LOGICAL():
    
    IF bf_Order.OrderType EQ {&ORDER_TYPE_MNP}   AND
       bf_Order.CrStamp   >= 20150616.40200      AND
       LOOKUP(bf_Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 AND
       NOT CAN-FIND(FIRST OrderAccessory NO-LOCK WHERE
                          OrderAccessory.Brand   EQ Syst.Var:gcBrand AND
                          OrderAccessory.OrderId EQ bf_Order.OrderID)   AND
       NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                         OrderTimeStamp.Brand   EQ Syst.Var:gcBrand   AND
                         OrderTimeStamp.OrderID EQ bf_Order.OrderID      AND
                         OrderTimeStamp.RowType EQ {&ORDERTIMESTAMP_SIMONLY}) THEN
       RETURN TRUE.

    RETURN FALSE.

END FUNCTION.

FUNCTION fIsResignationPeriod RETURNS LOGICAL
   ():
    DEFINE VARIABLE llReserveSimAndMsisdn AS LOG    NO-UNDO.

    IF bf_Order.ResignationPeriod THEN 
        llReserveSimAndMsisdn =  NOT CAN-FIND(FIRST ActionLog NO-LOCK WHERE
                                                    ActionLog.Brand        = Syst.Var:gcBrand AND
                                                    ActionLog.TableName    = "Order" AND
                                                    ActionLog.KeyValue     = STRING(bf_Order.OrderID) AND
                                                    ActionLog.ActionId     = "RESIGNATION" AND
                                                    ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}).
    ELSE 
        ASSIGN llReserveSimAndMsisdn = TRUE.
    
    RETURN llReserveSimAndMsisdn.

END FUNCTION.
/* ***************************  Main Block  *************************** */

DO ON ERROR UNDO, RETURN ERROR:
    
    FIND FIRST bf_Order WHERE bf_Order.OrderID = iiOrderID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE bf_Order THEN
        RETURN.
    
    RUN pInitialise.

    RUN pProcess.

END.
FINALLY:
    DELETE OBJECT loEventLogMaker.
END FINALLY. 

/* **********************  Internal Procedures  *********************** */

PROCEDURE pInitialise:
        
    ASSIGN loEventLogMaker = NEW Gwy.EventLogMaker(Syst.Var:katun).

    RETURN "".

END PROCEDURE.

PROCEDURE pProcess:
    
    DEFINE BUFFER bf_ChildOrderProduct          FOR OrderProduct.   
    DEFINE BUFFER bf_RemainingChildOrderProduct FOR OrderProduct.   

    /* Process parent product's */
    FOR EACH OrderProduct NO-LOCK WHERE 
             OrderProduct.OrderID  = iiOrderID AND
             OrderProduct.ParentID = 0         BY OrderProduct.OrderProductID:     
        
        IF LOOKUP(OrderProduct.StatusCode, {&ORDER_STATUS_COMPANY_NEW}         + "," + 
                                           {&ORDER_STATUS_COMPANY_MNP}         + "," + 
                                           {&ORDER_STATUS_ROI_LEVEL_1}         + "," + 
                                           {&ORDER_STATUS_ROI_LEVEL_2}         + "," +
                                           {&ORDER_STATUS_ROI_LEVEL_3}         + "," +
                                           {&ORDER_STATUS_MORE_DOC_NEEDED}     + "," + 
                                           {&ORDER_STATUS_ERROR}               + "," + 
                                           {&ORDER_STATUS_ONGOING}             + "," +  
                                           {&ORDER_INACTIVE_STATUSES}          + "," + 
                                           {&ORDER_STATUS_MNP_ON_HOLD}         + "," + 
                                           {&ORDER_STATUS_SENDING_TO_LO}       + "," +
                                           {&ORDER_STATUS_PENDING_ICC_FROM_LO} + "," + 
                                           {&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}) > 0 THEN 
           NEXT.

        IF Func.ValidateOrder:mIsConvergentTariff(OrderProduct.ProductID) THEN
        DO: 
            /* In case of convergent, mobile line is dependent on fixed line activation. So, processing 
               router first, fixedline activation and mobile sim activation */
            FIND FIRST bf_ChildOrderProduct WHERE 
                       bf_ChildOrderProduct.OrderId    = OrderProduct.OrderId        AND 
                       bf_ChildOrderProduct.ParentID   = OrderProduct.OrderProductID AND 
                       bf_ChildOrderProduct.ActionType = {&ORDER_PRODUCT_ROUTER}     NO-LOCK NO-ERROR.
            IF AVAIL bf_ChildOrderProduct THEN
               RUN pOrderProduct(bf_ChildOrderProduct.ActionType, bf_ChildOrderProduct.OrderProductID). /* Router */

            RUN pOrderProduct(OrderProduct.ActionType, OrderProduct.OrderProductID).  /* Fixed line */

            IF CAN-FIND(FIRST MsRequest WHERE 
                              MsRequest.MsSeq   = bf_Order.MSSeq AND 
                              MsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE}) THEN
            DO:
                /* Remaining products like mobile sim */
                FOR EACH bf_RemainingChildOrderProduct WHERE 
                         bf_RemainingChildOrderProduct.OrderId  = OrderProduct.OrderId        AND 
                         bf_RemainingChildOrderProduct.ParentID = OrderProduct.OrderProductID AND 
                         ROWID(bf_RemainingChildOrderProduct)  <> ROWID(bf_ChildOrderProduct) NO-LOCK:

                    RUN pOrderProduct(bf_RemainingChildOrderProduct.ActionType, bf_RemainingChildOrderProduct.OrderProductID).

                END.    
            END.
        END.
        ELSE 
        DO:
            /* In case of mobile or fixed only process child products first and then parent */   
            FOR EACH bf_ChildOrderProduct WHERE 
                     bf_ChildOrderProduct.OrderId  = OrderProduct.OrderId        AND 
                     bf_ChildOrderProduct.ParentID = OrderProduct.OrderProductID NO-LOCK:

                IF LOOKUP(bf_ChildOrderProduct.StatusCode, {&ORDER_STATUS_COMPANY_NEW}         + "," + 
                                                           {&ORDER_STATUS_COMPANY_MNP}         + "," + 
                                                           {&ORDER_STATUS_ROI_LEVEL_1}         + "," + 
                                                           {&ORDER_STATUS_ROI_LEVEL_2}         + "," +
                                                           {&ORDER_STATUS_ROI_LEVEL_3}         + "," +
                                                           {&ORDER_STATUS_MORE_DOC_NEEDED}     + "," + 
                                                           {&ORDER_STATUS_ERROR}               + "," + 
                                                           {&ORDER_STATUS_ONGOING}             + "," +  
                                                           {&ORDER_INACTIVE_STATUSES}          + "," + 
                                                           {&ORDER_STATUS_MNP_ON_HOLD}         + "," + 
                                                           {&ORDER_STATUS_SENDING_TO_LO}       + "," +
                                                           {&ORDER_STATUS_PENDING_ICC_FROM_LO} + "," + 
                                                           {&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}) > 0 THEN
                   NEXT.                                                     

                RUN pOrderProduct(bf_ChildOrderProduct.ActionType, bf_ChildOrderProduct.OrderProductID).

            END.

            RUN pOrderProduct(OrderProduct.ActionType, OrderProduct.OrderProductID).    
        END.
        
    END. 

    RETURN "".

END PROCEDURE.    

PROCEDURE pOrderProduct:
    DEFINE INPUT PARAMETER icProductType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiProductID   AS INTEGER   NO-UNDO.
    
    CASE icProductType:
        WHEN {&ORDER_PRODUCT_SIM}      OR 
        WHEN {&ORDER_PRODUCT_ROUTER}   OR 
        WHEN {&ORDER_PRODUCT_SETUPBOX} THEN 
        DO:            
            RUN pAccessory(icProductType, iiProductID).            
        END.
        WHEN {&ORDER_PRODUCT_MOBILE}     OR 
        WHEN {&ORDER_PRODUCT_FIXED_LINE} THEN 
        DO:            
            RUN pSubscription(icProductType, iiProductID).            
        END.
        WHEN {&ORDER_PRODUCT_ACC} THEN
        DO:
            RUN pServiceRequest(icProductType, iiProductID).            
        END.
    END CASE.
    
    IF RETURN-VALUE NE "" THEN 
       RETURN RETURN-VALUE.

    RETURN "".

END PROCEDURE.  

PROCEDURE pAccessory:
    DEFINE INPUT PARAMETER icProductType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiProductID   AS INTEGER   NO-UNDO.
    
   CASE icProductType :
       
       WHEN {&ORDER_PRODUCT_SIM} THEN 
       DO:           
           RUN pSIM(iiProductID).           
       END.      
       WHEN {&ORDER_PRODUCT_ROUTER} THEN 
       DO:           
           RUN pRouter(iiProductID).           
       END. 
       WHEN {&ORDER_PRODUCT_SETUPBOX} THEN 
       DO:           
           RUN pSetupBox.           
       END. 
   END CASE.
   
   IF RETURN-VALUE NE "" THEN 
      RETURN RETURN-VALUE.

   RETURN "".

END PROCEDURE.    

PROCEDURE pSubscription:
    DEFINE INPUT PARAMETER icProductType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiProductID   AS INTEGER   NO-UNDO.
    
    CASE icProductType:
        WHEN {&ORDER_PRODUCT_MOBILE} THEN 
        DO:
            RUN pMobile(iiProductID).
        END.    
        WHEN {&ORDER_PRODUCT_FIXED_LINE} THEN 
        DO:
            RUN pFixedLine(iiProductID).
        END.    
        WHEN {&ORDER_PRODUCT_GENERIC} THEN 
        DO:
            RUN pGeneral(iiProductID).
        END.
    END CASE.       
    
    IF RETURN-VALUE NE "" THEN 
    DO:
       fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ERROR}).

       fSetOrderProductStatus(bf_Order.OrderId, iiProductID, {&ORDER_STATUS_ERROR}).

       RETURN RETURN-VALUE.
    END.   

    RETURN "".

END PROCEDURE.

PROCEDURE pServiceRequest:
    DEFINE INPUT PARAMETER icProductType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiProductID   AS INTEGER   NO-UNDO.

    CASE icProductType:
        WHEN {&ORDER_PRODUCT_ACC} THEN 
        DO:
            RUN pAgrementCustomerChange(iiProductID).
        END.
        WHEN {&ORDER_PRODUCT_STC} THEN 
        DO:
            RUN pSubscriptionTypeChange(iiProductID).
        END.
        WHEN {&ORDER_PRODUCT_RENEWAL} THEN 
        DO:
            RUN pRenewal(iiProductID).
        END.
        WHEN {&ORDER_PRODUCT_ROLLBACK} THEN 
        DO:
            RUN pRollback(iiProductID).
        END.
    END CASE.
        
    RETURN "".

END PROCEDURE.

PROCEDURE pValidateSIM:
    DEFINE INPUT PARAMETER icSIM AS CHARACTER NO-UNDO.

    DEFINE VARIABLE llValidSIM AS LOGICAL NO-UNDO.

    ASSIGN llValidSIM = fValidateSIM(icSIM).

    IF NOT llValidSIM THEN
    DO:        
        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,
                               "Order Process Error",
                               "No valid ICC could be found"). 
        RETURN "SIM Number alloted seem to be not valid".
    END. 

    RETURN "".

END PROCEDURE.

PROCEDURE pReserveSIM:
    DEFINE INPUT PARAMETER iiMsSeq AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER icICC   AS CHARACTER NO-UNDO.

    IF icICC > "" THEN 
    DO:    
        FIND FIRST Sim WHERE Sim.ICC = icICC EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF NOT AVAILABLE Sim THEN 
            RETURN "Sim number alloted cannot be reserved".

        ASSIGN SIM.MsSeq = iiMsSeq.             
    END.
     
    IF NOT CAN-FIND(LAST OrderTimeStamp NO-LOCK WHERE
                         OrderTimeStamp.Brand   = Syst.Var:gcBrand   AND
                         OrderTimeStamp.OrderID = bf_Order.OrderID AND
                         OrderTimeStamp.RowType = {&ORDERTIMESTAMP_SIMONLY}) THEN 
    DO:
        IF (LOOKUP(bf_Order.OrderChannel,"pos,cc,pre-act,vip,fusion_pos,fusion_cc,pos_pro,fusion_pos_pro") > 0 AND
            icICC > "")   OR 
            bf_Order.OrderType = {&ORDER_TYPE_ROLLBACK} THEN 
            ASSIGN SIM.SimStat = 4.
    END.
        
    RELEASE SIM.

    RETURN "".

END PROCEDURE. 

PROCEDURE pSIM:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.

    DEFINE VARIABLE lcICC AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liSubscriptionProductId AS INT NO-UNDO.

    ASSIGN liSubscriptionProductId = fGetParentProductIDBasedOnChild(bf_Order.OrderId, iiOrderProductID).  
    /* When SIM is still not alloted */
    IF NOT fIsSIMNumberAssigned(bf_Order.OrderId, liSubscriptionProductId, OUTPUT lcICC) THEN 
    DO:  
        /* When parent product is of type 'convergent subscription' */
        IF Func.ValidateOrder:mIsConvergentTariff(bf_Order.CliType) THEN 
        DO:
            loEventLogMaker:make_eventlog("oldbuffer",BUFFER bf_Order:HANDLE).

            IF bf_Order.OrderType EQ {&ORDER_TYPE_MNP} AND 
               fGetMobileNumberPortingDate(bf_Order.OrderId, iiOrderProductID) <> ? THEN /* MNP */
            DO:
                fSetOrderStatus(bf_Order.OrderID, {&ORDER_STATUS_MNP_ON_HOLD}).

                fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_MNP_ON_HOLD}).
                
                RETURN "MNP Order with missing Porting Date".
            END.

            IF LOOKUP(bf_Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 THEN /* POS */
            DO:
                fSetOrderStatus(bf_Order.OrderID, {&ORDER_STATUS_PENDING_MOBILE_LINE}).

                fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_PENDING_MOBILE_LINE}).
            END.
            ELSE IF LOOKUP(bf_Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 THEN /* NON-POS */
            DO:
                IF Func.ValidateOrder:mIsFiberType(Order.CLIType) THEN
                DO:
                    IF bf_Order.DeliverySecure > 0 THEN
                    DO: 
                        /* Since, secure deliver is opted, sim need to be delivered by logistics operator. */
                        fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_SENDING_TO_LO}).

                        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_SENDING_TO_LO}).
                    END.
                    ELSE 
                    DO:
                        /* For fiber connection, an installer need to visit customer place, so installer need to allot a sim. 
                           Just in case, installer doesn't have free sim's available with him. A cron process check_order_icc.p, 
                           will kick in at end of the day and marks order status to ORDER_STATUS_SENDING_TO_LO if sim isn't alloted 
                           in 12hrs from order is marked to ORDER_STATUS_PENDING_ICC_FROM_INSTALLER and logistics operator take care 
                           of sim allotment */
                        fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}).

                        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}).
                    END.
                END.
                ELSE 
                DO: /* ADSL */
                    IF CAN-FIND(FIRST OrderGroup NO-LOCK WHERE
                                      OrderGroup.OrderId        EQ bf_Order.OrderId         AND
                                      OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
                                      ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN 
                    DO:
                        fSetOrderStatus(bf_Order.OrderID,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).

                        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_PENDING_ICC_FROM_LO}). 
                    END.    
                    ELSE 
                    DO:
                        fSetOrderStatus(bf_Order.OrderID,{&ORDER_STATUS_SENDING_TO_LO}).

                        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_SENDING_TO_LO}). 
                    END.
                END.
            END.

            loEventLogMaker:make_eventlog("modify",BUFFER bf_Order:HANDLE).
        END.
        ELSE
        DO: /* When parent product is of type 'mobile subscription' */ 
            loEventLogMaker:make_eventlog("oldbuffer",BUFFER bf_Order:HANDLE).

            IF LOOKUP(bf_Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 THEN /* POS */
            DO: /* Ideally, this shouldn't be a possibility */
                fSetOrderStatus(bf_Order.OrderID,{&ORDER_STATUS_ERROR}).

                fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ERROR}). 
            END.
            ELSE IF LOOKUP(bf_Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 THEN /* NON-POS */
            DO:
                /* New change in SIM reservation logic, when order is placed from telesalses
                   or web then order will be moved to order queue 15 0r 16. When order sent to LO,
                   LO will pick and reserve the SIM through external API (dextra_update_order_status) */  
                IF CAN-FIND(FIRST OrderGroup NO-LOCK WHERE
                                  OrderGroup.OrderId        EQ bf_Order.OrderId          AND
                                  OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
                                  ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN 
                DO:
                    fSetOrderStatus(bf_Order.OrderID,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).

                    fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_PENDING_ICC_FROM_LO}). 
                END.    
                ELSE 
                DO:
                    fSetOrderStatus(bf_Order.OrderID,{&ORDER_STATUS_SENDING_TO_LO}).

                    fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_SENDING_TO_LO}). 
                END.
                loEventLogMaker:make_eventlog("modify",BUFFER bf_Order:HANDLE).        
            END.
        END.

        IF fIsSimOnlyMNPNonPosOrder() THEN
        DO:
            loEventLogMaker:make_eventlog("oldbuffer",BUFFER bf_Order:HANDLE).

            fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_SIM_ONLY_MNP_IN}).

            fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_SIM_ONLY_MNP_IN}). 

            loEventLogMaker:make_eventlog("modify",BUFFER bf_Order:HANDLE).

            /* Set additional OrderStamp to avoid infinitive loop */
            fMarkOrderStamp(bf_Order.OrderID,"SimOnly",0.0).   
        END.
    END.
    ELSE 
    DO:
       RUN pValidateSIM(lcICC).
       IF RETURN-VALUE NE "" THEN
            RETURN RETURN-VALUE.
       
       RUN pReserveSIM(bf_Order.MsSeq, lcICC).

       IF bf_Order.ResignationPeriod AND fIsResignationPeriod() THEN /* Delayed order */
       DO:
          loEventLogMaker:make_eventlog("oldbuffer",BUFFER bf_Order:HANDLE).
          
          fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_RESIGNATION}).

          fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_RESIGNATION}).

          loEventLogMaker:make_eventlog("modify",BUFFER bf_Order:HANDLE).

          RETURN "Resignation Period".
       END.   

       fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_DELIVERED}).

       IF Func.ValidateOrder:mIsConvergentTariff(bf_Order.CliType) THEN  
          RUN pMobile(iiOrderProductID).
    END.   

    RETURN "".

END PROCEDURE.

PROCEDURE pRouter:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.

    IF fIsADSLFixedLineRouterShipped() THEN 
       fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_DELIVERED}).

    RETURN "".

END PROCEDURE.

PROCEDURE pSetupBox:
    
    RETURN "".

END PROCEDURE.

PROCEDURE pValidateSubscriptionType:
    DEFINE INPUT PARAMETER icSubscriptionType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE llValidTariffType AS LOGICAL   NO-UNDO.

    ASSIGN llValidTariffType = fValidateSubscriptionType(icSubscriptionType).

    IF NOT llValidTariffType THEN
    DO:
        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,      
                               "MSISDN",
                               "Subscription type is invalid").

        RETURN "Subscription type is invalid".
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pValidateMSISDN:
    DEFINE INPUT PARAMETER icMSISDN AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE llValidMSISDN AS LOGICAL   NO-UNDO.
    
    ASSIGN llValidMSISDN = fValidateMSISDN(icMSISDN).

    IF NOT llValidMSISDN THEN
    DO:
        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,      
                               "MSISDN",
                               "MSISDN is invalid").

        RETURN "MSISDN is invalid".
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pReserveMSISDN:
    DEFINE INPUT PARAMETER iiMsSeq   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER icMSISDN  AS CHARACTER NO-UNDO.
    
    FIND FIRST MSISDNNUMBER WHERE MSISDNNUMBER.CLI = icMSISDN NO-LOCK NO-ERROR.
    IF NOT AVAIL MSISDNNUMBER THEN 
    DO:
        CREATE MSISDNNUMBER.
        ASSIGN MSISDNNUMBER.CLI = icMSISDN.
    END.
            
    FIND FIRST MSISDN WHERE
               MSISDN.Brand = Syst.Var:gcBrand AND
               MSISDN.CLI   = icMSISDN         NO-LOCK NO-ERROR. 
    IF NOT AVAIL MSISDN THEN 
    DO:
        CREATE MSISDN.
        ASSIGN
            MSISDN.Brand      = Syst.Var:gcBrand
            MSISDN.MSSeq      = iiMsSeq
            MSISDN.Cli        = icMSISDN
            MSISDN.ActionDate = TODAY 
            MSISDN.ValidFrom  = Func.Common:mMakeTS()
            MSISDN.StatusCode = 22.
    END.         
    
    RELEASE MSISDN.
    RELEASE MSISDNNUMBER.

    RETURN "".

END PROCEDURE.

PROCEDURE pMobile:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.

    DEFINE VARIABLE lcTariff         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcMSISDN         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcSIMStatus      AS CHARACTER NO-UNDO.

    ASSIGN lcSIMStatus = fGetOrderProductStatus(bf_Order.OrderId, {&ORDER_PRODUCT_SIM}).

    IF lcSIMStatus = {&ORDER_STATUS_DELIVERED} THEN
    DO: 
        ASSIGN 
            lcMSISDN = Func.OrderProductsData:mGetOrderCLI(bf_Order.OrderId)
            lcTariff = Func.OrderProductsData:mGetOrderCLIType(bf_Order.OrderId).
        
        RUN pSetDefaultsToMissingAttributesOfOrder.

        RUN pValidateMSISDN(lcMSISDN).
        IF RETURN-VALUE NE "" THEN
            RETURN RETURN-VALUE.

        IF lcTariff = "" THEN 
        DO:
            fSetOrderStatus(bf_Order.OrderID,{&ORDER_STATUS_IN_CONTROL}).

            fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_IN_CONTROL}). 

            RETURN "".
        END.

        RUN pValidateSubscriptionType(lcTariff).
        IF RETURN-VALUE NE "" THEN
            RETURN RETURN-VALUE.

        RUN pReserveMSISDN(bf_Order.MsSeq, lcMSISDN).

        RUN pMobileActivationRequest(iiOrderProductID, lcMSISDN).

        IF RETURN-VALUE NE "" THEN
            RETURN RETURN-VALUE.
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pFixedLine:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE lcCLIType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcFixedNumber AS CHARACTER NO-UNDO.
    
    ASSIGN 
        lcCLIType     = Func.OrderProductsData:mGetOrderCLIType(iiOrderID)
        lcFixedNumber = Func.OrderProductsData:mGetOrderProductFixedNumber(iiOrderID, iiOrderProductID).

    IF fIsADSLFixedLineRouterShipped() AND 
       fIsFixedLineProvisioningDone()  AND
       (NOT CAN-FIND(FIRST MsRequest WHERE
                          MsRequest.MsSeq   = bf_Order.MSSeq  AND
                          MsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE})) THEN 
    DO:  
        RUN pFixedLineActivationRequest(iiOrderProductID, lcFixedNumber, lcCLIType).
        IF RETURN-VALUE NE "" THEN
           RETURN RETURN-VALUE.   
    END.

    RETURN "".

END PROCEDURE.    

PROCEDURE pGeneral:

END PROCEDURE.

PROCEDURE pFixedLineActivationRequest:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER icFixedNumber    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCliType        AS CHARACTER NO-UNDO.

    DEF VAR lcResult         AS CHAR    NO-UNDO.
    DEF VAR ldeSwitchTS      AS DECIMAL NO-UNDO.
    DEF VAR llStatusAssigned AS LOGICAL NO-UNDO.
    DEF VAR ldFixedActTS     AS DECIMAL NO-UNDO.

    ASSIGN ldFixedActTS = fGetFixedLineInstallationTS().

    fSubscriptionRequest(INPUT  bf_Order.MSSeq,
                         INPUT  icFixedNumber,
                         INPUT  bf_Order.CustNum,
                         INPUT  1,
                         INPUT  Syst.Var:katun,
                         INPUT  ldFixedActTS,
                         INPUT  "CREATE-FIXED",
                         INPUT  STRING(bf_Order.OrderId),
                         INPUT  STRING(iiOrderProductID),
                         INPUT  "", /*for old SIM*/
                         INPUT  "", /*for Reason info*/
                         INPUT  "", /*for ContractID*/
                         INPUT  FALSE,
                         INPUT  0,
                         INPUT  {&REQUEST_SOURCE_NEWTON},
                         OUTPUT lcResult).

    IF lcResult > "" THEN 
    DO:
        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ERROR}). 

        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ERROR}).

        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,
                               "FIXED LINE ACTIVATION FAILED",
                               lcResult).
        RETURN lcResult.
    END.            
    ELSE 
    DO:
        IF CAN-FIND(FIRST Memo NO-LOCK WHERE
                          Memo.Brand     = Syst.Var:gcBrand AND
                          Memo.HostTable = "Order" AND
                          Memo.Keyvalue  = STRING(bf_Order.OrderID) AND
                          Memo.MemoText  = "Fixed Cancellation failed because installation was already in place") THEN 
        DO:
            fSetOrderStatus(Order.OrderID, {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}).

            fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}).
        END.
        ELSE IF Func.ValidateOrder:mIsFixedOnlyTariff(icCliType) THEN
        DO:
            llStatusAssigned = fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ONGOING}).

            IF llStatusAssigned THEN 
            DO:
               fMarkOrderStamp(bf_Order.OrderID,"Change",0.0).  /* Mark timestamp as change */     

               fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ONGOING}).
            END.   
        END.
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pMobileActivationRequest: 
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER icMSISDN         AS CHARACTER NO-UNDO.

    DEF VAR lcResult         AS CHAR    NO-UNDO.
    DEF VAR ldeSwitchTS      AS DECIMAL NO-UNDO.
    DEF VAR llStatusAssigned AS LOGICAL NO-UNDO.

    IF fIsMNPOrder(bf_Order.OrderType) THEN 
        ASSIGN ldeSwitchTS = fGetPortabilitySwitchTS().
    ELSE 
        ASSIGN ldeSwitchTS = Func.Common:mMakeTS().
    
    fSubscriptionRequest(INPUT  bf_Order.MSSeq,
                         INPUT  icMSISDN,
                         INPUT  bf_Order.CustNum,
                         INPUT  1,
                         INPUT  Syst.Var:katun,
                         INPUT  ldeSwitchTS,
                         INPUT  "CREATE",
                         INPUT  STRING(bf_Order.OrderId),
                         INPUT  STRING(iiOrderProductID),
                         INPUT  "", /*for old SIM*/
                         INPUT  "", /*for Reason info*/
                         INPUT  "", /*for ContractID*/
                         INPUT  FALSE,
                         INPUT  0,
                         INPUT  {&REQUEST_SOURCE_NEWTON},
                         OUTPUT lcResult).

    IF lcResult > "" THEN 
    DO:
        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ERROR}). 

        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ERROR}).

        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,
                               "ACTIVATION FAILED",
                               lcResult).
        RETURN lcResult.
    END.            
    ELSE 
    DO:
        llStatusAssigned = fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ONGOING}).

        IF llStatusAssigned THEN 
        DO:
           fMarkOrderStamp(bf_Order.OrderID,"Change",0.0).  /* Mark timestamp as change */     

           fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ONGOING}).
        END.   
    END.
    
    RETURN "".

END PROCEDURE.

PROCEDURE pSetDefaultsToMissingAttributesOfOrder:
    
    IF bf_Order.MSSeq = 0  THEN 
        ASSIGN bf_Order.MSSeq = NEXT-VALUE(Mobsub).
        
    FOR EACH OrderCustomer OF bf_Order EXCLUSIVE-LOCK:
        IF OrderCustomer.Language = "0" OR OrderCustomer.Language = "" THEN 
            ASSIGN OrderCustomer.Language = "1".
    END.
    
END PROCEDURE. 

PROCEDURE pCheckOrderStatus:
    
    IF LOOKUP(bf_Order.Statuscode,{&ORDER_INACTIVE_STATUSES} + ",4,74") > 0  THEN 
        ASSIGN lgHoldOrder = TRUE.
    
END PROCEDURE.

PROCEDURE pCheckRollbackOrders:
    
    IF CAN-FIND(FIRST MsRequest WHERE
        MsRequest.MsSeq     EQ bf_Order.MSSeq                  AND
        MsRequest.ReqType   EQ {&REQTYPE_SUBSCRIPTION_CREATE} AND
        MsRequest.ReqStatus NE {&REQUEST_STATUS_CANCELLED}) THEN 
    DO: 
        ASSIGN 
            lgHoldOrder = TRUE.
    END.
    
END PROCEDURE.                

PROCEDURE pSetRenewalOrderStatus:

    DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO.

    IF bf_Order.OrderChannel BEGINS "Renewal_POS" AND
        CAN-FIND(FIRST MsRequest WHERE
        MsRequest.MsSeq   = bf_Order.MSSeq                AND
        MsRequest.ReqType = {&REQTYPE_AFTER_SALES_ORDER} AND
        MsRequest.ReqIParam1 = bf_Order.OrderID) THEN 
    DO:
        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ONGOING}).
        ASSIGN lgHoldOrder = TRUE.
        RETURN.
    END.
               
    IF LOOKUP(bf_Order.OrderChannel,{&ORDER_CHANNEL_DIRECT_RENEWAL}) > 0 THEN
        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_WAITING_SENDING_LO}).  
    ELSE 
        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ONGOING}). 

    IF bf_Order.OrderChannel BEGINS "Renewal_POS" THEN 
    DO:
        fAfterSalesRequest(bf_Order.MsSeq,
                           bf_Order.OrderId,
                           Syst.Var:katun,
                           Func.Common:mMakeTS(),
                           "7",
                           OUTPUT ocResult).
                   
        IF ocResult > "" THEN 
        DO:
            Func.Common:mWriteMemo("Order",
                                   STRING(bf_Order.OrderID),
                                   0,
                                   "After Sales Request creation failed",
                                   ocResult).
            fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_IN_CONTROL}).
        END.
    END.
                 
    ASSIGN lgHoldOrder = TRUE.
        
    RELEASE bf_Order NO-ERROR.

END PROCEDURE.

PROCEDURE pSubscriptionTypeChange:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.

    DEFINE VARIABLE liRequestID AS INTEGER NO-UNDO.

    RUN Mm/fusion_stc.p(bf_Order.OrderID, OUTPUT liRequestID).
               
    IF liRequestID > 0 THEN
    DO:
        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ONGOING}).

        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ONGOING}).          
    END.    
    ELSE 
    DO:
        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,
                               "STC request creation failed",
                               RETURN-VALUE).

        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_IN_CONTROL}).

        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_IN_CONTROL}).
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pAgreementCustomerChange:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.

    DEFINE VARIABLE liRequestID AS INTEGER NO-UNDO.

    RUN Mm/acc_order.p(bf_Order.OrderID, OUTPUT liRequestID).
               
    IF liRequestID > 0 THEN
    DO:
        fSetOrderStatus(bf_Order.OrderId, {&ORDER_STATUS_ONGOING}).

        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ONGOING}).
    END.     
    ELSE 
    DO:            
        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,
                               "ACC request creation failed",
                               RETURN-VALUE).

        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_IN_CONTROL}).

        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_IN_CONTROL}).
    END.

    RETURN "".

END PROCEDURE. 

PROCEDURE pReactivationRequest:
    DEFINE INPUT PARAMETER iiOrderProductID AS INTEGER NO-UNDO.

    DEF VAR lcResult         AS CHAR    NO-UNDO.
    DEF VAR llStatusAssigned AS LOGICAL NO-UNDO.

    fReactivationRequest(INPUT bf_Order.MsSeq,
                         INPUT bf_Order.OrderId,
                         INPUT Func.Common:mMakeTS(),
                         INPUT Syst.Var:katun,
                         INPUT {&REQUEST_SOURCE_NEWTON},
                         OUTPUT lcResult).

    IF lcResult > "" THEN 
    DO:
        fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ERROR}). 

        fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ERROR}).

        Func.Common:mWriteMemo("Order",
                               STRING(bf_Order.OrderID),
                               0,
                               "REACTIVATION FAILED",
                               lcResult).
        RETURN lcResult.
    END.            
    ELSE 
    DO:
        llStatusAssigned = fSetOrderStatus(bf_Order.OrderId,{&ORDER_STATUS_ONGOING}).

        IF llStatusAssigned THEN 
        DO:
           fMarkOrderStamp(bf_Order.OrderID,"Change",0.0).  /* Mark timestamp as change */     

           fSetOrderProductStatus(bf_Order.OrderId, iiOrderProductID, {&ORDER_STATUS_ONGOING}).
        END.   
    END.
    
    RETURN "".

END PROCEDURE.   
