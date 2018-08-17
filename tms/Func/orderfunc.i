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
{Syst/tmsconst.i}
{Func/forderstamp.i}
{Func/dextra.i}
{Syst/eventval.i}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Func/msisdn.i}
{Func/create_eventlog.i}
{Func/digital_signature.i}
{Func/extralinefunc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhOrderStatusChange AS HANDLE NO-UNDO.
END.

FUNCTION fGetChildProductID RETURNS INT
    (INPUT iiOrderID              AS INT,
     INPUT iiParentOrderProductID AS INT,
     INPUT icChildProductType     AS CHAR):

    DEFINE BUFFER bf_OrderProduct FOR OrderProduct.

    FIND FIRST bf_OrderProduct WHERE bf_OrderProduct.OrderId    = iiOrderID              AND  
                                     bf_OrderProduct.ParentID   = iiParentOrderProductID AND 
                                     bf_OrderProduct.ActionType = icChildProductType     NO-LOCK NO-ERROR.
    IF AVAIL bf_OrderProduct THEN 
       RETURN bf_OrderProduct.OrderProductId.     
    
    RETURN 0.

END FUNCTION.

FUNCTION fGetParentProductID RETURNS INT
    (INPUT iiOrderID              AS INT,
     INPUT icProductType          AS CHAR):

    DEFINE VARIABLE lcProductType AS CHAR NO-UNDO.

    DEFINE BUFFER bf_OrderProduct FOR OrderProduct.

    IF icProductType = {&ORDER_PRODUCT_SUBSCRIPTION} THEN 
       ASSIGN lcProductType = (IF CAN-FIND(FIRST bf_OrderProduct WHERE 
                                                 bf_OrderProduct.OrderId  = iiOrderID AND 
                                                 bf_OrderProduct.ParentID = 0         AND 
                                                 bf_OrderProduct.ActionType = {&ORDER_PRODUCT_FIXED_LINE} NO-LOCK) THEN 
                                  {&ORDER_PRODUCT_FIXED_LINE} 
                               ELSE IF CAN-FIND(FIRST bf_OrderProduct WHERE 
                                                      bf_OrderProduct.OrderId  = iiOrderID AND 
                                                      bf_OrderProduct.ParentID = 0         AND 
                                                      bf_OrderProduct.ActionType = {&ORDER_PRODUCT_MOBILE} NO-LOCK) THEN 
                                  {&ORDER_PRODUCT_MOBILE}
                               ELSE 
                                  {&ORDER_PRODUCT_GENERIC}).

    FIND FIRST bf_OrderProduct WHERE bf_OrderProduct.OrderId    = iiOrderID     AND  
                                     bf_OrderProduct.ParentID   = 0             AND 
                                     bf_OrderProduct.ActionType = lcProductType NO-LOCK NO-ERROR.
    IF AVAIL bf_OrderProduct THEN 
       RETURN bf_OrderProduct.OrderProductId.     
    
    RETURN 0.

END FUNCTION.

FUNCTION fSetOrderProductStatus RETURNS LOGICAL
    (INPUT iiOrderID        AS INT,
     INPUT iiOrderProductID AS INT,
     INPUT icStatusCode     AS CHAR):

    DEFINE BUFFER bf_OrderProduct FOR OrderProduct.

    FIND FIRST bf_OrderProduct WHERE 
               bf_OrderProduct.OrderID        = iiOrderID        AND
               bf_OrderProduct.OrderProductID = iiOrderProductID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAIL bf_OrderProduct THEN 
        RETURN FALSE.

    ASSIGN bf_OrderProduct.StatusCode = icStatusCode.

    RELEASE bf_OrderProduct.

    RETURN TRUE.

END FUNCTION.

FUNCTION fIsMainLineOrderPending RETURNS LOGICAL
   (INPUT pcIdType AS CHAR,
    INPUT pcPersonId AS CHAR,
    INPUT iiExcludeOrderID AS INT):

   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderAction FOR OrderAction.
   DEF BUFFER CLIType FOR CLIType.

   FOR EACH OrderCustomer NO-LOCK WHERE   
            OrderCustomer.Brand      EQ Syst.Var:gcBrand AND 
            OrderCustomer.CustId     EQ pcPersonId AND
            OrderCustomer.CustIdType EQ pcIdType AND
            OrderCustomer.RowType    EQ 1,
      EACH  Order NO-LOCK WHERE
            Order.Brand              EQ Syst.Var:gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND 
            Order.OrderType          NE {&ORDER_TYPE_STC} AND 
            LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0,
       EACH OrderAction NO-LOCK WHERE 
            OrderAction.Brand = Order.Brand AND
            OrderAction.OrderId = Order.OrderID AND
            OrderAction.ItemType = "BundleItem":

      IF iiExcludeOrderID > 0 AND Order.OrderID EQ iiExcludeOrderID THEN NEXT.
      IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.Brand = Syst.Var:gcBrand AND
                        CLIType.CLIType = OrderAction.ItemKey AND
                        CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Function makes new MSOwner when subscription is partially
   terminated or mobile part order closed. Calling program must have
   commali.i, Syst.Var:katun defined and call fCleanEventObjects after this function */
FUNCTION fUpdatePartialMSOwner RETURNS LOGICAL
   (iiMsSeq AS INT,
    icFixedNumber AS CHAR):
   DEF VAR ldUpdateTS AS DEC NO-UNDO.
   DEF BUFFER MsOwner FOR MsOwner.
   DEF BUFFER bNewMsowner FOR Msowner.

   ldUpdateTS = Func.Common:mMakeTS().
   FIND FIRST MSOwner WHERE 
              MSOwner.MsSeq  = iiMsSeq AND
              MSOwner.TsEnd >= ldUpdateTS
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL MSOwner THEN RETURN FALSE.

   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhMsOwner AS HANDLE NO-UNDO.
      lhMsOwner = BUFFER MSOwner:HANDLE.
      RUN StarEventInitialize(lhMsOwner).
      RUN StarEventSetOldBuffer (lhMsOwner).
   END.

   MSOwner.TsEnd = ldUpdateTS.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent (lhMsOwner).
   END.

   CREATE bNewMsowner.
   BUFFER-COPY MSOwner EXCEPT TsEnd tsbegin TO bNewMsowner.
   ASSIGN
      bNewMsowner.CLI = icFixedNumber
      bNewMsowner.imsi = ""
      bNewMsowner.CliEvent = "F"
      bNewMsowner.tsbegin = Func.Common:mSecOffSet(ldUpdateTS,1)
      bNewMsowner.TsEnd = 99999999.99999.

   IF llDoEvent THEN DO:
      lhMsOwner = BUFFER bNewMsowner:HANDLE.
      fMakeCreateEvent (lhMsOwner, "", "", "").
   END.

   RELEASE MSOwner.
   RELEASE bNewMsowner.
   RETURN TRUE.

END FUNCTION.

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
   DEF VAR liRequest  AS INT NO-UNDO.

   DEF BUFFER OrderPayment FOR OrderPayment.
   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER OrderFusion FOR OrderFusion.

   ORDER_TRANS:
   DO TRANS:
      FIND bfOrder WHERE
           bfOrder.Brand = Syst.Var:gcBrand AND
           bfOrder.OrderId = iOrderId
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bfOrder THEN DO:  
         bfOrder.StatusCode = icStatus.

         /* orders to status new should not be reported to ROI */
         IF LOOKUP(icStatus,"1,3,30") = 0 AND
            bfOrder.Ordertype NE {&ORDER_TYPE_STC} AND
            bfOrder.OrderType NE {&ORDER_TYPE_ACC} THEN 
            bfOrder.SendToROI  = {&ROI_HISTORY_TO_SEND}.

         /* RES-538 Digital Signature for Tienda and Telesales only */
         fHandleSignature(bfOrder.OrderId,icStatus).

         /* Mark time stamp, if order statuscode is changed */
         case icStatus:
            when "6" then do:
               fMarkOrderStamp(bfOrder.OrderID,"Delivery",0.0).
            end.
            when "7" or when "8" or when "9" then do:
               fMarkOrderStamp(bfOrder.OrderID,"Close",0.0).

               /* YDR-2495 creating STC request to fixed only when mobile part is terminated */
               FIND FIRST MobSub NO-LOCK WHERE
                          MobSub.MsSeq = bfOrder.MsSeq NO-ERROR.
               IF AVAILABLE MobSub THEN DO:
                  IF MobSub.CLIType  EQ bfOrder.CLIType             AND
                     MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} THEN
                     liRequest = fConvFixedSTCReq(bfOrder.CLIType,
                                                  bfOrder.MsSeq,
                                                  Func.Common:mMake2DT(TODAY + 1,0),
                                                  {&REQUEST_SOURCE_ORDER_CANCELLATION},
                                                  0).
               END.

               FIND FIRST OrderAccessory OF bfOrder WHERE
                          OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}) 
                          NO-LOCK NO-ERROR.
               IF AVAILABLE OrderAccessory AND
                  LOOKUP(STRING(OrderAccessory.HardBook),"1,2") > 0 THEN
                  llHardBook = TRUE.

               FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
                          OrderFusion.Brand = Syst.Var:gcBrand AND
                          OrderFusion.OrderID = bfOrder.OrderID AND
                   LOOKUP(OrderFusion.FusionStatus,SUBST("&1,&2",
                         {&FUSION_ORDER_STATUS_NEW},
                         {&FUSION_ORDER_STATUS_ERROR})) > 0 NO-ERROR.

               IF AVAIL OrderFusion THEN DO:
                  ASSIGN
                     OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_CANCELLED}
                     OrderFusion.UpdateTS = Func.Common:mMakeTS().
                  RELEASE OrderFusion.
               END.

               IF bfOrder.OrderType EQ {&ORDER_TYPE_ACC} THEN
                  FOR EACH MsRequest WHERE 
                           MsRequest.MsSeq = Order.Msseq AND
                           MsRequest.ReqType = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} AND
                           MsRequest.ReqIParam4 = Order.OrderID AND
                           MsRequest.Actstamp > Func.Common:mMakeTS() AND
                     LOOKUP(STRING(MsRequest.ReqStatus),
                           {&REQ_INACTIVE_STATUSES}) = 0:
                     fChangeReqStatus(MsRequest.MsRequest,
                                      4,"Cancelled by ACC order closing").
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
                                   MSISDN.Brand = Syst.Var:gcBrand AND
                                   MSISDN.CLI   = MobSub.CLI
                        EXCLUSIVE-LOCK NO-ERROR.
                        /* No mobile created. Release MSISDN */
                        IF AVAIL MSISDN AND MSISDN.StatusCode EQ 3 THEN DO:
                           fMakeMsidnHistory(INPUT RECID(MSISDN)).

                           IF fIsYoigoCLI(MobSub.CLI) EQ TRUE THEN
                              MSISDN.StatusCode = {&MSISDN_ST_MNP_OUT_YOIGO}.
                           ELSE MSISDN.StatusCode = {&MSISDN_ST_ASSIGNED_TO_ORDER}.
                           MSISDN.CustNum = 0.
                           MSISDN.ValidTo = Func.Common:mMakeTS().
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
                        FusionMessage.UpdateTS = Func.Common:mMakeTS()
                        FusionMessage.FixedStatusDesc = "Order closed"
                        FusionMessage.messageStatus = 
                           {&FUSIONMESSAGE_STATUS_CANCELLED}.
                  RELEASE FusionMessage.
               END.
               
               IF Syst.Var:katun NE "Dextra" AND
                 (bfOrder.Logistics > "" OR llHardBook = TRUE OR
                 llCancelFusion = TRUE) THEN DO:
                  fLogisticsRequest(
                     bfOrder.MsSeq,
                     bfOrder.OrderId,
                     "CANCEL",
                     Func.Common:mMakeTS(),
                     {&REQUEST_SOURCE_ORDER_CANCELLATION},
                     OUTPUT lcResult).

                  IF lcResult > "" THEN 
                     Func.Common:mWriteMemo("Order",
                                      STRING(bfOrder.OrderID),
                                      0,
                                      "Logistics cancel failed",
                                      lcResult).
               END.

               /* YDR-695 */
               IF bfOrder.InvNum = 0 AND
                  bfOrder.OrderChannel BEGINS "retention" THEN
                  FOR FIRST OrderPayment NO-LOCK WHERE
                            OrderPayment.Brand = Syst.Var:gcBrand AND
                            OrderPayment.OrderId = bfOrder.OrderId AND
                           (OrderPayment.Method = {&ORDERPAYMENT_M_CREDIT_CARD}
                            OR
                            OrderPayment.Method = {&ORDERPAYMENT_M_PAYPAL}):

                     CREATE ActionLog.
                     ASSIGN
                        ActionLog.Brand     = Syst.Var:gcBrand
                        ActionLog.ActionID  = "OrderCancelRetention"
                        ActionLog.ActionTS  = Func.Common:mMakeTS()
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
                            bfOrder2.Brand = Syst.Var:gcBrand AND
                            bfOrder2.MultiSimID = bfOrder.MultiSimID AND
                            bfOrder2.MultiSimType =
                              {&MULTISIMTYPE_SECONDARY}:
                     RUN Mc/closeorder.p(bfOrder2.OrderId,TRUE).
                  END.

               END.

               /* release pending secondary line orders, YDR-1089 */
               IF CAN-FIND(FIRST OrderAction WHERE
                                 OrderAction.Brand = Syst.Var:gcBrand AND
                                 OrderAction.OrderId = bfOrder.OrderID AND
                                 OrderAction.ItemType = "BundleItem" AND
                                 CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                                CLIType.Brand = Syst.Var:gcBrand AND
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
                           bfOrderCustomer2.Brand      EQ Syst.Var:gcBrand AND 
                           bfOrderCustomer2.CustId     EQ bfOrderCustomer.CustID AND
                           bfOrderCustomer2.CustIdType EQ bfOrderCustomer.CustIdType AND
                           bfOrderCustomer2.RowType    EQ 1,
                      EACH bfOrder2 NO-LOCK WHERE
                           bfOrder2.Brand              EQ Syst.Var:gcBrand AND
                           bfOrder2.orderid            EQ bfOrderCustomer2.Orderid AND
                           ROWID(bfOrder2) NE ROWID(bfOrder) AND
                           (bfOrder2.statuscode        EQ {&ORDER_STATUS_PENDING_MAIN_LINE} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_ROI_LEVEL_1} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_ROI_LEVEL_2} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_ROI_LEVEL_3} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_MORE_DOC_NEEDED} OR
                            bfOrder2.statuscode        EQ {&ORDER_STATUS_MNP_ON_HOLD}),
                      FIRST CLIType NO-LOCK WHERE
                            CLIType.Brand = Syst.Var:gcBrand AND
                            CLIType.CLIType = bfOrder2.CLIType AND
                           (CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
                            CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL}):

                     IF CAN-FIND(FIRST OrderAction WHERE
                                       OrderAction.Brand = Syst.Var:gcBrand AND
                                       OrderAction.OrderId = bfOrder2.OrderID AND
                                       OrderAction.ItemType = "BundleItem" AND
                              CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                             CLIType.Brand = Syst.Var:gcBrand AND
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
           bfOrder.Brand = Syst.Var:gcBrand AND
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
            Stock.Brand   = Syst.Var:gcBrand AND
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

FUNCTION fGetReleaseStatus RETURNS CHARACTER 
   (INPUT icOrderType AS INT):

   CASE icOrderType:
      WHEN {&ORDER_TYPE_NEW}     THEN RETURN {&ORDER_STATUS_NEW}.
      WHEN {&ORDER_TYPE_MNP}     THEN RETURN {&ORDER_STATUS_MNP}.
      WHEN {&ORDER_TYPE_RENEWAL} THEN RETURN {&ORDER_STATUS_RENEWAL_STC}.
      OTHERWISE.
   END CASE.

   RETURN "".

END.   

/* Function checks another mainline for Mobile only additional line */
FUNCTION fCheckMainLineForMobileOnly RETURNS INTEGER
   (INPUT icCustIDType AS CHAR,
    INPUT icCustID     AS CHAR):
   
   DEFINE BUFFER lbOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER lbOrder         FOR Order.
   
   DEF VAR liCount            AS INT  NO-UNDO. 
   DEF VAR lcMainLineCLITypes AS CHAR NO-UNDO INITIAL "CONT25,CONT26". 

   REPEAT liCount = 1 TO NUM-ENTRIES(lcMainLineCLITypes):

      FOR EACH lbOrderCustomer NO-LOCK WHERE
               lbOrderCustomer.Brand      = Syst.Var:gcBrand      AND
               lbOrderCustomer.CustIDType = icCustIDType AND
               lbOrderCustomer.CustID     = icCustID     AND
               lbOrderCustomer.RowType    = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
         FIRST lbOrder NO-LOCK WHERE
               lbOrder.Brand   = Syst.Var:gcBrand                                 AND
               lbOrder.OrderID = lbOrderCustomer.OrderID                 AND
               lbOrder.CLIType = ENTRY(liCount,lcMainLineCLITypes)       AND 
               LOOKUP(lbOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 BY Order.CrStamp:

         RETURN lbOrder.OrderId.

      END.

   END.

   RETURN 0.

END.    

/* Function releases OR CLOSE Additional lines */
FUNCTION fActionOnAdditionalLines RETURN LOGICAL
   (INPUT icCustIDType     AS CHAR,
    INPUT icCustID         AS CHAR,
    INPUT icCLIType        AS CHAR,
    INPUT illgProMigration AS LOG,
    INPUT icAction         AS CHAR):

   DEF BUFFER labOrder         FOR Order.
   DEF BUFFER labOrderCustomer FOR OrderCustomer.
   DEF BUFFER labOrderAction   FOR OrderAction.

   DEF VAR lcNewOrderStatus AS CHAR NO-UNDO INITIAL "".
   DEF VAR lcDiscList       AS CHAR NO-UNDO INITIAL "". 
   DEF VAR llgDeleteAction  AS LOG  NO-UNDO INITIAL FALSE.  
   DEF VAR liMOOrderId      AS INT  NO-UNDO INITIAL 0.
   DEF VAR llgMainLineAvail AS LOG  NO-UNDO INITIAL FALSE.   
   DEF VAR illgConvOrder    AS LOG  NO-UNDO INITIAL FALSE. 

   IF fIsConvergenceTariff(icCLIType) THEN 
      ASSIGN lcDiscList       = {&ADDLINE_DISCOUNTS_20} + "," + {&ADDLINE_DISCOUNTS}
             llgMainLineAvail = TRUE
             illgConvOrder    = TRUE.
   ELSE IF LOOKUP(icCLIType,{&ADDLINE_CLITYPES}) > 0 THEN DO: 
      ASSIGN lcDiscList       = {&ADDLINE_DISCOUNTS_HM}
             llgMainLineAvail = TRUE.
             
      IF icAction EQ "CLOSE" THEN        
         liMOOrderId = fCheckMainLineForMobileOnly(icCustIDType,
                                                   icCustID). 
   END.

   IF NOT llgMainLineAvail THEN
      RETURN FALSE.

   FOR EACH labOrderCustomer NO-LOCK WHERE
            labOrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            labOrderCustomer.CustId     EQ icCustID                AND
            labOrderCustomer.CustIdType EQ icCustIDType            AND
            labOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH labOrder NO-LOCK WHERE
            labOrder.Brand      EQ Syst.Var:gcBrand           AND
            labOrder.OrderId    EQ labOrderCustomer.OrderId          AND
            labOrder.Statuscode EQ {&ORDER_STATUS_PENDING_MAIN_LINE} AND
            LOOKUP(labOrder.CLIType,{&ADDLINE_CLITYPES}) > 0:

      IF illgProMigration THEN
         lcNewOrderStatus = {&ORDER_STATUS_CLOSED}.
      ELSE DO:
         /* This record has to be found, to check orderaction has any additional 
            line related discount available - and - this record is used if action is CLOSE */ 
         FIND FIRST labOrderAction EXCLUSIVE-LOCK WHERE
                    labOrderAction.Brand    = Syst.Var:gcBrand           AND
                    labOrderAction.OrderID  = labOrder.OrderId  AND
                    labOrderAction.ItemType = "AddLineDiscount" AND
             LOOKUP(labOrderAction.ItemKey, lcDiscList) > 0     NO-ERROR.

         IF NOT AVAIL labOrderAction THEN NEXT.

         CASE icAction:
            WHEN "CLOSE" THEN DO:

               IF illgConvOrder                                                         AND
                  (NOT fCheckOngoingConvergentOrder(labOrderCustomer.CustIdType,
                                                    labOrderCustomer.CustID,
                                                    labOrder.CliType)
                   AND
                   NOT fCheckFixedLineStatusForMainLine(labOrderCustomer.CustIdType,
                                                        labOrderCustomer.CustId,
                                                        labOrder.CLIType)
                   AND
                   NOT fCheckExistingConvergent(labOrderCustomer.CustIdType,
                                                labOrderCustomer.CustID,
                                                labOrder.CliType)                  
                   AND                                     
                   (CAN-FIND(FIRST labOrderAction NO-LOCK WHERE
                                   labOrderAction.Brand    = Syst.Var:gcBrand           AND
                                   labOrderAction.OrderID  = labOrder.OrderId  AND
                                   labOrderAction.ItemType = "AddLineDiscount" AND
                            LOOKUP(labOrderAction.ItemKey, {&ADDLINE_DISCOUNTS}) > 0)))  OR 
                  (NOT fCheckOngoing2PConvergentOrder(labOrderCustomer.CustIdType,
                                                      labOrderCustomer.CustID,
                                                      labOrder.CliType)
                   AND                               
                   NOT fCheckFixedLineStatusForMainLine(labOrderCustomer.CustIdType,
                                                        labOrderCustomer.CustId,
                                                        labOrder.CLIType)
                   AND
                   NOT fCheckExisting2PConvergent(labOrderCustomer.CustIdType,
                                                  labOrderCustomer.CustID,
                                                  labOrder.CliType)
                   AND
                   (CAN-FIND(FIRST labOrderAction NO-LOCK WHERE
                                   labOrderAction.Brand    = Syst.Var:gcBrand           AND
                                   labOrderAction.OrderID  = labOrder.OrderId  AND
                                   labOrderAction.ItemType = "AddLineDiscount" AND
                            LOOKUP(labOrderAction.ItemKey, {&ADDLINE_DISCOUNTS_20}) > 0))) THEN   
                  llgDeleteAction = TRUE.               
               ELSE DO: 
                  /* If Mainline is available then delete its particular additional discount 
                     orderaction record, if available any other additional orderaction 
                     records then it will be ignored */
                  IF labOrder.OrderId = liMOOrderId THEN 
                     llgDeleteAction = TRUE.   
                  ELSE IF liMOOrderId EQ 0                                       
                       AND
                       NOT fCheckOngoingMobileOnly(labOrderCustomer.CustIdType,
                                                   labOrderCustomer.CustID,
                                                   labOrder.CliType)             
                       AND
                       NOT fCheckExistingMobileOnly(labOrderCustomer.CustIdType,
                                                    labOrderCustomer.CustID,
                                                    labOrder.CliType) 
                       AND 
                       (CAN-FIND(FIRST labOrderAction NO-LOCK WHERE
                                       labOrderAction.Brand    = Syst.Var:gcBrand           AND
                                       labOrderAction.OrderID  = labOrder.OrderId  AND
                                       labOrderAction.ItemType = "AddLineDiscount" AND
                                       LOOKUP(labOrderAction.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0)) THEN
                       llgDeleteAction = TRUE.
               END.   

               IF llgDeleteAction THEN DO:
                  DELETE labOrderAction.
                  Func.Common:mWriteMemo("Order",
                                   STRING(labOrder.OrderID),
                                   0,
                                   "ADDLINE DISCOUNT ORDERACTION REMOVED",
                                   "Removed AddLineDiscount Item from OrderAction").
               END.

            END.
            OTHERWISE .
         END CASE.

         CASE labOrder.OrderType:
            WHEN {&ORDER_TYPE_NEW} THEN lcNewOrderStatus = {&ORDER_STATUS_NEW}.
            WHEN {&ORDER_TYPE_MNP} THEN lcNewOrderStatus = {&ORDER_STATUS_MNP}.
            OTHERWISE.
         END CASE.

         IF icAction     EQ     "RELEASE"                              AND 
            labOrder.ICC EQ     ""                                     AND  
            icCLIType    BEGINS "CONTFH"                               AND
            LOOKUP(labOrder.OrderChannel,{&ORDER_CHANNEL_DIRECT}) GT 0 THEN DO:

            IF labOrder.DeliverySecure > 0 THEN 
               lcNewOrderStatus = {&ORDER_STATUS_SENDING_TO_LO}.
            ELSE lcNewOrderStatus = {&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}. 

         END.

      END.

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
   
   RETURN TRUE.

END FUNCTION.   

FUNCTION fCreateNewTPService RETURNS INTEGER
 (iiMsSeq       AS INT,
  icProduct     AS CHAR,
  icProvider    AS CHAR,
  icType        AS CHAR,
  icOperation   AS CHAR,
  icStatus      AS CHAR,
  icOffer       AS CHAR,
  icUser        AS CHAR):

    DEFINE BUFFER bf_TPService_Activation FOR TPService.

    FIND FIRST TPService WHERE TPService.MsSeq       = iiMsSeq           AND 
                               TPService.Operation   = icOperation       AND  
                               TPService.ServType    = icType            AND  
                               TPService.ServStatus <> {&STATUS_HANDLED} AND 
                               TPService.Product     = icProduct         NO-LOCK NO-ERROR.
    IF NOT AVAIL TPService THEN 
    DO:
        CREATE TPService.
        ASSIGN
            TPService.MsSeq      = iiMsSeq
            TPService.ServSeq    = NEXT-VALUE(TPServiceSeq)
            TPService.ServType   = icType
            TPService.Operation  = icOperation
            TPService.Product    = icProduct
            TPService.Provider   = icProvider
            TPService.ServStatus = icStatus
            TPService.Offer      = icOffer
            TPService.UserCode   = icUser
            TPService.CreatedTS  = Func.Common:mMakeTS()
            TPService.UpdateTS   = TPService.CreatedTS.

        IF icOperation = {&TYPE_DEACTIVATION} THEN 
        DO:
            FIND FIRST bf_TPService_Activation WHERE bf_TPService_Activation.MsSeq      = iiMsSeq            AND 
                                                     bf_TPService_Activation.Operation  = {&TYPE_ACTIVATION} AND 
                                                     bf_TPService_Activation.ServType   = icType             AND  
                                                     bf_TPService_Activation.ServStatus > ""                 AND
                                                     bf_TPService_Activation.Product    = icProduct          NO-LOCK NO-ERROR.
            IF AVAIL bf_TPService_Activation THEN 
                ASSIGN TPService.SerialNbr = bf_TPService_Activation.SerialNbr.                                                   
        END.                                                     
    END.

    RETURN TPService.ServSeq.   

END FUNCTION.

FUNCTION fCreateTPServiceMessage RETURNS LOGICAL
 (iiMsSeq       AS INT,
  iiServSeq     AS INT,
  icSource      AS CHAR,
  icStatus      AS CHAR):

    FIND FIRST TPService WHERE TPService.MsSeq = iiMsSeq AND TPService.ServSeq = iiServSeq EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF LOCKED TPService THEN 
        RETURN FALSE.

    CREATE TPServiceMessage.
    ASSIGN
       TPServiceMessage.MsSeq         = iiMsSeq
       TPServiceMessage.ServSeq       = iiServSeq
       TPServiceMessage.MessageSeq    = NEXT-VALUE(TPServiceMessageSeq)
       TPServiceMessage.Source        = icSource
       TPServiceMessage.MessageStatus = icStatus
       TPServiceMessage.CreatedTS     = Func.Common:mMakeTS()
       TPServiceMessage.UpdateTS      = TPServiceMessage.CreatedTS.

    ASSIGN   
       TPService.ServStatus           = icStatus
       TPService.UpdateTS             = Func.Common:mMakeTS().

    RETURN TRUE.   

END FUNCTION.

FUNCTION fActionOnExtraLineOrders RETURN LOGICAL
   (INPUT iiMainLineOrderId  AS INT,
    INPUT icAction           AS CHAR):

   DEFINE BUFFER lbMLOrder         FOR Order.
   DEFINE BUFFER lbELOrder         FOR Order.
   DEFINE BUFFER lbEMLOrder        FOR Order.
   DEFINE BUFFER lbELOrderAction   FOR OrderAction.
   DEFINE BUFFER lbMLOrderCustomer FOR OrderCustomer.

   DEF VAR lcNewOrderStatus AS CHAR NO-UNDO. 
   DEF VAR liMLMsSeq        AS INT  NO-UNDO INITIAL 0. 
   DEF VAR liMLOrderId      AS INT  NO-UNDO.  

   FOR EACH lbELOrder EXCLUSIVE-LOCK  WHERE
            lbELOrder.Brand        EQ Syst.Var:gcBrand                  AND
            lbELOrder.StatusCode   EQ {&ORDER_STATUS_PENDING_MAIN_LINE} AND 
            lbELOrder.MultiSimId   EQ iiMainLineOrderId                 AND 
            lbELOrder.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}: 

      IF llDoEvent THEN DO:
         lhOrderStatusChange = BUFFER lbELOrder:HANDLE.
         RUN StarEventInitialize(lhOrderStatusChange).
         RUN StarEventSetOldBuffer(lhOrderStatusChange).
      END.
     
      CASE icAction:
         WHEN "RELEASE" THEN DO:
         
            FIND FIRST lbMLOrder NO-LOCK WHERE 
                       lbMLOrder.Brand        EQ Syst.Var:gcBrand         AND
                       lbMLOrder.OrderId      EQ iiMainLineOrderId        AND 
                LOOKUP(lbMLOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) EQ 0 NO-ERROR. 

            lcNewOrderStatus = fGetReleaseStatus(lbELOrder.OrderType).

            IF AVAIL lbMLOrder                                            AND 
                     lbMLOrder.CLIType BEGINS "CONTFH"                    AND
                     lbELOrder.ICC     EQ     ""                          AND 
              LOOKUP(lbELOrder.OrderChannel,{&ORDER_CHANNEL_DIRECT}) GT 0 THEN DO:
            
               IF lbELOrder.DeliverySecure > 0 THEN 
                  lcNewOrderStatus = {&ORDER_STATUS_SENDING_TO_LO}.
               ELSE lcNewOrderStatus = {&ORDER_STATUS_PENDING_ICC_FROM_INSTALLER}. 

            END.
            
            fSetOrderStatus(lbELOrder.OrderId,lcNewOrderStatus).

         END.                      
         WHEN "CLOSE" THEN DO:
            /* if Main line order is closed, check for existing/ongoing mainline 
               available for same customer.  
               1. if available then reassign the order and release it 
               2. else close associated extraline ongoing order */
            FIND FIRST lbMLOrder NO-LOCK WHERE 
                       lbMLOrder.Brand        EQ Syst.Var:gcBrand        AND
                       lbMLOrder.OrderId      EQ iiMainLineOrderId       AND 
                       lbMLOrder.StatusCode   EQ {&ORDER_STATUS_CLOSED}  NO-ERROR. 

            IF AVAIL lbMLOrder THEN DO:
              
               liMLOrderId = 0.

               FIND FIRST lbMLOrderCustomer NO-LOCK WHERE
                          lbMLOrderCustomer.Brand   EQ Syst.Var:gcBrand                   AND
                          lbMLOrderCustomer.OrderId EQ lbMLOrder.OrderId                  AND
                          lbMLOrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.
 
               IF AVAIL lbMLOrderCustomer THEN DO:
                  liMLOrderId = fCheckExistingMainLineAvailForExtraLine(lbELOrder.CLIType,
                                                                        lbMLOrderCustomer.CustIdType,
                                                                        lbMLOrderCustomer.CustID,
                                                                        OUTPUT liMLMsSeq).
                  IF liMLOrderID > 0 THEN DO: 
                     lbELOrder.MultiSimId = liMLOrderId.
                     
                     /* If we reassign the extraline to exisiting Mainline of a 
                        customer then release it immediately */
                     lcNewOrderStatus = fGetReleaseStatus(lbELOrder.OrderType).

                     fSetOrderStatus(lbELOrder.OrderId,lcNewOrderStatus).

                  END.   
                  ELSE DO:
                     liMLOrderId = fCheckOngoingMainLineAvailForExtraLine(lbELOrder.CLIType,
                                                                          lbMLOrderCustomer.CustIdType,
                                                                          lbMLOrderCustomer.CustID).
                     IF liMLOrderID > 0 THEN 
                        lbELOrder.MultiSimId = liMLOrderId.
                  END.
               END.   
               
               IF liMLOrderID EQ 0 THEN DO:
                  FIND FIRST lbELOrderAction EXCLUSIVE-LOCK WHERE
                             lbELOrderAction.Brand    EQ Syst.Var:gcBrand           AND
                             lbELOrderAction.OrderID  EQ lbELOrder.OrderID          AND
                             lbELOrderAction.ItemType EQ "ExtraLineDiscount"        AND
                             lbELOrderAction.ItemKey  EQ lbELOrder.CLIType + "DISC" NO-ERROR.

                  IF AVAIL lbELOrderAction THEN DO:
                     DELETE lbELOrderAction.
                     Func.Common:mWriteMemo("Order",
                                            STRING(lbELOrder.OrderID),
                                            0,
                                            "EXTRALINE DISCOUNT ORDERACTION REMOVED",
                                            "Removed ExtraLineDiscount Item from OrderAction").
                  END.
               END.

            END.

         END. 
      END CASE.
   
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrderStatusChange).
         fCleanEventObjects().
      END.
   END.      

   RETURN TRUE.

END FUNCTION.   

FUNCTION fGetRegionDiscountPlan RETURNS CHARACTER
  (INPUT icRegion AS CHAR):

  DEF VAR lcDiscountPlan AS CHAR NO-UNDO.

  FIND FIRST Region WHERE Region.Region = icRegion NO-LOCK NO-ERROR.
  IF AVAIL Region THEN 
  DO:
      CASE Region.TaxZone:
          WHEN "1" THEN
              ASSIGN lcDiscountPlan = fCParam("TVService","Mainland").
          WHEN "2" THEN 
              ASSIGN lcDiscountPlan = fCParam("TVService","CanaryIslands").
          WHEN "3" THEN 
              ASSIGN lcDiscountPlan = fCParam("TVService","Ceuta").
          WHEN "4" THEN 
              ASSIGN lcDiscountPlan = fCParam("TVService","Melilla").
      END CASE.
  END.

  RETURN lcDiscountPlan.

END FUNCTION.  

FUNCTION fDeactivateTVService RETURNS LOGICAL
  (iiMsSeq      AS INTE,
   icUser       AS CHAR):

  DEFINE VARIABLE liServSeq              AS INTEGER   NO-UNDO.
  DEFINE VARIABLE liActivationServSeq    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lcActivationServStatus AS CHARACTER NO-UNDO.

  FIND FIRST TPService WHERE TPService.MsSeq       = iiMsSeq            AND 
                             TPService.Operation   = {&TYPE_ACTIVATION} AND 
                             TPService.ServType    = "Television"       AND 
                             TPService.ServStatus <> {&STATUS_CANCELED} AND 
                             TPService.ServStatus <> {&STATUS_ERROR}    NO-LOCK NO-ERROR.
  IF AVAIL TPService THEN
  DO:
      ASSIGN 
          liActivationServSeq    = TPService.ServSeq
          lcActivationServStatus = TPService.ServStatus.

      IF LOOKUP(TPService.ServStatus, {&WAITING_FOR_STB_ACTIVATION_CONFIRMATION} + "," + {&STATUS_HANDLED}) > 0 THEN 
      DO:
          ASSIGN liServSeq = fCreateNewTPService(iiMsSeq, 
                                                 TPService.Product, 
                                                 "Huawei", 
                                                 "Television", 
                                                 {&TYPE_DEACTIVATION}, 
                                                 {&STATUS_NEW}, 
                                                 "",      /* OfferId */ 
                                                 icUser). /* UserCode */ 

          IF liServSeq > 0 THEN 
          DO:
              fCreateTPServiceMessage(iiMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_NEW}).

              fCreateTPServiceMessage(iiMsSeq, liServSeq , {&SOURCE_TMS}, {&WAITING_FOR_STB_DEACTIVATION}).

              /* Cancelling the ongoing activation */  
              IF lcActivationServStatus = {&WAITING_FOR_STB_ACTIVATION_CONFIRMATION} THEN 
                  fCreateTPServiceMessage(iiMsSeq, liActivationServSeq, {&SOURCE_TMS}, {&STATUS_CANCELED}).
          END.    
      END.
      ELSE
      DO: /* NEW, Logistics_initiated, WAITING_FOR_STB_ACTIVATION */
          ASSIGN liServSeq = fCreateNewTPService(iiMsSeq, 
                                                 TPService.Product, 
                                                 "Huawei", 
                                                 "Television", 
                                                 {&TYPE_DEACTIVATION}, 
                                                 {&STATUS_NEW}, 
                                                 "",       /* OfferId */ 
                                                 icUser).  /* UserCode */ 

          IF liServSeq > 0 THEN 
          DO: 
              fCreateTPServiceMessage(iiMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_NEW}).

              fCreateTPServiceMessage(iiMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_HANDLED}).

              /* Cancelling the ongoing activation */  
              fCreateTPServiceMessage(iiMsSeq, liActivationServSeq, {&SOURCE_TMS}, {&STATUS_CANCELED}).
          END.                                  
      END.
  END.
      
  RETURN TRUE.

END FUNCTION.  

FUNCTION fIsTerminalOrder RETURNS LOG
   (INPUT liOrderId       AS INT,
    OUTPUT ocTerminalCode AS CHAR):

   DEFINE BUFFER bTermOrder FOR Order.

   FIND FIRST bTermOrder NO-LOCK WHERE
              bTermOrder.Brand   = Syst.Var:gcBrand AND
              bTermOrder.OrderId = liOrderId        NO-ERROR.

   IF NOT AVAIL bTermOrder THEN RETURN FALSE.

   /* Prepaid Order */
   IF bTermOrder.PayType = TRUE THEN RETURN FALSE.

   /* Terminal Financing in direct channel deployment on 09.07.2014 8:00 CET */
   IF bTermOrder.CrStamp < 20140709.28800 THEN RETURN FALSE.

   /* Check Terminal billcode */
   FOR EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand       = Syst.Var:gcBrand   AND
            OfferItem.Offer       = bTermOrder.Offer   AND
            OfferItem.BeginStamp <= bTermOrder.CrStamp AND
            OfferItem.EndStamp   >= bTermOrder.CrStamp AND
            OfferItem.ItemType    = "BillItem",
      FIRST BillItem NO-LOCK WHERE
            BillItem.Brand    = Syst.Var:gcBrand AND
            BillItem.BillCode = OfferItem.ItemKey,
      FIRST BitemGroup NO-LOCK WHERE
            BitemGroup.Brand   = Syst.Var:gcBrand AND
            BitemGroup.BIGroup = BillItem.BIGroup AND
            BItemGroup.BIGroup EQ "7":

      /* Exclude discount billing item on terminal */
      IF BillItem.BillCode BEGINS "CPDISC" THEN NEXT.

      ocTerminalCode = BillItem.BillCode.
      RETURN TRUE.
   END.

   RETURN FALSE.
END FUNCTION.

&ENDIF.


