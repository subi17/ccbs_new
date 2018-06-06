{Syst/commpaa.i}
Syst.Var:katun   = "Mock".
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/tmsparam4.i}
{Func/orderfunc.i}

DEF VAR liNewDelay AS INT    NO-UNDO. 
DEF VAR lhOrder    AS HANDLE NO-UNDO.
DEF VAR liRequest  AS INT    NO-UNDO. 
DEF VAR lcError    AS CHAR NO-UNDO. 

DEFINE BUFFER bOrder FOR Order. 

FUNCTION fCreateOrderGroup RETURNS LOGICAL
   (INPUT liOrderId        AS INT, 
    INPUT liMLOrderId      AS INT,
    INPUT llDespacharValue AS LOG):

   CREATE OrderGroup.
   ASSIGN OrderGroup.OrderId   = liOrderId
          OrderGroup.GroupId   = liMLOrderID
          OrderGroup.GroupType = {&OG_LOFILE}
          OrderGroup.Info      = IF llDespacharValue THEN
                                    "01" + CHR(255) + "OrderStatusChangedByMock"
                                 ELSE "02" + CHR(255) + "OrderStatusChangedByMock"
          OrderGroup.CrStamp   = Func.Common:mMakeTS().

   RETURN TRUE.

END FUNCTION.

liNewDelay = fCParamI4(Syst.Var:gcBrand, "Logistics","NewOrderDelay").

FOR EACH Order NO-LOCK WHERE 
         Order.Brand      EQ Syst.Var:gcBrand              AND 
        (Order.StatusCode EQ {&ORDER_STATUS_SENDING_TO_LO} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_ICC_FROM_LO}):

   IF Order.OrderType NE 0 AND
      Order.OrderType NE 1 THEN NEXT.
   
   IF Order.Orderchannel BEGINS "migration" THEN NEXT.

   /* YOT-867 */
   IF Order.MNPStatus EQ 0 AND 
      liNewDelay      NE ? AND
      Func.Common:mOffSet(Order.CrStamp, 24 * liNewDelay) > Func.Common:mMakeTS() THEN NEXT.

   IF Order.StatusCode EQ {&ORDER_STATUS_SENDING_TO_LO} THEN DO: 
      IF llDoEvent THEN DO:
         lhOrder = BUFFER Order:HANDLE.
         RUN StarEventInitialize(lhOrder).
         RUN StarEventSetOldBuffer(lhOrder).
      END.
     
         fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.

      FIND FIRST OrderGroup NO-LOCK WHERE
                 OrderGroup.OrderId        EQ Order.OrderID            AND
                 OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
         ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_FALSE_VALUE} NO-ERROR. 

      fCreateOrderGroup(Order.OrderId,
                        IF AVAIL OrderGroup THEN 
                           OrderGroup.GroupId
                        ELSE Order.OrderId,
                        TRUE).
   END.                     
   
   FIND FIRST SIM EXCLUSIVE-LOCK WHERE 
              SIM.Brand   EQ Syst.Var:gcBrand AND
              SIM.Stock   EQ {&ICC_STOCK_LO}  AND  
              SIM.SimArt  EQ "universal"      AND 
              SIM.SimStat EQ {&SIM_SIMSTAT_AVAILABLE} NO-ERROR.

   /* If already subscription is available then create 
      ICC change request */
   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.MsSeq EQ Order.MsSeq AND 
              MobSub.ICC   NE ""          NO-ERROR.

   IF AVAIL MobSub THEN DO:
      liRequest = fSubscriptionRequest(Mobsub.MsSeq,
                                       Mobsub.Cli,
                                       Mobsub.CustNum,
                                       1,                        /*tarifftype*/
                                       "",                       /*creator*/
                                       Func.Common:mMakeTS(),    /*ActStamp*/
                                       "CHANGEICC",              /*ReqParam*/
                                       SIM.ICC,                  /*ReqParam2*/
                                       "",                       /*old SIM*/
                                       "",                       /*Reason*/
                                       "",                       /*ContractID*/
                                       FALSE,                    /*CreateFees*/
                                       0,                        /*Charge*/
                                       {&SOURCE_LOGISTICS},      /*Request source*/
                                       OUTPUT lcError).          /*result*/
      IF liRequest EQ 0 THEN DO:
         Func.Common:mWriteMemo("MobSub",
                                STRING(MobSub.MsSeq),
                                MobSub.CustNum,
                                "ICC In Use",
                                SIM.ICC + " is already in use").
         NEXT.
      END.

      IF llDoEvent THEN DO:
         lhOrder = BUFFER Order:HANDLE.
         RUN StarEventInitialize(lhOrder).
         RUN StarEventSetOldBuffer(lhOrder).
      END.

      /* order status with 76 is not needed to be released, at it will 
         be released when associated main line fixed line is installed */
      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_ICC_FROM_LO} THEN DO:
         CASE Order.OrderType:
            WHEN {&ORDER_TYPE_NEW} THEN fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_NEW}).
            WHEN {&ORDER_TYPE_MNP} THEN fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_MNP}).
         END CASE.
      END.

      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.

   END.
   ELSE DO:

      IF Order.ICC        EQ ""                                    AND
        (Order.StatusCode EQ {&ORDER_STATUS_PENDING_ICC_FROM_LO} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE}  OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_MAIN_LINE})    AND 
         LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) EQ 0 THEN DO: 

         FIND bOrder EXCLUSIVE-LOCK WHERE 
              ROWID(bOrder) = ROWID(Order) NO-ERROR.

         IF ERROR-STATUS:ERROR OR LOCKED(bOrder) THEN RETURN.

         ASSIGN bOrder.ICC  = SIM.ICC 
                SIM.SimStat = 4
                SIM.MsSeq   = bOrder.MsSeq. 
      
         CREATE SimDeliveryhist.
         ASSIGN SimDeliveryHist.OrderID    = bOrder.OrderID
                SimDeliveryHist.MSSeq      = bOrder.MSSeq 
                SimDeliveryHist.StatusCode = 2.
                SimDeliveryHist.TimeStamp  = Func.Common:mMakeTS().
   
         Func.Common:mWriteMemo("Order",
                                STRING(bOrder.OrderID),
                                bOrder.CustNum,
                                "ICC value updated by LO",
                                "").
         
         IF llDoEvent THEN DO:
            lhOrder = BUFFER bOrder:HANDLE.
            RUN StarEventInitialize(lhOrder).
            RUN StarEventSetOldBuffer(lhOrder).
         END.

         /* order status with 76 is not needed to be released, at it will 
            be released when associated main line fixed line is installed */
         IF bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_ICC_FROM_LO} THEN DO:
            CASE bOrder.OrderType:
               WHEN {&ORDER_TYPE_NEW} THEN fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_NEW}).
               WHEN {&ORDER_TYPE_MNP} THEN fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_MNP}).
            END CASE.
         END.

         IF llDoEvent THEN DO:
            RUN StarEventMakeModifyEvent(lhOrder).
            fCleanEventObjects().
         END.

      END.
      ELSE DO:
         CREATE ErrorLog.
         ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
                ErrorLog.ActionID  = "ORDERICC"
                ErrorLog.TableName = "Order"
                ErrorLog.KeyValue  = STRING(Order.OrderId) 
                ErrorLog.ErrorMsg  = "ICC not assigned due to wrong order status"
                ErrorLog.UserCode  = Syst.Var:katun
                ErrorLog.ActionTS  = Func.Common:mMakeTS().
      END.

   END.

END.         
