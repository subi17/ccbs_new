/* closeorder.p    

   changed:         22.11.06/aam fMarkOrderStamp, ask verification
                    30.11.06/aam validations
                    01.02.07 kl  allow close when invoice is credited

*/
   
{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/forderstamp.i}
{Func/msreqfunc.i}
{Func/orderfunc.i}
{Syst/tmsconst.i}
{Func/fcreditreq.i}
{Func/ordercancel.i}
{Func/msisdn.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER ilSilent AS LOG NO-UNDO.

DEF VAR llOk             AS LOG       NO-UNDO.
DEF VAR lcError          AS CHARACTER NO-UNDO.
DEF VAR lcQuestion       AS CHARACTER NO-UNDO. 
DEF VAR lcCreditReason   AS CHARACTER NO-UNDO. 
DEF VAR lcOldOrderStatus AS CHARACTER NO-UNDO. 

/* Additional line mobile only ALFMO-5*/
DEF VAR lcAddlineCliypes AS CHARACTER NO-UNDO. 
DEF VAR llDelete         AS LOGICAL   NO-UNDO. 

DEFINE BUFFER lbOrderCustomer FOR OrderCustomer.
DEFINE BUFFER lbOrder         FOR Order.

/* Additional line mobile only ALFMO-5 */
DEFINE BUFFER lbOrdCust       FOR OrderCustomer.
DEFINE BUFFER lbOrd           FOR Order.
DEFINE BUFFER lbOrdAct        FOR OrderAction.

FIND Order WHERE 
     Order.Brand   = gcBrand AND 
     Order.OrderID = iiOrder EXCLUSIVE-LOCK NO-ERROR.

IF not avail order THEN DO:
   lcError = "Unknown order ID " + STRING(iiorder).
   IF NOT ilSilent THEN MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
   RETURN lcError.
END.

lcOldOrderStatus = Order.StatusCode.

IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES} + ",12") > 0 THEN DO:
   lcError = SUBST("Cannot close order with status &1",Order.StatusCode).
   IF NOT ilSilent THEN   
      MESSAGE lcError
      VIEW-AS ALERT-BOX INFORMATION.
   RETURN lcError.
END. 

IF NOT ilSilent AND
   Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN DO:

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand   = Order.Brand AND
              OrderFusion.OrderId = Order.OrderID NO-ERROR.
   IF AVAIL OrderFusion AND 
            OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_FINALIZED} AND
            OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_CANCELLED} THEN DO:

      MESSAGE "Not allowed: Ongoing fixed line installation"
      VIEW-AS ALERT-BOX ERROR.
      
      RETURN "".
   END.
END.

llOk = FALSE.
   
FOR EACH MNPProcess WHERE
         MNPProcess.OrderId = Order.OrderId AND
         MNPProcess.MNPType = {&MNP_TYPE_IN} NO-LOCK:
   
   IF MNPProcess.StatusCode NE {&MNP_ST_AREC} AND
      MNPProcess.StatusCode NE {&MNP_ST_AREC_CLOSED} AND
      MNPProcess.StatusCode NE {&MNP_ST_ACAN} AND
      MNPProcess.StatusCode NE {&MNP_ST_APOR} THEN DO:
      
      lcError = "Cannot close order. Ongoing MNP process".
      IF NOT ilSilent THEN   
         MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
      RETURN lcError.
   END.
END.
   
IF NOT ilSilent THEN DO:   
   
   IF Order.Invnum > 0 THEN
      lcQuestion = "Are you sure you want to close the Order and Create credit note to the Cash invoice?".
   ELSE lcQuestion = "Do you want to mark this order closed?".

   MESSAGE lcQuestion
      VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN RETURN "".
END.

IF Order.InvNum > 0 THEN DO:
   
   IF Order.MNPStatus = 5 THEN lcCreditReason = "1012".
   ELSE IF Order.MNPStatus = 0 THEN lcCreditReason = "1010".
   ELSE DO:
      IF CAN-FIND(FIRST MNPProcess NO-LOCK WHERE
           MNPProcess.OrderId = Order.OrderId AND
           MNPProcess.StatusCode = {&MNP_ST_ACAN} AND
           MNPProcess.StatusReason = "CANC_ABONA") THEN
           lcCreditReason = "1010".
      ELSE lcCreditReason = "1011".
   END.
   
   lcError = fCashInvoiceCreditNote(
      Order.Invnum,
      lcCreditReason).

   IF lcError NE "" THEN DO:
      IF NOT ilSilent THEN MESSAGE lcError VIEW-AS ALERT-BOX.
      /* used in Vista and translated (do not change) */
      RETURN "Order cannot be closed because of unsuccessful Credit note request creation.". 
   END.

END.

/* ongoing 'renewal pos stc' must be cancelled */
IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN DO:

   FIND FIRST MsRequest WHERE 
              MsRequest.MsSeq = Order.Msseq AND
              MsRequest.ReqType = 0 AND
              LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0 NO-LOCK NO-ERROR.

   IF AVAIL MsRequest THEN DO:
      
      IF NOT ilSilent THEN DO:
         MESSAGE 
            "This will cancel ongoing subcription type change request\n"
            "Do you still want to continue?"
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         TITLE " ORDER " + STRING(Order.OrderID) + " "
         SET llOk.
         
         IF NOT llOk THEN RETURN "".
      END.

      IF fChkReqStatusChange(4) EQ FALSE AND
         NOT (MsRequest.ReqStatus EQ 3 AND
              MsRequest.Memo MATCHES "*Pending renewal order*") THEN DO:
         lcError = "Cannot cancel STC request. Order cancellation is not possible".
         IF NOT ilSilent THEN 
            MESSAGE lcError 
            VIEW-AS ALERT-BOX
            TITLE " ORDER " + STRING(Order.OrderID) + " ".
         RETURN lcError.
      END.
      ELSE fReqStatus(4,SUBST("Cancelled " + Order.OrderChannel +
                      " order &1", Order.OrderId)).
   END.
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
   RUN StarEventSetOldBuffer(lhOrder).
END.               

IF Order.OrderType EQ {&ORDER_TYPE_NEW} AND
  (Order.StatusCode EQ {&ORDER_STATUS_OFFER_SENT} OR /* shouldn't never get this value because of YDR-2575 */
   Order.StatusCode EQ {&ORDER_STATUS_RESIGNATION}) THEN DO:
   
   FIND FIRST MSISDN WHERE
              MSISDN.Brand = gcBrand AND
              MSISDN.CLI   = Order.CLI
   EXCLUSIVE-LOCK NO-ERROR.
   
   IF AVAIL MSISDN AND MSISDN.StatusCode EQ 2 THEN DO:
      fMakeMsidnHistory(INPUT RECID(MSISDN)).
      ASSIGN
         MSISDN.StatusCode = 4
         MSISDN.CustNum    = 0.
      RELEASE MSISDN.
   END.
   
   If Order.StatusCode EQ {&ORDER_STATUS_RESIGNATION} AND
      Order.ICC > "" THEN DO:

      FIND SIM EXCLUSIVE-LOCK WHERE
           SIM.ICC = Order.ICC AND
           SIM.SimStat = 20 AND 
           SIM.MsSeq = Order.Msseq NO-ERROR.
      IF AVAIL SIM THEN 
         SIM.SIMStat = {&SIM_SIMSTAT_AVAILABLE}.
      RELEASE SIM.
   END.
END.

fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_CLOSED}).

IF llDoEvent THEN
   RUN StarEventMakeModifyEvent(lhOrder).

/* ADDLINE-19 AC10 Additional Line */
IF lcOldOrderStatus EQ {&ORDER_STATUS_PENDING_FIXED_LINE}        OR
   lcOldOrderStatus EQ {&ORDER_STATUS_ROI_LEVEL_1}               OR
   lcOldOrderStatus EQ {&ORDER_STATUS_ROI_LEVEL_2}               OR
   lcOldOrderStatus EQ {&ORDER_STATUS_ROI_LEVEL_3}               OR
   lcOldOrderStatus EQ {&ORDER_STATUS_IN_CONTROL}                OR
   lcOldOrderStatus EQ {&ORDER_STATUS_MNP_REJECTED}              OR
   lcOldOrderStatus EQ {&ORDER_STATUS_MORE_DOC_NEEDED}           OR
   lcOldOrderStatus EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} OR
   lcOldOrderStatus EQ {&ORDER_STATUS_PENDING_MOBILE_LINE}       THEN
DO:
   FIND FIRST lbOrderCustomer NO-LOCK WHERE
              lbOrderCustomer.Brand   = gcBrand       AND
              lbOrderCustomer.OrderId = Order.OrderId AND
              lbOrderCustomer.RowType = 1             NO-ERROR.
   IF AVAILABLE lbOrderCustomer THEN DO:
      /* If Main Line is Closed and customer has no other main line then removing the additional line discount */
      FOR EACH OrderCustomer NO-LOCK WHERE
               OrderCustomer.Brand      = gcBrand                    AND
               OrderCustomer.CustIDType = lbOrderCustomer.CustIDType AND
               OrderCustomer.CustID     = lbOrderCustomer.CustID     AND
               OrderCustomer.RowType    = 1,
         FIRST lbOrder NO-LOCK WHERE
               lbOrder.Brand      = gcBrand                           AND
               lbOrder.OrderID    = OrderCustomer.OrderID             AND
               lbOrder.StatusCode = {&ORDER_STATUS_PENDING_MAIN_LINE} AND
        LOOKUP(lbOrder.CLIType, {&ADDLINE_CLITYPES}) > 0:

         FIND FIRST OrderAction EXCLUSIVE-LOCK WHERE
                    OrderAction.Brand    = gcBrand           AND
                    OrderAction.OrderID  = lbOrder.OrderID   AND
                    OrderAction.ItemType = "AddLineDiscount" AND
             LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS} + "," + {&ADDLINE_DISCOUNTS_20}) > 0 NO-ERROR.
         IF AVAILABLE OrderAction THEN DO:
            IF (LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS}) > 0    AND
                NOT fCheckOngoingConvergentOrder(lbOrderCustomer.CustIdType,
                                                 lbOrderCustomer.CustID,
                                                 lbOrder.CliType)          AND
                NOT fCheckExistingConvergent(lbOrderCustomer.CustIdType,
                                             lbOrderCustomer.CustID,
                                             lbOrder.CliType))             OR
               (LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_20}) > 0 AND
                NOT fCheckOngoing2PConvergentOrder(lbOrderCustomer.CustIdType,
                                                   lbOrderCustomer.CustID,
                                                   lbOrder.CliType)        AND
                NOT fCheckExisting2PConvergent(lbOrderCustomer.CustIdType,
                                               lbOrderCustomer.CustID,
                                               lbOrder.CliType))           THEN DO:
               DELETE OrderAction.
               DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                "Order",
                                STRING(lbOrder.OrderID),
                                0,
                                "ADDLINE DISCOUNT ORDERACTION REMOVED",
                                "Removed AddLineDiscount Item from OrderAction").

               fReleaseORCloseAdditionalLines(lbOrderCustomer.CustIdType,
                                              lbOrderCustomer.CustID).
            END.
         END.
      END.
   END.
END. 

/* Additional Line with mobile only ALFMO-5 
   If Mobile only Main Line is Closed and customer has no other mobile only 
   main line then remove the mobile only additional line discount */

FIND FIRST lbOrderCustomer NO-LOCK WHERE
           lbOrderCustomer.Brand   = gcBrand       AND
           lbOrderCustomer.OrderId = Order.OrderId AND
           lbOrderCustomer.RowType = 1             NO-ERROR.
IF AVAILABLE lbOrderCustomer THEN DO:

   /* Just to create the list of 
     additional line CLITYPES(76 status) 
     Additional line mobile only ALFMO-5 */

   FOR EACH lbOrdCust NO-LOCK WHERE
            lbOrdCust.Brand      = gcBrand                    AND
            lbOrdCust.CustIDType = lbOrderCustomer.CustIDType AND
            lbOrdCust.CustID     = lbOrderCustomer.CustID     AND
            lbOrdCust.RowType    = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
      FIRST lbOrd NO-LOCK WHERE
            lbOrd.Brand      = gcBrand                           AND
            lbOrd.OrderID    = lbOrdCust.OrderID                 AND
            lbOrd.StatusCode = {&ORDER_STATUS_PENDING_MAIN_LINE} AND
            LOOKUP(lbOrd.CLIType, {&ADDLINE_CLITYPES}) > 0:

      IF CAN-FIND( FIRST lbOrdAct NO-LOCK WHERE
                  lbOrdAct.Brand    = gcBrand           AND
                  lbOrdAct.OrderID  = lbOrd.OrderID   AND
                  lbOrdAct.ItemType = "AddLineDiscount" AND
           LOOKUP(lbOrdAct.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0 ) THEN
      DO:      
         IF lcAddlineCliypes = "" THEN
            ASSIGN lcAddlineCliypes = lbOrd.CLIType.
         ELSE
            ASSIGN lcAddlineCliypes = lcAddlineCliypes + "," + lbOrd.CLIType.
      END.
   END.


   /* If Main Line is Closed and customer has no other mobile only 
      main line then removing the additional line discount */
   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand      = gcBrand                    AND
            OrderCustomer.CustIDType = lbOrderCustomer.CustIDType AND
            OrderCustomer.CustID     = lbOrderCustomer.CustID     AND
            OrderCustomer.RowType    = 1,
      FIRST lbOrder NO-LOCK WHERE
            lbOrder.Brand      = gcBrand                           AND
            lbOrder.OrderID    = OrderCustomer.OrderID             AND
     LOOKUP(lbOrder.CLIType, {&ADDLINE_CLITYPES}) > 0:         
      FIND FIRST OrderAction EXCLUSIVE-LOCK WHERE
                 OrderAction.Brand    = gcBrand           AND
                 OrderAction.OrderID  = lbOrder.OrderID   AND
                 OrderAction.ItemType = "AddLineDiscount" AND
          LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0 NO-ERROR.
      IF AVAILABLE OrderAction THEN 
      DO:
         IF NOT fCheckOngoingMobileOnly(lbOrderCustomer.CustIdType,
                                        lbOrderCustomer.CustID,
                                        lbOrder.CliType) AND
            NOT fCheckExistingMobileOnly(lbOrderCustomer.CustIdType,
                                         lbOrderCustomer.CustID,
                                         lbOrder.CliType) THEN 
         DO:
            llDelete = TRUE.            
            IF NUM-ENTRIES(lcAddlineCliypes) > 1 THEN
            DO:
               IF LOOKUP(ENTRY(3,{&ADDLINE_CLITYPES}),lcAddlineCliypes) > 0 THEN
               DO:
                  IF lbOrder.CLIType <> ENTRY(3,{&ADDLINE_CLITYPES}) THEN
                     llDelete = FALSE.
               END.
               ELSE IF LOOKUP(ENTRY(4,{&ADDLINE_CLITYPES}),lcAddlineCliypes) > 0 THEN
               DO:
                  IF lbOrder.CLIType <> ENTRY(4,{&ADDLINE_CLITYPES}) THEN
                     llDelete = FALSE.
               END.
            END.

            IF llDelete THEN
            DO:            
               DELETE OrderAction.
               DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                "Order",
                                STRING(lbOrder.OrderID),
                                0,
                                "ADDLINE DISCOUNT ORDERACTION REMOVED",
                                "Removed AddLineDiscount Item from OrderAction").            
            END.

            IF lbOrder.StatusCode = {&ORDER_STATUS_PENDING_MAIN_LINE} THEN
            DO:
               fReleaseORCloseAdditionalLines(lbOrderCustomer.CustIdType,
                                              lbOrderCustomer.CustID).
            END.                                  
         END.
      END.
   END.
END.

FOR EACH MNPProcess WHERE
   MNPProcess.OrderID = Order.OrderId AND
   MNPProcess.MNPType = {&MNP_TYPE_IN} AND
   MNPProcess.StatusCode = {&MNP_ST_AREC} EXCLUSIVE-LOCK:
   ASSIGN
      MNPProcess.UpdateTS = fMakeTS()
      MNPProcess.StatusCode = {&MNP_ST_AREC_CLOSED}.
END.

/* YDR-16 and YDR-415 */
IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
   Order.ICC > "" AND
   Order.OrderType <= 1 THEN DO:

   FIND SIM WHERE
        SIM.ICC = Order.ICC AND
        SIM.SimStat = 4 NO-LOCK NO-ERROR.
   IF AVAIL SIM AND SIM.Stock = "RETAILER" THEN DO:
      IF LOOKUP(STRING(Order.MNPStatus),"1,5,8") > 0 OR
         Order.OrderType = 0 THEN
         fReleaseSIM(Order.OrderId).
   END. /* IF AVAIL SIM THEN DO: */
END.

/* Move the SIM Status to LOST if rollback MNP order is closed */
IF Order.OrderType = 3 THEN DO:
   FIND SIM WHERE
        SIM.ICC = Order.ICC AND
        SIM.SimStat = 4 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL SIM THEN SIM.SimStat = 7.
END. /* IF Order.OrderType = 3 THEN DO: */

/* Release SIM in case of YDR-1825 */
IF Order.OrderType = 1 AND
   LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0 AND
   Order.ICC > "" THEN DO:
   FIND SIM NO-LOCK WHERE
        SIM.ICC = Order.ICC AND
        SIM.SimStat = 20 NO-ERROR.
   IF AVAIL SIM THEN 
         fReleaseSIM(Order.OrderId).
END.

/* Release SIM if renewal order is made with ICC change */
IF Order.OrderType = 2 AND
   NOT Order.OrderChannel BEGINS "RENEWAL_POS" THEN DO:
   FIND SIM WHERE
        SIM.ICC = Order.ICC AND
        SIM.SimStat = 13 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL SIM THEN
      SIM.SIMStat = {&SIM_SIMSTAT_AVAILABLE}.
END. /* IF Order.OrderType = 2 AND */

fMarkOrderStamp(Order.OrderID,
                "Close",
                0.0).

IF llDoEvent THEN fCleanEventObjects().

RUN Mc/cancelorder.p(Order.OrderId,TRUE).

RETURN "".

