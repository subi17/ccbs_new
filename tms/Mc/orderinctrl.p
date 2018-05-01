/* orderinctrl.p    

   changed:         22.11.06/aam fMarkOrderStamp, ask verification
*/
   
{Syst/commali.i}
{Syst/eventval.i}
{Func/forderstamp.i}
{Func/orderchk.i}
{Func/orderfunc.i}
{Mc/orderfusion.i}
{Func/profunc.i}
{Func/main_add_lines.i}

DEF INPUT PARAMETER iiOrder        AS INT NO-UNDO.
DEF INPUT PARAMETER iiSecureOption AS INT NO-UNDO.
DEF INPUT PARAMETER ilSilent       AS LOG NO-UNDO.

DEF VAR llOk                    AS LOG  NO-UNDO.
DEF VAR lcCampaignType          AS CHAR NO-UNDO.
DEF VAR lcRenoveSMSText         AS CHAR NO-UNDO. 
DEF VAR ldeSMSStamp             AS DEC  NO-UNDO. 
DEF VAR lcOldStatus             AS CHAR NO-UNDO. 
DEF VAR lcNewStatus             AS CHAR NO-UNDO. 
DEF VAR lcError                 AS CHAR NO-UNDO.
DEF VAR llCompanyScoringNeeded  AS LOG  NO-UNDO. 
DEF VAR liRequest               AS INT  NO-UNDO.
DEF VAR lcExtraMainLineCLITypes AS CHAR NO-UNDO. 

DEF BUFFER lbOrder          FOR Order.

FIND FIRST Order WHERE 
           Order.Brand   = Syst.Var:gcBrand AND 
           Order.OrderID = iiOrder NO-LOCK NO-ERROR.

IF not avail order THEN DO:
    MESSAGE
    "Unknown order ID " iiorder
    VIEW-aS ALERT-BOX.
    RETURN "Unknown order ID " + STRING(iiorder).
END.

IF NOT ilSilent THEN DO:
   
   llOk = FALSE.

   MESSAGE "Do You want to release order" +
      (IF iiSecureOption EQ 1
       THEN " (with secure Correos)"
       ELSE IF iiSecureOption EQ 2
       THEN " (with secure POS)"
       ELSE "") + "?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN DO:
      RETURN "".
   END.
END.

FIND CURRENT Order EXCLUSIVE-LOCK.
IF NOT ilSilent AND CURRENT-CHANGED Order THEN DO:
   
   MESSAGE "Order status was changed by other process. Try again"
   VIEW-AS ALERT-BOX ERROR.
   
   RETURN "".
END.

IF NOT ilSilent AND
   Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN DO:

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand   = Order.Brand AND
              OrderFusion.OrderId = Order.OrderID NO-ERROR.
   IF AVAIL OrderFusion AND 
            OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_FINALIZED} THEN DO:
      MESSAGE "Not allowed: Ongoing fixed line installation"
      VIEW-AS ALERT-BOX ERROR.
      
      RETURN "".
   END.
END.

lcOldStatus = Order.StatusCode.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder   AS HANDLE NO-UNDO.
   DEFINE VARIABLE lh17Order AS HANDLE NO-UNDO.

   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               
      
FIND OrderCustomer WHERE
     OrderCustomer.Brand = Syst.Var:gcBrand AND
     OrderCustomer.OrderId = Order.OrderId AND
     OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

IF AVAIL OrderCustomer THEN
   llCompanyScoringNeeded = 
      (Order.CREventQty = 0 AND 
      Order.CredOk = FALSE AND
      OrderCustomer.CustidType = "CIF").

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

IF Order.StatusCode EQ {&ORDER_STATUS_OFFER_SENT} THEN DO: /* shouldn't never get this value because of YDR-2575 */
   
   IF Order.RoiResult EQ "risk" THEN DO:

      fSetOrderStatus(Order.OrderId, STRING(40 + Order.RoiLevel)). 
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.

      RETURN "".
   END.

   IF Order.OrderChannel BEGINS "fusion" AND
      OrderCustomer.CustidType = "CIF" THEN DO:

      FIND FIRST Customer WHERE
         Customer.Brand      = Order.Brand          AND 
         Customer.OrgId      = OrderCustomer.CustId AND
         Customer.CustIdType = OrderCustomer.CustIdType AND
         Customer.Roles NE "inactive" NO-LOCK NO-ERROR. 
      IF AVAIL Customer THEN DO:
         FIND FIRST MobSub WHERE
                    MobSub.Brand   = Syst.Var:gcBrand AND
                    MobSub.AgrCust = Customer.CustNum
              NO-LOCK NO-ERROR.
         IF NOT AVAIL MobSub THEN lcNewStatus = "20".
         ELSE lcNewStatus = "21".
      END. /* IF AVAIL Customer THEN DO: */
      ELSE lcNewStatus = "20".

      fSetOrderStatus(Order.OrderId,lcNewStatus).
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.
      
      RETURN "".
   END.
END.

/*YPR-5316 AC2: release from ROI Queue if there is MNP out ongoint*/
IF (Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1}  OR
    Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2}  OR
    Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3}  OR 
    Order.StatusCode EQ {&ORDER_STATUS_MORE_DOC_NEEDED} OR
    Order.StatusCode EQ {&ORDER_STATUS_OFFER_SENT})  AND /* shouldn't never get this value because of YDR-2575 */
    Order.OrderType  EQ {&ORDER_TYPE_STC}           AND
    Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT Order.CLI) EQ TRUE THEN DO:

   IF iiSecureOption > 0 THEN Order.DeliverySecure = iiSecureOption.
   fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_MNP_RETENTION}).

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhOrder).
      fCleanEventObjects().
   END.

   RETURN "".

END.

/*YPR-5316 AC3: release convergent STC order from "75 - MNP retention" status when MNP out-porting request is cancelled*/
IF ((Order.StatusCode EQ {&ORDER_STATUS_MNP_RETENTION} AND
     Order.OrderType  EQ {&ORDER_TYPE_STC}) OR
     /*YTS-6045*/
     Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1}  OR
     Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2}  OR
     Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3}  OR
     Order.StatusCode EQ {&ORDER_STATUS_MORE_DOC_NEEDED} OR
     Order.StatusCode EQ {&ORDER_STATUS_OFFER_SENT})  AND /* shouldn't never get this value because of YDR-2575 */
     Order.OrderChannel BEGINS "fusion" AND
     Order.OrderType NE 2 AND
     NOT llCompanyScoringNeeded THEN DO:

   IF iiSecureOption > 0 THEN Order.DeliverySecure = iiSecureOption.
   fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_FIXED_LINE}).

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand   = Order.Brand AND
              OrderFusion.OrderId = Order.OrderID NO-ERROR.
   IF AVAIL OrderFusion AND
            OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_NEW} THEN DO:

      IF OrderFusion.FixedNumber EQ "" THEN
         fCreateFusionReserveNumberMessage(Order.OrderID,
                                           OUTPUT lcError).
      ELSE fCreateFusionCreateOrderMessage(Order.OrderID,
                                           OUTPUT lcError).

      IF lcError NE "" THEN 
         Func.Common:mWriteMemo("Order",
                          STRING(Order.OrderID),
                          0,
                          "Masmovil message creation failed",
                          lcError).
   END.
      
   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhOrder).
      fCleanEventObjects().
   END.

   RETURN "".
END.

/* YTS-4631 */
IF (Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1} OR
    Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2} OR
    Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3} OR
    Order.StatusCode EQ {&ORDER_STATUS_MORE_DOC_NEEDED} OR
    Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE}) AND
   CAN-FIND(lbOrder NO-LOCK WHERE
            lbOrder.Brand = Syst.Var:gcBrand AND
            lbOrder.CLI = Order.CLI AND
     LOOKUP(lbOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0 AND
            lbOrder.OrderID NE Order.Orderid) THEN DO:

   IF iiSecureOption > 0 THEN Order.DeliverySecure = iiSecureOption.
   fSetOrderStatus(Order.OrderId,"4").

   Func.Common:mWriteMemo("Order",
                    STRING(Order.OrderID),
                    0,
                    "Order exists with same MSISDN",
                    SUBST("Orderid: &1", Order.orderid)).

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhOrder).
      fCleanEventObjects().
   END.

   RETURN "".

END.
   
/* Assign order que according to MNP status */
IF Order.MNPStatus = 0 THEN lcNewStatus = "1". 
ELSE lcNewStatus = "3".
      
IF Order.MultiSIMId > 0 AND
   Order.MultiSIMType = {&MULTISIMTYPE_SECONDARY} THEN DO:

   FIND FIRST lbOrder NO-LOCK WHERE
              lbOrder.Brand = Syst.Var:gcBrand AND
              lbOrder.MultiSIMId = Order.MultiSIMId AND
              lbOrder.MultiSImType = {&MULTISIMTYPE_PRIMARY} NO-ERROR.
   IF AVAIL lbOrder AND
            lbOrder.StatusCode NE {&ORDER_STATUS_DELIVERED} THEN
      lcNewStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
END.
/* YTS-5296 */
ELSE IF Order.Ordertype < 2 AND
   lcOldStatus NE {&ORDER_STATUS_PENDING_MAIN_LINE} AND
   
   CAN-FIND(FIRST CLIType NO-LOCK WHERE
                  CLIType.Brand       = Syst.Var:gcBrand       AND
                  CLIType.CLIType     = Order.CLIType AND
                 (CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
                  CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL})) AND
   NOT CAN-FIND(FIRST OrderAction WHERE
                     OrderAction.Brand = Syst.Var:gcBrand AND
                     OrderAction.OrderId = Order.OrderID AND
                     OrderAction.ItemType = "BundleItem" AND
                CAN-FIND(FIRST CLIType NO-LOCK WHERE
                               CLIType.Brand = Syst.Var:gcBrand AND
                               CLIType.CLIType = OrderAction.ItemKey AND
                               CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}))
                            THEN DO:
      
   IF NOT fIsMainLineSubActive(
      OrderCustomer.CustIDType,
      OrderCustomer.CustId) THEN 
      lcNewStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
END.
/* special flow: 80 => 1/3 (fixed line creation) => 80 => 79 */
ELSE IF lcOldStatus EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} AND
   LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
   Order.OrderType NE {&ORDER_TYPE_STC} AND
   Order.ICC EQ "" AND
   CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq = Order.MsSeq AND
                  MsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE}) THEN DO:
   lcNewStatus = {&ORDER_STATUS_PENDING_MOBILE_LINE}.
END.
ELSE IF Order.OrderType = {&ORDER_TYPE_MNP} AND
   lcOldStatus NE {&ORDER_STATUS_PENDING_FIXED_LINE} AND
   /* special flow: 80 => 1/3 (fixed line creation) => 80 => 22 */
   NOT (lcOldStatus EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} AND
      NOT CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq = Order.MsSeq AND
                         MsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE})) AND
   lcOldStatus NE {&ORDER_STATUS_MNP_ON_HOLD} AND
   lcOldStatus NE {&ORDER_STATUS_SIM_ONLY_MNP_IN} AND  /* Prevents state change from 99 to 22 again */
   Order.PortingDate <> ? THEN
   lcNewStatus = {&ORDER_STATUS_MNP_ON_HOLD}.

lcCampaignType = ENTRY(3,Order.Campaign,";") NO-ERROR.
IF lcCampaignType = "newspaper" AND Order.OrderChannel EQ "cc" THEN DO:
   lcNewStatus = "23".
END.
   
/* Check if order must go to status 20 or status 21 (no renove CIF orders) */
IF lcOldStatus NE {&ORDER_STATUS_PENDING_MAIN_LINE} AND
   lcOldStatus NE {&ORDER_STATUS_MNP_ON_HOLD} AND
   lcOldStatus NE {&ORDER_STATUS_SIM_ONLY_MNP_IN} AND
   lcOldStatus NE {&ORDER_STATUS_PENDING_FIXED_LINE} AND
   lcOldStatus NE {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} AND
   lcOldStatus NE {&ORDER_STATUS_PENDING_MOBILE_LINE} AND
   Order.OrderType NE 2 AND
   llCompanyScoringNeeded THEN DO:
   
   FIND FIRST Customer WHERE
      Customer.Brand      = Order.Brand          AND 
      Customer.OrgId      = OrderCustomer.CustId AND
      Customer.CustIdType = OrderCustomer.CustIdType AND
      Customer.Roles NE "inactive" NO-LOCK NO-ERROR. 
   IF AVAIL Customer THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.Brand   = Syst.Var:gcBrand AND
                 MobSub.AgrCust = Customer.CustNum
           NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN lcNewStatus = "20".
      ELSE lcNewStatus = "21".
   END. /* IF AVAIL Customer THEN DO: */
   ELSE lcNewStatus = "20".
END.

/* MNP Retention Project */
IF Order.OrderChannel BEGINS "retention" AND
   Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT Order.CLI) THEN DO:
   lcNewStatus = {&ORDER_STATUS_MNP_RETENTION}.
END. /* IF Order.statuscode NE "4" AND */

/* order status queue must be i.e. 4 -> 22 -> 20 -> 1 */
ELSE IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN DO:
      
   FIND Customer NO-LOCK WHERE
        Customer.Custnum = Order.Custnum NO-ERROR.

   CASE Order.OrderChannel:
      WHEN "renewal" OR
      WHEN "renewal_telesales" OR
      WHEN "retention" OR
      WHEN "renewal_ctc" THEN DO:
         IF fCheckRenewalData(Order.OrderID) = TRUE OR 
            OrderCustomer.DataChecked NE ? THEN
            lcNewStatus =  {&ORDER_STATUS_RENEWAL}.
         ELSE DO:

            lcNewStatus = {&ORDER_STATUS_RENEWAL_HOLD}.
            
            /* YOT-1690 */
            lcRenoveSMSText = fGetSMSTxt(
                               "RenoveOnHold",
                               TODAY,
                               (IF AVAIL Customer
                                THEN Customer.Language
                                ELSE 1),
                                OUTPUT ldeSMSStamp).
         
            IF lcRenoveSMSText > "" THEN DO:
               lcRenoveSMSText = REPLACE(lcRenoveSMSText,"#CONTRACT_ID",STRING(Order.ContractID)).
               fMakeSchedSMS2(Order.CustNum,
                             Order.CLI,
                             {&SMSTYPE_AFTER_SALES_ORDER},
                             lcRenoveSMSText,
                             ldeSMSStamp,
                             "622",
                             "").
            END.
         END.
      END.
      WHEN "renewal_pos" THEN DO:
        /* Address is changed then customer address should be */
        /* changed without sending order into Renewal Queue   */
        lcNewStatus = {&ORDER_STATUS_RENEWAL}.
      END. /* WHEN "renewal_pos" THEN DO: */
      WHEN "renewal_pos_stc" OR WHEN "retention_stc" THEN DO:

         FIND FIRST MsRequest WHERE
                    MsRequest.MsSeq = Order.MsSeq AND
                    MsRequest.ReqType = 0 AND
         LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0
         NO-LOCK NO-ERROR.

         IF AVAIL MsRequest THEN DO:

            /* orderid was not propably set in rpc */
            IF NOT MsRequest.ReqIParam2 > 0 THEN DO:
               FIND CURRENT MsRequest EXCLUSIVE-LOCK.
               MsRequest.ReqIParam2 = Order.OrderId.
               RELEASE MsRequest.
            END.
         
            /* YDR-323 */
            lcNewStatus = (IF llCompanyScoringNeeded
                           THEN {&ORDER_STATUS_RENEWAL_STC_COMPANY}
                           ELSE {&ORDER_STATUS_RENEWAL_STC}).
         END.
         /* TODO: Does this make any sense */
         ELSE lcNewStatus = {&ORDER_STATUS_RENEWAL}.
      END.
   END.
END.   
IF iiSecureOption > 0 THEN Order.DeliverySecure = iiSecureOption.

fSetOrderStatus(Order.OrderId,lcNewStatus).

IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhOrder).
   fCleanEventObjects().
END.

/* Release pending additional lines, in case of pending convergent 
   or mobile main line order is released */
/* YTS-10832 fix, checking correct status of order */
IF LOOKUP(STRING(Order.OrderType),"0,1,3,4") > 0 AND
   (lcNewStatus = {&ORDER_STATUS_NEW}                 OR
    lcNewStatus = {&ORDER_STATUS_MNP}                 OR 
    lcNewStatus = {&ORDER_STATUS_PENDING_MOBILE_LINE}) THEN DO:
  
   fActionOnAdditionalLines (OrderCustomer.CustIdType,
                             OrderCustomer.CustID,
                             Order.CLIType,      
                             FALSE,
                            "RELEASE"). 
END.

RETURN "".
