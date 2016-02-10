/* orderinctrl.p    

   changed:         22.11.06/aam fMarkOrderStamp, ask verification
*/
   
{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Func/forderstamp.i}
{Func/orderchk.i}
{Func/orderfunc.i}
{Mnp/mnpoutchk.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER iiSecureOption AS INT NO-UNDO.
DEF INPUT PARAMETER ilSilent AS LOG NO-UNDO.

DEF VAR llOk AS LOG NO-UNDO.
DEF VAR lcCampaignType AS CHARACTER NO-UNDO.
DEF VAR lcRenoveSMSText AS CHAR NO-UNDO. 
DEF VAR ldeSMSStamp AS DEC NO-UNDO. 
DEF VAR lcOldStatus AS CHAR NO-UNDO. 
DEF VAR lcNewStatus AS CHAR NO-UNDO. 

DEF BUFFER lbOrder FOR Order.

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand AND 
           Order.OrderID = iiOrder EXCLUSIVE-LOCK NO-ERROR.

IF not avail order THEN DO:
    MESSAGE
    "Unknown order ID " iiorder
    VIEW-aS ALERT-BOX.
    RETURN "Unknown order ID " + STRING(iiorder).
END.

IF NOT ilSilent THEN DO:
   
   llOk = FALSE.

   MESSAGE "Do You want to release order " + 
      (IF iiSecureOption EQ 1 THEN "(with secure)" ELSE "") + "?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN DO:
      RETURN "".
   END.
END.

lcOldStatus = Order.StatusCode.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
END.               

IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrder).

IF Order.StatusCode EQ {&ORDER_STATUS_OFFER_SENT} THEN DO:
   
   IF Order.RoiResult EQ "risk" THEN DO:
      fSetOrderStatus(Order.OrderId, STRING(40 + Order.RoiLevel)). 
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.

      RETURN "".
   END.

   IF Order.OrderChannel BEGINS "fusion" THEN DO:
      FIND FIRST OrderCustomer WHERE
         OrderCustomer.Brand = gcBrand AND
         OrderCustomer.OrderId = Order.OrderId AND
         OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

      IF OrderCustomer.CustidType = "CIF" THEN DO:
         FIND FIRST Customer WHERE
            Customer.Brand      = Order.Brand          AND 
            Customer.OrgId      = OrderCustomer.CustId AND
            Customer.CustIdType = OrderCustomer.CustIdType AND
            Customer.Roles NE "inactive" NO-LOCK NO-ERROR. 
         IF AVAIL Customer THEN DO:
            FIND FIRST MobSub WHERE
                       MobSub.Brand   = gcBrand AND
                       MobSub.AgrCust = Customer.CustNum
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL MobSub THEN lcNewStatus = "20".
            ELSE lcNewStatus = "21".
         END. /* IF AVAIL Customer THEN DO: */
         ELSE lcNewStatus = "20".
         fSetOrderStatus(Order.OrderId,lcNewStatus).
      END. /* IF OrderCustomer.CustidType = "CIF" THEN */
      ELSE fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_FIXED_LINE}).
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.
      
      RETURN "".
   END.
END.

/* YTS-6045 */
IF (Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1}  OR
    Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2}  OR
    Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3}  OR
    Order.StatusCode EQ {&ORDER_STATUS_MORE_DOC_NEEDED}) AND
    Order.OrderChannel BEGINS "fusion" THEN DO:

   fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_FIXED_LINE}).

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand   = Order.Brand AND
              OrderFusion.OrderId = Order.OrderID NO-ERROR.
   IF AVAIL OrderFusion AND
            OrderFusion.FusionStatus EQ "" THEN DO:

      FIND CURRENT OrderFusion EXCLUSIVE-LOCK.
      ASSIGN
         OrderFusion.FusionStatus = "NEW"
         OrderFusion.UpdateTS = fMakeTS().
      FIND CURRENT OrderFusion NO-LOCK.
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
            lbOrder.Brand = gcBrand AND
            lbOrder.CLI = Order.CLI AND
     LOOKUP(lbOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0 AND
            lbOrder.OrderID NE Order.Orderid) THEN DO:

   fSetOrderStatus(Order.OrderId,"4").

   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "Order",
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
              lbOrder.Brand = gcBrand AND
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
                  CLIType.Brand = gcBrand AND
                  CLIType.CLIType = Order.CLIType AND
                  CLIType.LineType > 0) AND
   NOT CAN-FIND(FIRST OrderAction WHERE
                     OrderAction.Brand = gcBrand AND
                     OrderAction.OrderId = Order.OrderID AND
                     OrderAction.ItemType = "BundleItem" AND
                CAN-FIND(FIRST CLIType NO-LOCK WHERE
                               CLIType.Brand = gcBrand AND
                               CLIType.CLIType = OrderAction.ItemKey AND
                               CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}))
                            THEN DO:
      
   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand = gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = 1 NO-ERROR.

   IF NOT fIsMainLineSubActive(
      OrderCustomer.CustIDType,
      OrderCustomer.CustId) THEN 
      lcNewStatus = {&ORDER_STATUS_PENDING_MAIN_LINE}.
END.
ELSE IF Order.Ordertype = {&ORDER_TYPE_MNP} AND
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
   Order.CREventQty = 0 AND 
   Order.CredOk = FALSE AND
   Order.OrderType NE 2 THEN DO: /* Credit scoring is not tried yet */
   
   FIND FIRST OrderCustomer WHERE
      OrderCustomer.Brand = gcBrand AND
      OrderCustomer.OrderId = Order.OrderId AND
      OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

   IF OrderCustomer.CustidType = "CIF" THEN DO:
      FIND FIRST Customer WHERE
         Customer.Brand      = Order.Brand          AND 
         Customer.OrgId      = OrderCustomer.CustId AND
         Customer.CustIdType = OrderCustomer.CustIdType AND
         Customer.Roles NE "inactive" NO-LOCK NO-ERROR. 
      IF AVAIL Customer THEN DO:
         FIND FIRST MobSub WHERE
                    MobSub.Brand   = gcBrand AND
                    MobSub.AgrCust = Customer.CustNum
              NO-LOCK NO-ERROR.
         IF NOT AVAIL MobSub THEN lcNewStatus = "20".
         ELSE lcNewStatus = "21".
      END. /* IF AVAIL Customer THEN DO: */
      ELSE lcNewStatus = "20".
   END. /* IF OrderCustomer.CustidType = "CIF" THEN */
END.

/* MNP Retention Project */
IF Order.OrderChannel BEGINS "retention" AND
   fIsMNPOutOngoing(INPUT Order.CLI) THEN DO:
   lcNewStatus = {&ORDER_STATUS_MNP_RETENTION}.
END. /* IF Order.statuscode NE "4" AND */

/* order status queue must be i.e. 4 -> 22 -> 20 -> 1 */
ELSE IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN DO:
      
   FIND FIRST OrderCustomer WHERE 
      OrderCustomer.Brand = gcBrand AND
      OrderCustomer.OrderId = Order.OrderId AND
      OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

   FIND Customer NO-LOCK WHERE
        Customer.Custnum = Order.Custnum NO-ERROR.

   CASE Order.OrderChannel:
      WHEN "renewal" OR
      WHEN "renewal_telesales" OR
      WHEN "retention" OR
      WHEN "renewal_ctc" THEN DO:
         IF fCheckRenewalData() = TRUE OR OrderCustomer.DataChecked NE ? THEN
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
               lcRenoveSMSText = REPLACE(lcRenoveSMSText,"#ORDER_NUMBER",STRING(Order.OrderId)).
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
            lcNewStatus = (IF OrderCustomer.CustIdType EQ "CIF" AND
                              Order.CredOk = FALSE AND
                              Order.CREventQty = 0
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

IF Order.OrderChannel BEGINS "fusion" THEN DO:
   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand = Order.Brand AND
              OrderFusion.OrderId = Order.OrderID NO-ERROR.
   IF AVAIL OrderFusion AND
            OrderFusion.FusionStatus EQ "" THEN DO:

      FIND CURRENT OrderFusion EXCLUSIVE-LOCK.
      ASSIGN
         OrderFusion.FusionStatus = "NEW"
         OrderFusion.UpdateTS = fMakeTS().
      FIND CURRENT OrderFusion NO-LOCK.
   END.
END.

fMarkOrderStamp(Order.OrderID,
               "Change",
                0.0).

IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhOrder).
   fCleanEventObjects().
END.

RETURN "".
