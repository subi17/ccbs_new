&IF "{&orderchk}" NE "YES"
&THEN
&GLOBAL-DEFINE orderchk YES

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/extralinefunc.i}

/* ----------------------------------------------------------------------
  MODULE .......: orderchk.i 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 20.08.08
  CHANGED ......:
  Version ......: xfera
  27/12/17  ashok  YDR-2665 ExtraLine should not be counted for 
                            Subscription and Activation limits
----------------------------------------------------------------------- */

FUNCTION fCheckSubsLimit RETURNS INT (INPUT iiCustnum      AS INT,
                                      INPUT iiLimitType    AS INT,
                                      INPUT icIdType       AS CHAR,
                                      INPUT ilSelfEmployed AS LOG,
                                      INPUT ilpro          AS LOG):

   DEF VAR liLimit AS INT NO-UNDO.

   DEF BUFFER Limit FOR Limit.
   DEF BUFFER CustCat FOR CustCat.

   FIND FIRST Limit WHERE 
              Limit.CustNum = iiCustnum     AND
              Limit.LimitType = iiLimitType AND
              Limit.ToDate >= TODAY NO-LOCK NO-ERROR.
   IF AVAIL Limit AND Limit.LimitAmt NE ? THEN 
      liLimit = Limit.LimitAmt.
   ELSE DO:
      FIND FIRST CustCat WHERE
                 CustCat.Brand = Syst.Var:gcBrand AND
                 CustCat.CustIdType = icIdType AND
                 CustCat.SelfEmployed = ilSelfEmployed AND
                 CustCat.pro EQ ilpro NO-LOCK NO-ERROR. 
      IF AVAIL CustCat THEN DO:
         IF iiLimitType = {&LIMIT_TYPE_SUBQTY} THEN
            liLimit = CustCat.MobSubLimit.
         ELSE IF iiLimitType = {&LIMIT_TYPE_SUBACTQTY} THEN
            liLimit = CustCat.ActivationLimit.
      END. /* IF AVAIL CustCat THEN DO: */
   END. /* ELSE DO: */

   RETURN liLimit.
END FUNCTION. /* FUNCTION fCheckSubsLimit */

/*Usage of return values:
TRUE  -> RENEWAL
FALSE -> RENEWAL_HOLD*/
FUNCTION fCheckRenewalData RETURNS LOGICAL
   (INPUT iiOrderID AS INT):

   DEF BUFFER bOrderCustomer FOR OrderCustomer.
   DEF BUFFER Order FOR Order.
   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderID EQ iiOrderID NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.           

   /* delivery address is different than customer address */
   IF CAN-FIND(FIRST bOrderCustomer NO-LOCK WHERE
      bOrderCustomer.Brand = Syst.Var:gcBrand AND
      bOrderCustomer.OrderId  = OrderCustomer.OrderID AND
      bOrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}) 
      OR
      OrderCustomer.Firstname NE Customer.Firstname OR
      OrderCustomer.Surname1 NE Customer.Custname OR
      OrderCustomer.Surname2 NE Customer.Surname2 OR
      OrderCustomer.BirthDay NE Customer.BirthDay OR
      OrderCustomer.Address NE Customer.Address OR
      OrderCustomer.PostOffice NE Customer.PostOffice OR
      OrderCustomer.Region NE Customer.Region OR
      OrderCustomer.Country NE Customer.Country OR
      OrderCustomer.ZipCode NE Customer.ZipCode OR      
      OrderCustomer.BankCode NE Customer.BankAcct  
   THEN DO:
    /*YDR-2834 [RES-1119] Skip order status 31 SM/PM*/
    /*No need to RENEWAL_HOLD when delivery to shop*/
      IF (Order.OrderChannel EQ "renewal" OR
          Order.OrderChannel EQ "renewal_telesales" OR
          Order.OrderChannel EQ "retention" OR
          Order.OrderChannel EQ "renewal_ctc") AND
         (Order.Deliverytype EQ {&ORDER_DELTYPE_POS}  OR
          Order.Deliverytype EQ {&ORDER_DELTYPE_POST})THEN RETURN TRUE.
      RETURN FALSE.
   END.

   RETURN TRUE.

END FUNCTION. 

/* do not change error text values (used in web) */
FUNCTION fSubscriptionLimitCheck RETURNS LOGICAL
   (pcPersonId AS CHAR,
    pcIdType AS CHAR,
    plSelfEmployed AS LOG,
    plpro AS LOG, 
    piOrders AS INT,
    OUTPUT oiSubLimit AS INT,
    OUTPUT oiSubCount AS INT,
    OUTPUT oiSubActLimit AS INT,
    OUTPUT oiActOrderCount AS INT):

   /* no sole trader passport custcat available */
   IF pcIdType = "passport" THEN plSelfEmployed = FALSE.

   DEF VAR ldaOrderDate AS DATE NO-UNDO. 

   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER Order FOR Order.
   DEF BUFFER Customer FOR Customer.
   DEF BUFFER Limit FOR Limit.
   DEF BUFFER CustCat FOR CustCat.
   DEF BUFFER bMobSub FOR MobSub.
 

   FOR EACH OrderCustomer NO-LOCK WHERE   
            OrderCustomer.Brand      EQ Syst.Var:gcBrand AND 
            OrderCustomer.CustId     EQ pcPersonId AND
            OrderCustomer.CustIdType EQ pcIdType AND
            OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
      EACH  Order NO-LOCK WHERE
            Order.Brand              EQ Syst.Var:gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND
            Order.OrderType          NE {&ORDER_TYPE_STC} AND
            Order.SalesMan NE "GIFT":
        /* YDR-2665 */
        IF fCLITypeIsExtraLine(Order.CLIType)              AND 
           Order.MultiSimId   NE 0                         AND 
           Order.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} THEN 
        DO:
            IF CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                        OrderAction.Brand    = Syst.Var:gcBrand        AND
                        OrderAction.OrderID  = Order.OrderID           AND
                        OrderAction.ItemType = "ExtraLineDiscount"     AND
                        OrderAction.ItemKey  = Order.CLIType + "DISC") THEN NEXT.
        END.
       
        IF LOOKUP(STRING(Order.statuscode),{&ORDER_CLOSE_STATUSES}) EQ 0
        THEN DO:
           /* YDR-1532 */
           IF Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} THEN DO:
              Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
              IF INTERVAL(TODAY, ldaOrderDate, "months") >= 24 THEN NEXT.
           END.
           oiActOrderCount = oiActOrderCount + 1.

        END.
        
        IF LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0 THEN
            oiSubCount = oiSubCount + 1.
      
   END.
   
   /* used with multisim orders */
   IF piOrders > 1 THEN
      oiActOrderCount = oiActOrderCount + (piOrders - 1).
   
   subs_count:
   FOR EACH Customer NO-LOCK
   WHERE Customer.Brand           EQ Syst.Var:gcBrand
     AND Customer.CustIdType      EQ pcIdType
     AND Customer.OrgId           EQ pcPersonId
     AND Customer.Roles           NE "inactive",
   EACH bMobsub NO-LOCK
   WHERE bMobSub.Brand             EQ Syst.Var:gcBrand 
     AND bMobsub.AgrCust           EQ Customer.CustNum
     AND bMobSub.SalesMan NE "GIFT":
      /* YDR-2665 */
      IF fCLITypeIsExtraLine(bMobsub.CliType)       AND 
         bMobsub.MultiSimId                   GT 0  AND 
         bMobsub.MultiSimType                 EQ {&MULTISIMTYPE_EXTRALINE} THEN DO: 
         FOR EACH DiscountPlan NO-LOCK 
            WHERE DiscountPlan.brand EQ Customer.Brand 
              AND DiscountPlan.DPRuleID = (bMobsub.CLIType + "DISC")  
              AND DiscountPlan.ValidTo >= TODAY,
             FIRST DPMember NO-LOCK WHERE
               DPMember.DPID       = DiscountPlan.DPID AND
               DPMember.HostTable  = "MobSub" AND
               DPMember.KeyValue   = STRING(bMobSub.MsSeq) AND
               DPMember.ValidTo   >= TODAY AND
               DPMember.ValidFrom <= DPMember.ValidTo:
             NEXT subs_count.
         END. 
      END.
      oiSubCount = oiSubCount + 1.
   END.

   FIND FIRST Customer
   WHERE Customer.Brand           EQ Syst.Var:gcBrand
     AND Customer.CustIdType      EQ pcIdType
     AND Customer.OrgId           EQ pcPersonId 
     AND Customer.Roles           NE "inactive"
   NO-LOCK NO-ERROR.

   IF AVAIL Customer THEN DO:

       oiSubLimit    = fCheckSubsLimit(INPUT Customer.Custnum,
                                       INPUT {&LIMIT_TYPE_SUBQTY},
                                       INPUT pcIdType,
                                       INPUT plSelfEmployed,
                                       INPUT plpro).
       oiSubActLimit = fCheckSubsLimit(INPUT Customer.Custnum,

                                       INPUT {&LIMIT_TYPE_SUBACTQTY},
                                       INPUT pcIdType,
                                       INPUT plSelfEmployed,
                                       INPUT plpro).
       /* check subscription limit and subscription activation limit */
   END. /* IF AVAIL Customer THEN DO: */
   ELSE DO:
      FIND FIRST CustCat WHERE
                 CustCat.Brand = Syst.Var:gcBrand AND
                 CustCat.CustIdType = pcIdType AND
                 CustCat.SelfEmployed = plSelfEmployed NO-LOCK NO-ERROR. 
      IF AVAIL CustCat THEN
         ASSIGN oiSubLimit    = CustCat.MobSubLimit
                oiSubActLimit = CustCat.ActivationLimit.
   END.
   
   IF oiSubCount >= oiSubLimit OR oiActOrderCount >= oiSubActLimit THEN
      RETURN FALSE.

   RETURN TRUE.
END.

/* Two orders cannot be ongoing at the same time with this logic */
FUNCTION fOngoingOrders RETURNS LOGICAL
(pcCli AS CHAR,
 pcNumberType AS CHAR):

   DEF VAR liExcludeOrderType AS INT NO-UNDO. 

   IF pcNumberType EQ "stc" THEN liExcludeOrderType = {&ORDER_TYPE_RENEWAL}.
   ELSE IF pcNumberType EQ "renewal" OR
           pcNumberType EQ "retention" THEN liExcludeOrderType = {&ORDER_TYPE_STC}.
   ELSE liExcludeOrderType = -1.

   DEF BUFFER lbOtherOrder FOR Order.   
   
   FOR EACH lbOtherOrder NO-LOCK WHERE
            lbOtherOrder.brand EQ Syst.Var:gcBrand AND
            lbOtherOrder.CLI EQ pcCLI AND
            LOOKUP(lbOtherOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0 AND
            lbOtherOrder.OrderType NE liExcludeOrderType:

      /* YPR-2105 */
      IF pcNumberType EQ "retention" AND
         lbOtherOrder.StatusCode = {&ORDER_STATUS_OFFER_SENT} THEN NEXT. /* shouldn't never happen because of YDR-2575 */
      RETURN TRUE.
   END.
   
   RETURN FALSE.

END FUNCTION. 

FUNCTION fOngoingFixedOrders RETURNS CHARACTER
(pcFixedNumber AS CHAR,
 pcNumberType  AS CHAR):

   DEF VAR liExcludeOrderType AS INT NO-UNDO. 

   IF pcNumberType EQ "stc" THEN liExcludeOrderType = {&ORDER_TYPE_RENEWAL}.
   ELSE IF pcNumberType EQ "renewal" OR
           pcNumberType EQ "retention" THEN liExcludeOrderType = {&ORDER_TYPE_STC}.
   ELSE liExcludeOrderType = -1.

   /* Check if same number in ongoing Fusion order */
   DEF BUFFER lbOtherOrder  FOR Order.
   DEF BUFFER lbOrderFusion FOR OrderFusion.
   FOR EACH lbOrderFusion NO-LOCK WHERE
            lbOrderFusion.FixedNumber EQ pcFixedNumber,
      EACH  lbOtherOrder NO-LOCK WHERE
            lbOtherOrder.brand EQ Syst.Var:gcBrand AND
            lbOtherOrder.OrderId EQ lbOrderFusion.OrderId AND
            LOOKUP(lbOtherOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0 AND
            lbOtherOrder.OrderType NE liExcludeOrderType:

      RETURN "Ongoing order for number|" + pcFixedNumber.
   END.

   RETURN "".

END FUNCTION. 

FUNCTION fIsPreactivatedCustomer RETURNS LOGICAL
(iiCustnum AS INTEGER):
   RETURN (LOOKUP(STRING(iiCustnum), "233718,239696,239680,239666") > 0).
END FUNCTION. 

&ENDIF
