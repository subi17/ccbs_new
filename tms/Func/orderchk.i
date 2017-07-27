&IF "{&orderchk}" NE "YES"
&THEN
&GLOBAL-DEFINE orderchk YES

{Syst/commali.i}
{Func/barrfunc.i}
{Syst/tmsconst.i}

/* ----------------------------------------------------------------------
  MODULE .......: orderchk.i 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 20.08.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

FUNCTION fCheckSubsLimit RETURNS INT (INPUT iiCustnum      AS INT,
                                      INPUT iiLimitType    AS INT,
                                      INPUT icIdType       AS CHAR,
                                      INPUT ilSelfEmployed AS LOG,
                                      INPUT ilpro          AS LOG):

   DEF VAR liLimit AS INT NO-UNDO.

   FIND FIRST Limit WHERE 
              Limit.CustNum = iiCustnum     AND
              Limit.LimitType = iiLimitType AND
              Limit.ToDate >= TODAY NO-LOCK NO-ERROR.
   IF AVAIL Limit AND Limit.LimitAmt NE ? THEN 
      liLimit = Limit.LimitAmt.
   ELSE DO:
      FIND FIRST CustCat WHERE
                 CustCat.Brand = gcBrand AND
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

FUNCTION fCheckRenewalData RETURNS LOGICAL:

   DEF BUFFER bOrderCustomer FOR OrderCustomer.

   IF CAN-FIND(FIRST bOrderCustomer NO-LOCK WHERE
                     bOrderCustomer.Brand = gcBrand AND
                     bOrderCustomer.OrderId  = OrderCustomer.OrderID AND
                     bOrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}) 
                     THEN RETURN FALSE.
   
   IF 
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
   THEN RETURN FALSE.
   ELSE RETURN TRUE.

END FUNCTION. 

/* do not change error text values (used in web) */
FUNCTION fSubscriptionLimitCheck RETURNS LOGICAL
   (pcPersonId AS CHAR,
    pcIdType AS CHAR,
    plSelfEmployed AS LOG,
    plpro AS LOG, 
    piOrders AS INT,
    OUTPUT ocReason AS CHAR,
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
            OrderCustomer.Brand      EQ gcBrand AND 
            OrderCustomer.CustId     EQ pcPersonId AND
            OrderCustomer.CustIdType EQ pcIdType AND
            OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
      EACH  Order NO-LOCK WHERE
            Order.Brand              EQ gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND
            Order.OrderType          NE {&ORDER_TYPE_STC} AND
            Order.SalesMan NE "GIFT":
        
        IF LOOKUP(STRING(Order.statuscode),{&ORDER_CLOSE_STATUSES}) EQ 0
        THEN DO:
           /* YDR-1532 */
           IF Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} THEN DO:
              fTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
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

   FOR EACH Customer NO-LOCK
   WHERE Customer.Brand           EQ gcBrand
     AND Customer.CustIdType      EQ pcIdType
     AND Customer.OrgId           EQ pcPersonId
     AND Customer.Roles           NE "inactive",
   EACH bMobsub NO-LOCK
   WHERE bMobSub.Brand             EQ gcBrand 
     AND bMobsub.AgrCust           EQ Customer.CustNum
     AND bMobSub.SalesMan NE "GIFT":
      oiSubCount = oiSubCount + 1.
   END.

   FIND FIRST Customer
   WHERE Customer.Brand           EQ gcBrand
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
       /* check barring subscriptions */
       IF fExistBarredSubForCustomer(Customer.CustNum) THEN DO: 
          ocReason = "barring".
          RETURN FALSE.
       END.
       /* check subscription limit and subscription activation limit */
   END. /* IF AVAIL Customer THEN DO: */
   ELSE DO:
      FIND FIRST CustCat WHERE
                 CustCat.Brand = gcBrand AND
                 CustCat.CustIdType = pcIdType AND
                 CustCat.SelfEmployed = plSelfEmployed NO-LOCK NO-ERROR. 
      IF AVAIL CustCat THEN
         ASSIGN oiSubLimit    = CustCat.MobSubLimit
                oiSubActLimit = CustCat.ActivationLimit.
   END.
   
   IF oiSubCount >= oiSubLimit OR oiActOrderCount >= oiSubActLimit THEN DO:
      ocReason = "subscription limit". 
      RETURN FALSE.
   END.

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
            lbOtherOrder.brand EQ gcBrand AND
            lbOtherOrder.CLI EQ pcCLI AND
            LOOKUP(lbOtherOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0 AND
            lbOtherOrder.OrderType NE liExcludeOrderType:

      /* YPR-2105 */
      IF pcNumberType EQ "retention" AND
         lbOtherOrder.StatusCode = {&ORDER_STATUS_OFFER_SENT} THEN NEXT.

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
            lbOtherOrder.brand EQ gcBrand AND
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
