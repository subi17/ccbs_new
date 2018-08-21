 
 /*------------------------------------------------------------------------
    File        : ValidateACC
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Thu Apr 26 11:50:51 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}
&SCOPED-DEFINE ACC_OLB_BARRINGS_NOT_ALLOWED "Y_HURG"

CLASS Func.ValidateACC FINAL: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PRIVATE ValidateACC (  ):
	END CONSTRUCTOR.

   METHOD PUBLIC STATIC LOGICAL mGetPROStatus
      ( icCategory AS CHARACTER,
        ilOld      AS LOGICAL ):

      DEFINE BUFFER CustCat FOR CustCat.

      FOR CustCat NO-LOCK WHERE
          CustCat.Brand    EQ Syst.Var:gcBrand AND
          CustCat.Category EQ icCategory:
          RETURN CustCat.PRO.   
      END.

      UNDO, THROW NEW Progress.Lang.AppError
            (SUBSTITUTE("Incorrect &1 customer category '&2'",
                        STRING(ilOld,"old/new"),
                        icCategory)).

   END METHOD.

   METHOD PUBLIC STATIC CHARACTER mExistingCustomerACCCompability
      ( icSrcCategory   AS CHARACTER,
        icDstCategory   AS CHARACTER,
        iiDstCustNum    AS INTEGER,
        icDstCustIdType AS CHARACTER,
        icDstOrgId      AS CHARACTER ):
   
      DEFINE VARIABLE llSRCPRO AS LOGICAL NO-UNDO.      
      DEFINE VARIABLE llDSTPRO AS LOGICAL NO-UNDO.
      DEFINE BUFFER MobSub FOR MobSub.

      DO ON ERROR UNDO, THROW:
         ASSIGN
            llSRCPRO = mGetPROStatus(icSrcCategory, YES)
            llDSTPRO = mGetPROStatus(icDstCategory, NO).

         CATCH loError AS Progress.Lang.AppError:
            RETURN loError:ReturnValue.
         END CATCH.
      END.
   
      IF llSRCPRO NE llDSTPRO
      THEN DO:
         IF NOT llSRCPRO AND llDSTPRO
         THEN RETURN "ACC is not allowed from NON-PRO to PRO customer".

         /* Check for any active/ongoing subscriptions */
         IF CAN-FIND(FIRST MobSub NO-LOCK WHERE
                           MobSub.Brand   = Syst.Var:gcBrand AND 
                           MobSub.AgrCust = iiDstCustNum     AND 
                           MobSub.Cli     > "") OR 
            mCheckOngoingOrders(icDstOrgId, icDstCustIdType, 0)
         THEN RETURN "ACC is not allowed from PRO to NON-PRO customer due to " +
                     "active subscriptions or ongoing order".
      END.   

      RETURN "".

   END METHOD.

   METHOD PUBLIC STATIC LOGICAL mCheckOngoingOrders
      ( icOrgId      AS CHARACTER,
        icCustIdType AS CHARACTER,
        iiMsSeq      AS INTEGER ):

      DEFINE BUFFER OrderCustomer FOR OrderCustomer.
      DEFINE BUFFER Order FOR Order.
      
      FOR EACH OrderCustomer NO-LOCK WHERE
               OrderCustomer.Brand      EQ Syst.Var:gcBrand AND
               OrderCustomer.CustId     EQ icOrgId          AND
               OrderCustomer.CustIdType EQ icCustIdType     AND
               OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
         FIRST Order NO-LOCK WHERE
               Order.Brand              EQ Syst.Var:gcBrand AND
               Order.OrderId            EQ Ordercustomer.OrderId AND
               Order.MsSeq              NE iiMsSeq AND
               LOOKUP(Order.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0:
         RETURN TRUE.
      END.
      RETURN FALSE.
   
   END METHOD.

   METHOD PUBLIC STATIC CHARACTER mPreCheckSubscriptionForACC
      (iiMsSeq AS INTEGER):

      DEFINE BUFFER MobSub FOR MobSub.
      DEFINE BUFFER CLIType FOR CLIType.

      FOR MobSub NO-LOCK WHERE
          MobSub.MsSeq = iiMsSeq:

         IF MobSub.MultiSimId > 0 AND
            MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY}
         THEN RETURN "ACC is not allowed for multi SIM secondary subscription".

         IF CAN-FIND(FIRST CLIType WHERE
                           CLIType.Brand    = Syst.Var:gcBrand AND
                           CLIType.CLIType  = (IF MobSub.TariffBundle > ""
                                               THEN MobSub.TariffBundle
                                               ELSE MobSub.CLIType) AND
                           CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL})
         THEN RETURN "ACC is not allowed for additional line".
         
         RETURN "".
      END.
      
      RETURN SUBSTITUTE("Cannot find subscription with MsSeq &1", iiMsSeq).

   END METHOD.

   METHOD PUBLIC STATIC CHARACTER mCheckSubscriptionForACC
      ( iiMsSeq      AS INTEGER,
        iiCurrentReq AS INTEGER,
        iiCurrentOrderID AS INTEGER,
        icChannel    AS CHARACTER ):
      
      DEFINE BUFFER Barring FOR Barring.
      DEFINE BUFFER MobSub FOR MobSub.
      DEFINE BUFFER MsOwner FOR MsOwner.
      DEFINE BUFFER Customer FOR Customer.
      DEFINE BUFFER Limit FOR Limit.
      DEFINE BUFFER MsRequest FOR MsRequest.
      DEFINE BUFFER Order FOR Order.

      DEFINE VARIABLE lcSRCCategory  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcReturnValue  AS CHARACTER NO-UNDO.
     
      FIND MobSub NO-LOCK WHERE MobSub.MsSeq = iiMsSeq NO-ERROR.
      
      IF NOT AVAILABLE MobSub
      THEN RETURN "ERROR|Unknown subscription".
   
      IF NOT CAN-FIND(Customer NO-LOCK WHERE Customer.CustNum = MobSub.AgrCust)
      THEN RETURN "ERROR|Current agreement customer for subscription is invalid".
   
      IF MobSub.AgrCust NE MobSub.InvCust AND
         NOT CAN-FIND(Customer NO-LOCK WHERE Customer.CustNum = MobSub.InvCust)
      THEN RETURN "ERROR|Current invoicing customer for subscription is invalid".
   
      IF MobSub.AgrCust NE MobSub.CustNum AND
         NOT CAN-FIND(Customer NO-LOCK WHERE Customer.CustNum = MobSub.CustNum)
      THEN RETURN "ERROR|Current user for subscription is invalid".

      IF TODAY - MobSub.ActivationDate < 30
      THEN RETURN "CHECK|Subscription has not been active long enough".

      /* Is there a pending request */ 
      FOR EACH MsRequest NO-LOCK WHERE 
               MsRequest.MSSeq = Mobsub.MSSeq:

          IF LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0 AND
             MsRequest.MsRequest NE iiCurrentReq
          THEN IF MsRequest.ReqType EQ 18
               THEN RETURN "ERROR/SMS/312|Subscription has a pending termination request.".
               ELSE RETURN "ERROR/SMS/310|Subscription has a pending request.".
      END.

      FOR EACH Order NO-LOCK USE-INDEX MsSeq WHERE
               Order.MsSeq EQ Mobsub.MsSeq AND
               Order.OrderID NE iiCurrentOrderID AND
               Order.OrderType EQ {&ORDER_TYPE_ACC} AND
            LOOKUP(STRING(Order.StatusCode), {&ORDER_INACTIVE_STATUSES}) = 0:
         RETURN "ERROR/SMS/310|Subscription has a pending request.".
      END.
   
      IF LOOKUP(STRING(MobSub.MsStat),"4,8,17") = 0
      THEN RETURN "ERROR|Subscription status is not valid for owner change".

      IF Func.BarrMethod:mSubsHaveActiveBarring(MobSub.MsSeq,
                                                {&FRAUD_BARR_CODES} +
                                                {&ACC_OLB_BARRINGS_NOT_ALLOWED})
      THEN RETURN "ERROR/SMS/311|Subscription has an active operator or debt barring".

      IF icChannel NE {&REQUEST_SOURCE_NEWTON} THEN
         FOR FIRST Limit NO-LOCK WHERE
                   Limit.MsSeq     = MobSub.MsSeq   AND
                   Limit.LimitType = 3              AND
                   Limit.TMRuleSeq = 0              AND
                   Limit.ToDate   >= TODAY          AND
                   Limit.FromDate <= TODAY          AND
                   Limit.Custnum   = MobSub.Custnum AND
                   Limit.LimitID   = 0              AND
                   Limit.LimitAmt > 0:
            RETURN "ERROR|Subscription has a billing suspension / prohibition".
         END.
   
      FIND FIRST MsOwner USE-INDEX MsSeq NO-LOCK WHERE
                 MsOwner.MsSeq = MobSub.MsSeq AND
                 MsOwner.TsEnd > MsOwner.TsBegin
      NO-ERROR.
      IF NOT AVAILABLE MsOwner    OR 
         MsOwner.TsEnd < 99999999 OR
         MsOwner.CustNum NE MobSub.CustNum OR
         MsOwner.InvCust NE MobSub.InvCust OR
         MsOwner.AgrCust NE MobSub.AgrCust
      THEN RETURN "ERROR|Timestamp history data for subscription is invalid".
         
      IF Mnp.MNPOutGoing:mIsMNPOutOngoing(MobSub.CLI)
      THEN RETURN "ERROR|Ongoing MNP OUT request".
      
      IF iiCurrentReq > 0 THEN DO:
         FIND FIRST MsRequest NO-LOCK WHERE
                    MsRequest.MsRequest = iiCurrentReq AND
                    MsRequest.ReqType = 10 NO-ERROR.
         IF NOT AVAILABLE MsRequest
         THEN RETURN "ERROR|Unknown request".
   
         IF MsRequest.ReqIParam1 > 0 THEN DO:
   
            FIND Customer NO-LOCK WHERE
                 Customer.Custnum EQ MsRequest.Custnum NO-ERROR.
            IF NOT AVAILABLE Customer
            THEN RETURN "ERROR|Old customer not found".
   
            lcSRCCategory = Customer.Category.
            
            FIND Customer NO-LOCK WHERE
                 Customer.Custnum EQ MsRequest.ReqIParam1 NO-ERROR.
            IF NOT AVAILABLE Customer
            THEN RETURN "ERROR|New customer not found".
            
            lcReturnValue = Func.ValidateACC:mExistingCustomerACCCompability
                                       (lcSRCCategory,
                                        Customer.Category,
                                        Customer.CustNum,
                                        Customer.CustIdType,
                                        Customer.OrgId).
   
            IF lcReturnValue > "" THEN RETURN "ERROR|" + lcReturnValue. 
         END.
      END.
   
      RETURN "".
   
   END METHOD.

   METHOD PUBLIC STATIC CHARACTER mCheckTargetCustomerForACC
      (iiNewCustNum AS INTEGER):
      
      DEFINE BUFFER MobSub   FOR MobSub.
      DEFINE BUFFER Customer FOR Customer.
      
      DEFINE VARIABLE lcBarrStatus AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE liSubLimit AS INTEGER NO-UNDO. 
      DEFINE VARIABLE lisubs AS INTEGER NO-UNDO.
      DEFINE VARIABLE liActLimit AS INTEGER NO-UNDO.
      DEFINE VARIABLE liActs AS INTEGER NO-UNDO.
      
      IF iiNewCustnum = 0
      THEN RETURN "".
   
      FOR EACH MobSub NO-LOCK WHERE
               MobSub.Brand = Syst.Var:gcBrand AND
               MobSub.AgrCust = iiNewCustnum AND
               MobSub.PayType = FALSE:

         IF Func.BarrMethod:mSubsHaveActiveBarring(MobSub.MsSeq,
                                                   {&FRAUD_BARR_CODES} +
                                                   {&ACC_OLB_BARRINGS_NOT_ALLOWED})
         THEN RETURN "ERROR|Target customer has subscription with active operator or debt barring".

      END.
   
      FIND FIRST Customer NO-LOCK WHERE
                 Customer.CustNum = iiNewCustnum 
      NO-ERROR.
      
      IF AVAILABLE Customer AND
         NOT mSubscriptionLimitCheck(INPUT Customer.OrgId,
                                     INPUT Customer.CustIdType,
                                     INPUT mIsSelfEmpl(Customer.Category),
                                     INPUT mIsPro(Customer.Category),
                                     1,
                                     OUTPUT liSubLimit,
                                     OUTPUT liSubs,
                                     OUTPUT liActLimit,
                                     OUTPUT liActs)
      THEN RETURN "ERROR|Subscription limit exceeded".
   
      RETURN "".
   
   END METHOD. 

   METHOD PRIVATE STATIC INTEGER mCheckSubsLimit
      (iiCustnum      AS INTEGER,
       iiLimitType    AS INTEGER,
       icIdType       AS CHARACTER,
       ilSelfEmployed AS LOGICAL,
       ilPro          AS LOGICAL):
   
      DEF VAR liLimit AS INT NO-UNDO.
   
      DEF BUFFER Limit FOR Limit.
      DEF BUFFER CustCat FOR CustCat.
   
      FIND FIRST Limit NO-LOCK WHERE 
                 Limit.CustNum   EQ iiCustnum     AND
                 Limit.LimitType EQ iiLimitType AND
                 Limit.ToDate    >= TODAY
      NO-ERROR.
      
      IF AVAIL Limit AND Limit.LimitAmt NE ?
      THEN liLimit = Limit.LimitAmt.
      ELSE DO:
         FIND FIRST CustCat NO-LOCK WHERE
                    CustCat.Brand        EQ Syst.Var:gcBrand AND
                    CustCat.CustIdType   EQ icIdType AND
                    CustCat.SelfEmployed EQ ilSelfEmployed AND
                    CustCat.pro          EQ ilpro
         NO-ERROR. 
         IF AVAILABLE CustCat
         THEN DO:
            IF iiLimitType = {&LIMIT_TYPE_SUBQTY}
            THEN liLimit = CustCat.MobSubLimit.
            ELSE IF iiLimitType = {&LIMIT_TYPE_SUBACTQTY}
            THEN liLimit = CustCat.ActivationLimit.
         END.
      END.

      RETURN liLimit.

   END METHOD.

   METHOD PUBLIC STATIC LOGICAL mSubscriptionLimitCheck
      (icPersonId             AS CHARACTER,
       icIdType               AS CHARACTER,
       ilSelfEmployed         AS LOGICAL,
       ilPro                  AS LOGICAL, 
       iiOrders               AS INTEGER,
       OUTPUT oiSubLimit      AS INTEGER,
       OUTPUT oiSubCount      AS INTEGER,
       OUTPUT oiSubActLimit   AS INTEGER,
       OUTPUT oiActOrderCount AS INTEGER):
   
      /* no sole trader passport custcat available */
      IF icIdType = "passport" THEN ilSelfEmployed = FALSE.
   
      DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO. 
   
      DEFINE BUFFER OrderCustomer FOR OrderCustomer.
      DEFINE BUFFER Order FOR Order.
      DEFINE BUFFER OrderAction FOR OrderAction.
      DEFINE BUFFER Customer FOR Customer.
      DEFINE BUFFER Limit FOR Limit.
      DEFINE BUFFER CustCat FOR CustCat.
      DEFINE BUFFER MobSub FOR MobSub. 
      DEFINE BUFFER DiscountPlan FOR DiscountPlan.
      DEFINE BUFFER DPMember FOR DPMember.
      
      FOR
         EACH OrderCustomer NO-LOCK WHERE   
              OrderCustomer.Brand      EQ Syst.Var:gcBrand AND 
              OrderCustomer.CustId     EQ icPersonId       AND
              OrderCustomer.CustIdType EQ icIdType         AND
              OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
         EACH Order NO-LOCK WHERE
              Order.Brand              EQ Syst.Var:gcBrand      AND
              Order.orderid            EQ OrderCustomer.Orderid AND
              Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND
              Order.OrderType          NE {&ORDER_TYPE_STC}     AND
              Order.SalesMan NE "GIFT":

         IF Order.MultiSimId   NE 0                         AND 
            Order.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND
            NOT mIsTerminalOrder(Order.OrderId)             AND 
            CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                           OrderAction.Brand    = Syst.Var:gcBrand        AND
                           OrderAction.OrderID  = Order.OrderID           AND
                           OrderAction.ItemType = "ExtraLineDiscount"     AND
                           OrderAction.ItemKey  = Order.CLIType + "DISC")
         THEN NEXT.
          
         IF LOOKUP(STRING(Order.statuscode),{&ORDER_CLOSE_STATUSES}) EQ 0
         THEN DO:
            /* YDR-1532 */
            IF Order.StatusCode EQ {&ORDER_STATUS_DELIVERED}
            THEN DO:
               Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
               IF INTERVAL(TODAY, ldaOrderDate, "months") >= 24 THEN NEXT.
            END.
            oiActOrderCount = oiActOrderCount + 1.
         END.

         IF LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0
         THEN oiSubCount = oiSubCount + 1.
         
      END.
      
      /* used with multisim orders */
      IF iiOrders > 1
      THEN oiActOrderCount = oiActOrderCount + (iiOrders - 1).
      
      subs_count:
      FOR
         EACH Customer NO-LOCK WHERE
              Customer.Brand      EQ Syst.Var:gcBrand AND
              Customer.CustIdType EQ icIdType         AND
              Customer.OrgId      EQ icPersonId       AND
              Customer.Roles      NE "inactive",
         EACH MobSub NO-LOCK WHERE
              MobSub.Brand        EQ Syst.Var:gcBrand AND
              MobSub.AgrCust      EQ Customer.CustNum AND
              MobSub.SalesMan     NE "GIFT":
         /* YDR-2665 */
         IF MobSub.MultiSimId     GT 0  AND 
            MobSub.MultiSimType   EQ {&MULTISIMTYPE_EXTRALINE}
         THEN
            FOR
               EACH DiscountPlan NO-LOCK WHERE
                    DiscountPlan.Brand    EQ Customer.Brand          AND
                    DiscountPlan.DPRuleID EQ MobSub.CLIType + "DISC" AND
                    DiscountPlan.ValidTo  >= TODAY,
               FIRST DPMember NO-LOCK WHERE
                     DPMember.DPID       = DiscountPlan.DPID AND
                     DPMember.HostTable  = "MobSub" AND
                     DPMember.KeyValue   = STRING(MobSub.MsSeq) AND
                     DPMember.ValidTo   >= TODAY AND
                     DPMember.ValidFrom <= DPMember.ValidTo:
                FIND FIRST Order WHERE 
                           Order.MsSeq        = MobSub.MsSeq AND 
                           Order.MultiSimId   GT 0           AND 
                           Order.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}  
                           NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Order 
                   OR ( NOT mIsTerminalOrder(Order.OrderId) ) THEN
                   NEXT subs_count.  
            END. 
         oiSubCount = oiSubCount + 1.
      END.
   
      FIND FIRST Customer NO-LOCK WHERE
                 Customer.Brand           EQ Syst.Var:gcBrand AND
                 Customer.CustIdType      EQ icIdType         AND
                 Customer.OrgId           EQ icPersonId       AND
                 Customer.Roles           NE "inactive"
      NO-ERROR.
   
      IF AVAILABLE Customer
      THEN DO:
          oiSubLimit    = mCheckSubsLimit(INPUT Customer.Custnum,
                                          INPUT {&LIMIT_TYPE_SUBQTY},
                                          INPUT icIdType,
                                          INPUT ilSelfEmployed,
                                          INPUT ilPro).
          oiSubActLimit = mCheckSubsLimit(INPUT Customer.Custnum,
                                          INPUT {&LIMIT_TYPE_SUBACTQTY},
                                          INPUT icIdType,
                                          INPUT ilSelfEmployed,
                                          INPUT ilPro).
          /* check subscription limit and subscription activation limit */
      END.

      ELSE DO:
         FIND FIRST CustCat NO-LOCK WHERE
                    CustCat.Brand        EQ Syst.Var:gcBrand AND
                    CustCat.CustIdType   EQ icIdType         AND
                    CustCat.SelfEmployed EQ ilSelfEmployed
         NO-ERROR. 
         IF AVAILABLE CustCat
         THEN ASSIGN oiSubLimit    = CustCat.MobSubLimit
                     oiSubActLimit = CustCat.ActivationLimit.
      END.
      
      IF oiSubCount >= oiSubLimit OR oiActOrderCount >= oiSubActLimit
      THEN RETURN FALSE.
   
      RETURN TRUE.

   END METHOD.

   /* check self employee */
   METHOD PUBLIC STATIC LOGICAL mIsSelfEmpl
      (icCategory AS CHARACTER):
   
      DEFINE BUFFER CustCat FOR CustCat.

      FOR CustCat NO-LOCK WHERE
          CustCat.Brand    EQ Syst.Var:gcBrand AND
          CustCat.Category EQ icCategory:
          RETURN INDEX(custcat.catname, "self") > 0.   
      END.

      RETURN FALSE.
   
   END METHOD.

   /* check pro */
   METHOD PUBLIC STATIC LOGICAL mIsPro
      (icCategory AS CHARACTER):
   
      DEFINE BUFFER CustCat FOR CustCat.

      FOR CustCat NO-LOCK WHERE
          CustCat.Brand    EQ Syst.Var:gcBrand AND
          CustCat.Category EQ icCategory:
          RETURN Custcat.Pro.   
      END.

      RETURN FALSE.
   
   END METHOD.

   METHOD PUBLIC STATIC CHARACTER mNewCustomerACCCompability
      (icCategory   AS CHARACTER,
       icCustId     AS CHARACTER,
       icCustIdType AS CHARACTER):
   
      DEFINE VARIABLE llSRCPRO       AS LOGICAL NO-UNDO.
      DEFINE VARIABLE llSelfEmployed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE liTemp         AS INTEGER NO-UNDO.
   
      llSRCPRO = mIsPro(icCategory).   
      IF llSRCPRO AND icCustIdType NE "CIF"
      THEN llSelfEmployed = mIsSelfEmpl(icCategory).

      IF NOT Func.ValidateACC:mSubscriptionLimitCheck(icCustId,
                                                      icCustIdType,
                                                      llSelfEmployed, 
                                                      llSRCPRO,
                                                      1,
                                                      OUTPUT liTemp,
                                                      OUTPUT liTemp,
                                                      OUTPUT liTemp,
                                                      OUTPUT liTemp)
      THEN RETURN "Subscription limit exceeded".
      
      RETURN "".

   END METHOD.


   METHOD PUBLIC STATIC LOGICAL mIsTerminalOrder
      (INPUT liOrderId       AS INTEGER ):

      DEFINE BUFFER bTermOrder FOR Order.
      DEFINE BUFFER OfferItem  FOR OfferItem.
      DEFINE BUFFER BillItem   FOR BillItem.
      DEFINE BUFFER BitemGroup FOR BitemGroup.

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
            RETURN TRUE.
      END.

      RETURN FALSE.
      
   END METHOD.
   
END CLASS.
