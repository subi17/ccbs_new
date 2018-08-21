/* ----------------------------------------------------------------------
  MODULE .......: createcustomer.p
  TASK .........: create new customer from order
  CREATED ......: JP
  CHANGED ......: 20.11.06/aam new db structure
                  27.11.06/aam create periodical contract if terminal sold  
                  02.12.06/jp  customer number for msrequest
                  15.01.07/aam error from cashfee to an order memo
                  27.02.07 kl  PrePaid testing
                  28.02.07 kl  MsStatus = 4, topup for Prepaid
                  09.03.07 jp  create monthlyfee if clitype.feemodel1 activated
                  19.03.07 kl  check that msisdn is available
                  08.05.07/jp  msisdn changes
                  10.05.07/aam create fees according to clitype.feemodel1
                  28.08.07/aam fatime from order
                  02.10.07/aam OrderTopup handling
                  04.02.08/aam dccli for pos-orders also
                  11.08.2015 hugo.lujan YBU-4587 [DCH] - Authorized Document id
                  value NIF/NIE is updated when handling a postpaid order in 
                  status 6 and company is an existing company with an active 
                  postpaid subscription.
  Version ......: yoigo
-------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msisdn.i}
{Func/forderstamp.i}
{Func/ftaxdata.i}
{Func/ftopup.i}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Func/service.i}
{Func/ftmrlimit.i}
{Func/fsmsreq.i}
{Func/orderfunc.i}
{Func/fsubsterminal.i}
{Mc/invoicetarget.i}
{Func/dss_activation.i}
{Func/orderchk.i}
{Func/fmakesms.i}
{Func/transname.i}
{Mm/fbundle.i}
{Func/fbankdata.i}
{Func/create_eventlog.i}
{Func/msisdn_prefix.i}
{Func/add_lines_request.i}
{Func/order.i}

DEF INPUT  PARAMETER iiMSRequest AS INT  NO-UNDO.

DEF VAR lcCharValue           AS CHAR NO-UNDO.
DEF VAR oiCustomer            AS INT  NO-UNDO.
DEF VAR ldAmount              AS DEC  NO-UNDO.
DEF VAR lcError               AS CHAR NO-UNDO. 
DEF VAR liPPRequest           AS INT  NO-UNDO.
DEF VAR lcFatGroup            AS CHAR NO-UNDO. 
DEF VAR lcTaxZone             AS CHAR NO-UNDO.
DEF VAR liReqStatus           AS INT  NO-UNDO.
DEF VAR lcErrorTxt            AS CHAR NO-UNDO.
DEF VAR ldaOrderDate          AS DATE NO-UNDO.
DEF VAR liOrderTime           AS INT  NO-UNDO.
DEF VAR liRuleId              AS INT  NO-UNDO.
DEF VAR liRequest             AS INT  NO-UNDO.
DEF VAR lcResult              AS CHAR NO-UNDO.
DEF VAR llCorporate           AS LOG  NO-UNDO.
DEF VAR llDefBarring          AS LOG  NO-UNDO.
DEF VAR ldeSMSStamp           AS DEC  NO-UNDO.
DEF VAR lcBundleId            AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS4SubsType AS CHAR NO-UNDO. 
DEF VAR liDSSPriMsSeq         AS INT  NO-UNDO.
DEF VAR lcDSSBundleId         AS CHAR NO-UNDO.
DEF VAR lcDSSId               AS CHAR NO-UNDO. 
DEF VAR llgMatrixAvailable    AS LOG  NO-UNDO. 
DEF VAR lcBundleCLITypes      AS CHAR NO-UNDO.
DEF VAR lcReplacedTxt         AS CHAR NO-UNDO.
DEF VAR lcMandateId           AS CHAR NO-UNDO. 
DEF VAR ldaMandateDate        AS DATE NO-UNDO. 
DEF VAR ldLastDate            AS DATE NO-UNDO. 
DEF VAR lcInitialBarring      AS CHAR NO-UNDO. 
DEF VAR ldReqActDate          AS DATE NO-UNDO.
DEF VAR liReqActTime          AS INT  NO-UNDO.
DEF VAR ldeActivationTS       AS DEC  NO-UNDO.
DEF VAR ldaActDate            AS DATE NO-UNDO. 
DEF VAR lcMobileNumber        AS CHAR NO-UNDO. 
DEF VAR llgPriDSSMobSub       AS LOG  NO-UNDO.
DEF VAR liOngoingOrderId      AS INT  NO-UNDO.
DEF VAR liExistingOrderId     AS INT  NO-UNDO. 
DEF VAR liMLMsSeq             AS INT  NO-UNDO. 
DEF VAR lcProductCLIType      AS CHAR NO-UNDO.
DEF VAR lcProductCLI          AS CHAR NO-UNDO.
DEF VAR lcOrderSubICC         AS CHAR NO-UNDO.
DEF VAR lcTopupPrefix         AS CHAR NO-UNDO.
DEF VAR lcTopupReference      AS CHAR NO-UNDO.
DEF VAR liSubscriptionProductId AS INTE NO-UNDO.
DEF VAR llIsFixedOnly         AS LOGI NO-UNDO.

DEF BUFFER bInvCust        FOR Customer.
DEF BUFFER bRefCust        FOR Customer.
DEF BUFFER bRefMobsub      FOR Mobsub.
DEF BUFFER bRefCLIType     FOR CLIType.
DEF BUFFER bMsRequest      FOR MsRequest.
DEF BUFFER lbOrder         FOR Order.
DEF BUFFER lbMobSub        FOR MobSub.
DEF BUFFER lbOrderCustomer FOR OrderCustomer.
DEF BUFFER lbMobSubAD1     FOR MobSub.
DEF BUFFER bTerMsRequest   FOR MsRequest.
DEF BUFFER bMsOwner        FOR MsOwner.
DEF BUFFER lbMLOrder       FOR Order.
DEF BUFFER lbMLMobSub      FOR MobSub.
DEF BUFFER lbELMobSub      FOR MobSub. 
DEF BUFFER lbMobSubs       FOR MobSub.
DEF BUFFER lbPriDSSMobSub  FOR MobSub.
DEF BUFFER lbELOrderAction FOR OrderAction.
DEF BUFFER lbOngOrder      FOR Order.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}
   DEFINE VARIABLE lhMsOwner AS HANDLE NO-UNDO.
   lhMsOwner = BUFFER MSOwner:HANDLE.
   RUN StarEventInitialize(lhMsOwner).
END.

FUNCTION fCreatePoUser RETURNS LOGICAL
   (iiMsSeq AS INTEGER,
    BUFFER ibOrderCustomer FOR OrderCustomer):

   DEFINE VARIABLE liLineType AS INTEGER NO-UNDO.

   CASE ibOrderCustomer.RowType:
      WHEN {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}
      THEN liLineType = 1.
      WHEN {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}
      THEN liLineType = 2.
      OTHERWISE RETURN TRUE.
   END CASE.

   FIND FIRST PoUser EXCLUSIVE-LOCK WHERE
      PoUser.MsSeq    = iiMsSeq AND
      PoUser.LineType = liLineType
   NO-ERROR.

   IF NOT AVAILABLE PoUser
   THEN CREATE PoUser.

   ASSIGN
      PoUser.MsSeq     = iiMsSeq
      PoUser.LineType  = liLineType
      PoUser.Company   = ibOrderCustomer.Company WHEN ibOrderCustomer.CustIdType EQ "CIF"
      PoUser.CompanyId = ibOrderCustomer.CustId WHEN ibOrderCustomer.CustIdType EQ "CIF"
      PoUser.CustName  = OrderCustomer.Surname1
      PoUser.SurName2  = OrderCustomer.Surname2
      PoUser.FirstName = OrderCustomer.FirstName.

   IF ibOrderCustomer.CustIdType EQ "CIF"
   THEN ASSIGN
           PoUser.PersonId     = ibOrderCustomer.AuthCustId
           PoUser.PersonIdType = ibOrderCustomer.AuthCustIdType.
   ELSE ASSIGN
           PoUser.PersonId     = ibOrderCustomer.CustId
           PoUser.PersonIdType = ibOrderCustomer.CustIdType.

   RETURN FALSE.

END FUNCTION.

FIND FIRST MSRequest WHERE 
           MSRequest.MSrequest = iiMSrequest
EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR
   (MsRequest.ReqType NE {&REQTYPE_SUBSCRIPTION_CREATE} AND 
    MsRequest.ReqType NE {&REQTYPE_FIXED_LINE_CREATE}) THEN 
    RETURN "ERROR".
   
liReqStatus = MsRequest.ReqStatus.

IF MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_CREATE} AND
   CAN-FIND(FIRST bMsRequest NO-LOCK WHERE
                  bMsRequest.MsSeq = MsRequest.MsSeq AND
                  bMsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE} AND
    LOOKUP(STRING(bMsRequest.ReqStatus),
           {&REQ_INACTIVE_STATUSES}) = 0) THEN
   RETURN "ERROR".

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
      
FIND FIRST order WHERE
           Order.MSSeq = MSRequest.MSSeq
EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

ASSIGN liSubscriptionProductId = INT(MsRequest.ReqCParam5).

IF liSubscriptionProductId > 0 THEN
DO: 
    ASSIGN 
        lcProductCLIType = Func.OrderProductsData:mGetOrderProductCLIType(liSubscriptionProductId)
        llIsFixedOnly    = Func.ValidateOrder:mIsFixedOnlyTariff(lcProductCLIType).

    IF llIsFixedOnly THEN
        ASSIGN  
            lcProductCLI  = Func.OrderProductsData:mGetOrderProductFixedNumber(liSubscriptionProductId)
            lcOrderSubICC = "".
    ELSE
        ASSIGN  
            lcProductCLI  = Func.OrderProductsData:mGetOrderProductCLI(liSubscriptionProductId)
            lcOrderSubICC = Func.OrderProductsData:mGetOrderMobileICC(Order.OrderID, liSubscriptionProductId).
END.        
ELSE 
    ASSIGN 
        lcProductCLIType = Order.CLIType
        lcProductCLI     = Order.CLI
        lcOrderSubICC    = Order.ICC.

RUN check-order(output lcErrorTxt).

IF lcErrorTxt > "" THEN DO:
   /* locked, try again */
   IF lcErrorTxt BEGINS "Info" THEN DO:
      fReqStatus(liReqStatus,"").
      RETURN.
   END.
   /* stop handling */
   ELSE DO:
      fReqError(lcErrorTxt).
      RETURN.
   END.
END.

IF fIsConvergenceTariff(lcProductCLIType) THEN DO:

   FIND orderfusion NO-LOCK WHERE
        orderfusion.Brand = Syst.Var:gcBrand AND
        orderfusion.orderid = order.orderid NO-ERROR.
   IF NOT AVAIL orderfusion THEN DO:
      fReqError("OrderFusion not found").
      RETURN. 
   END.
END.
ELSE RELEASE OrderFusion.
   
FIND FIRST Mobsub WHERE 
           Mobsub.MSSEq = MSRequest.Msseq
NO-LOCK NO-ERROR.

IF AVAIL OrderFusion AND
   MSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_CREATE} THEN DO:

   IF NOT AVAIL mobsub THEN DO:
      fReqError("Subscription not found").
      RETURN. 
   END.           

   FIND CURRENT mobsub EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF LOCKED mobsub THEN RETURN.

   FIND Customer NO-LOCK WHERE
        Customer.Custnum = mobsub.Custnum.

END.
ELSE DO:

   IF AVAIL mobsub THEN DO:
      fReqError("Subscription already exists").
      RETURN. 
   END.           

   FIND FIRST Mobsub WHERE 
              Mobsub.CLI = lcProductCLI
   NO-LOCK NO-ERROR.

   IF AVAIL mobsub THEN DO: 
      fReqError("Subscription with the same MSISDN already exists").
      RETURN. 
   END.           
   
   IF AVAIL OrderFusion THEN DO:
      IF CAN-FIND(
         FIRST Mobsub WHERE 
               Mobsub.Brand = Syst.Var:gcBrand AND
               Mobsub.FixedNumber = orderfusion.fixednumber) THEN DO:
         fReqError("Subscription with the same fixed number already exists").
         RETURN. 
      END.
   END.
END.

/*YDR-1824
AC1: Request activation time is used as a beginning of a subscription timestamps if the request handling time is the same day than activation date. 
AC2: First second of subscription handling date is used as a beginning of subscription timestamps if the request handling time is not the same day than activation date.*/
Func.Common:mSplitTS(MsRequest.ActStamp,
                     ldReqActDate,
                     liReqActTime).

IF ldReqActDate = TODAY THEN
   ASSIGN ldeActivationTS = MSRequest.ActStamp.
ELSE
   ASSIGN ldeActivationTS = Func.Common:mMake2DT(TODAY,1).

IF lcProductCLIType EQ "TARJ5" THEN DO:

   RUN Gwy/air_update_serviceclass.p(lcProductCLI,
                                 {&SC_TARJ5_NORMAL},
                                 {&SC_TARJ5_PROMOTIONAL},
                                 TODAY + 31,
                                 OUTPUT lcResult).
   IF lcResult BEGINS "ERROR" THEN DO:
      fReqError("Promotional price activation failed: " + lcResult).
      RETURN.
   END.
END.
   
IF NOT AVAIL mobsub THEN DO:
   
   IF NOT llIsFixedOnly THEN
   DO: 
       FIND FIRST MSISDNNumber WHERE
                  MSISDNNumber.CLI   = lcProductCLI
       EXCLUSIVE-LOCK NO-ERROR.
                
       IF NOT AVAIL msisdnNumber THEN DO:
          CREATE MSISDNNumber.
          ASSIGN MSISDNNumber.CLI = lcProductCLI.
       END.

       FIND FIRST MSISDN WHERE 
                  MSISDN.Brand    = Syst.Var:gcBrand    AND 
                  MSISDN.CLI      = lcProductCLI        AND 
                  MSISDN.ValidTo >= Func.Common:mMakeTS() 
       EXCLUSIVE-LOCK NO-ERROR.

       IF NOT AVAIL MSISDN THEN DO:

          CREATE MSISDN.
          ASSIGN
             MSISDN.CLI         = lcProductCLI
             MSISDN.Brand       = Syst.Var:gcBrand
             MSISDN.Stat        = 3.

          ASSIGN 
             MSISDN.ValidFrom   = ldeActivationTS 
             MSISDN.ActionDate  = Today.
       END.

       ELSE DO:
          
          fMakeMsidnHistory(INPUT RECID(MSISDN)).
          MSISDN.Stat        = 3.
       END.
   END.

   RUN Mm/createcustomer.p(INPUT Order.OrderId,1,FALSE,TRUE,output oiCustomer).

   ASSIGN Msrequest.CustNum = oiCustomer.

   RUN Mm/createcustomer.p(INPUT Order.OrderId,3,FALSE,TRUE,output oicustomer).

   FIND FIRST CLIType WHERE
              CLIType.CliType = lcProductCLIType 
   NO-LOCK NO-ERROR.

   IF Msrequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE}
   THEN RELEASE IMSI.
   ELSE FIND FIRST imsi NO-LOCK WHERE 
                   imsi.icc = lcOrderSubICC NO-ERROR.

   CREATE MobSub. 

   ASSIGN
      Mobsub.ActivationTS  = ldeActivationTS
      MobSub.MsSeq         = Order.MSSeq .

   llCorporate = CAN-FIND(OrderCustomer OF Order WHERE
                          OrderCustomer.RowType = 1 AND
                          OrderCustomer.CustIdType = "CIF").

   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand   = Syst.Var:gcBrand   AND
            OrderCustomer.OrderID = Order.OrderID:

      CASE OrderCustomer.RowType:
         WHEN {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
         THEN MobSub.AgrCust = OrderCustomer.CustNum.
         WHEN {&ORDERCUSTOMER_ROWTYPE_INVOICE}
         THEN MobSub.InvCust = OrderCustomer.CustNum.
         WHEN {&ORDERCUSTOMER_ROWTYPE_USER}
         THEN MobSub.CustNum = OrderCustomer.CustNum.
         WHEN {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
         THEN MobSub.TerritoryOwner = OrderCustomer.TerritoryOwner.
         WHEN {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER} OR
         WHEN {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}
         THEN fCreatePoUser(Order.MSSeq, BUFFER OrderCustomer).
      END CASE.
      
      /* Create contact data for corporate customers */
      IF llCorporate AND 
         (OrderCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} OR
          OrderCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT})
      THEN DO:
         
         RUN Mm/createcustcontact.p(
             Order.OrderId,
             MsRequest.Custnum,
             OrderCustomer.RowType,
             OUTPUT lcError).
      
         /* write possible error to an order memo */
         IF lcError > "" THEN DO:
            Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             msrequest.custnum,
                             "CUSTOMER CONTACT CREATION FAILED",
                             lcError).
         END.
      END.
   END.

   IF MobSub.InvCust = 0 THEN MobSub.InvCust = MobSub.AgrCust.
   IF MobSub.CustNum = 0 THEN DO:
      CASE Order.UserRole:
      WHEN 2 THEN MobSub.CustNum = MobSub.InvCust.
      OTHERWISE   MobSub.CustNum = MobSub.AgrCust.
      END CASE. 
   END.   

   FIND FIRST Customer WHERE
              Customer.Custnum = MobSub.CustNum NO-LOCK NO-ERROR.

   IF Customer.DataProtected = TRUE THEN DO:
      FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN Customer.DataProtected = FALSE.
      FIND CURRENT Customer NO-LOCK NO-ERROR.
   END.

   FIND FIRST BillTarget NO-LOCK WHERE 
              BillTarget.CustNum    = Customer.CustNum AND
              BillTarget.BillTarget = CliType.BillTarget 
   NO-ERROR.

   IF NOT AVAIL BillTarget THEN DO:
      CREATE BillTarget.
      ASSIGN
         BillTarget.CustNum    = Customer.CustNum
         BillTarget.BillTarget = CliType.BillTarget
         BillTarget.DiscPlan   = CliType.DiscPlan
         BillTarget.RatePlan   = CliType.PricePlan.
   END.

   ASSIGN
      MobSub.CLI              = lcProductCLI
      MobSub.FixedNumber      = orderfusion.FixedNumber WHEN AVAIL orderfusion
      Mobsub.icc              = lcOrderSubICC WHEN MsRequest.ReqType NE {&REQTYPE_FIXED_LINE_CREATE}
      Mobsub.Brand            = Order.Brand 
      Mobsub.MsStatus         = (IF NOT llIsFixedOnly AND MsRequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE} THEN 
                                    {&MSSTATUS_MOBILE_PROV_ONG} 
                                 ELSE 
                                    {&MSSTATUS_ACTIVE})
      MobSub.Reseller         = Order.Reseller
      Mobsub.Paytype          = (CLIType.PayType = 2)
      Mobsub.salesman         = Order.salesman 
      Mobsub.MNPChannel       = 2 
      Mobsub.CreationDate     = TODAY
      Mobsub.CLIType          = lcProductCLIType    
      Mobsub.BillTarget       = Clitype.BillTarget
      Mobsub.SimDelStatus     = 2
      Mobsub.Activationdate   = TODAY
      Mobsub.IDCode           = STRING(RANDOM(0,9999),"9999")
      MobSub.TariffActDate    = TODAY
      MobSub.TariffActTS      = ldeActivationTS.


   /* CDS-8 start */
   fUpdateAccountID(Customer.CustNum).
   /* CDS-8 end */

   /* Extra line */
   /* In case of extra line discount subcription creation,
      following fields has to be updated */
   IF Order.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} THEN DO:
      FIND FIRST lbMLOrder NO-LOCK WHERE 
                 lbMLOrder.Brand      EQ Syst.Var:gcBrand AND 
                 lbMLOrder.OrderId    EQ Order.MultiSimID AND 
          LOOKUP(lbMLOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) = 0 NO-ERROR. /* Mainline Orderid */

      IF AVAIL lbMLOrder THEN 
         FIND FIRST lbMLMobSub NO-LOCK WHERE 
                    lbMLMobSub.MsSeq EQ lbMLOrder.MsSeq NO-ERROR.

      IF AVAIL lbMLMobSub THEN 
         ASSIGN MobSub.MultiSimID   = lbMLMobSub.MsSeq         /* Mainline Subid  */
                MobSub.MultiSimType = Order.MultiSimType.      /* Extraline = 3   */
      ELSE DO:
         
         liExistingOrderId = fCheckExistingMainLineAvailForExtraLine(lcProductCLIType,
                                                                     Customer.CustIdType,
                                                                     Customer.OrgId,
                                                                     OUTPUT liMLMsSeq).
         IF liExistingOrderId > 0 THEN 
            FIND FIRST lbMLMobSub NO-LOCK WHERE
                       lbMLMobSub.MsSeq EQ liMLMsSeq NO-ERROR.

         IF AVAIL lbMLMobSub THEN 
            ASSIGN MobSub.MultiSimID   = lbMLMobSub.MsSeq         /* Mainline Subid  */
                   MobSub.MultiSimType = Order.MultiSimType.      /* Extraline = 3   */
         ELSE DO:

            liOngoingOrderId = fCheckOngoingMainLineAvailForExtraLine(lcProductCLIType,
                                                                      Customer.CustIdType,
                                                                      Customer.OrgId).
            IF liOngoingOrderId > 0 THEN 
            DO:
               FIND FIRST lbOngOrder NO-LOCK WHERE
                          lbOngOrder.Brand   EQ Syst.Var:gcBrand AND
                          lbOngOrder.OrderId EQ liOngoingOrderId NO-ERROR.
               IF AVAILABLE lbOngOrder THEN
                  ASSIGN MobSub.MultiSimID   = lbOngOrder.MsSeq
                         MobSub.MultiSimType = Order.MultiSimType.
            END.
            ELSE DO:
               ASSIGN MobSub.MultiSimID   = 0
                      MobSub.MultiSimType = 0.
               
               FIND FIRST lbELOrderAction EXCLUSIVE-LOCK WHERE
                          lbELOrderAction.Brand    EQ Syst.Var:gcBrand       AND
                          lbELOrderAction.OrderID  EQ Order.OrderID          AND
                          lbELOrderAction.ItemType EQ "ExtraLineDiscount"    AND
                          lbELOrderAction.ItemKey  EQ lcProductCLIType + "DISC" NO-ERROR.
      
               IF AVAILABLE lbELOrderAction THEN DO:
                  DELETE lbELOrderAction.
                  Func.Common:mWriteMemo("Order",
                                          STRING(Order.OrderID),
                                          0,
                                          "EXTRA LINE DISCOUNT REMOVED",
                                          "Removed ExtraLineDiscount Item from OrderAction").
               END.
            END.
         END.

      END.
   END.
 
   IF Avail imsi THEN Mobsub.imsi = IMSI.IMSI.

    /* Topup Requests initial & campaign */
    lcTaxZone = "".
   FOR EACH OrderTopup OF Order NO-LOCK:

      IF lcTaxZone = "" THEN DO:
         FIND bInvCust WHERE bInvCust.CustNum = MobSub.InvCust NO-LOCK.
         lcTaxZone = fRegionTaxZone(bInvCust.Region).
      END.
         
      IF OrderTopup.TopupType  = {&INITIAL_TOPUP} THEN
          ASSIGN 
              lcTopupPrefix = Func.Common:mGetTMSParamValue(INPUT "TOPUP",INPUT "InitialTopupPrefix",INPUT {&CHARACTER_PARAM}).  
      ELSE           
          ASSIGN 
              lcTopupPrefix = Func.Common:mGetTMSParamValue(INPUT "TOPUP",INPUT "CampaignTopupPrefix",INPUT {&CHARACTER_PARAM}).          
      
      ASSIGN 
          lcTopupReference = Func.OrderProductsData:mGetOrderProductOfferingID(INPUT OrderTopup.OrderID , INPUT OrderTopup.OrderProductID).
      
      IF lcTopupReference = "" THEN ASSIGN lcTopupReference = "CAMPAIGN".
         
      fCreateTopUpRequest(MobSub.MsSeq,
                          MobSub.CLI,
                          "RefillTRequest",
                          "WEB Order",
                          "RefillTRequest",
                          lcTopupPrefix,
                          lcTopupReference,
                          lcTaxZone,
                          0,
                          OrderTopup.Amount * 100,
                          OrderTopup.VatAmount * 100).
   END.
   
   CREATE Segmentation.
   ASSIGN
      Segmentation.MsSeq   = Order.MsSeq
      Segmentation.SegmentCode  = "SN"
      Segmentation.SegmentOffer = "OFF"
      Segmentation.SegmentDate  = TODAY
      Segmentation.SegmentCreation = Func.Common:mMakeTS().
   
   CREATE msowner.
   ASSIGN
      MSOwner.CLI       = Mobsub.cli
      MSOwner.FixedNumber = Mobsub.FixedNumber
      MSOwner.CustNum   = Mobsub.CustNum
      MSOwner.MsSeq     = MobSub.MsSeq
      MSOwner.TsBegin   = ldeActivationTS
      msowner.tsend      = 99999999.99999
      MSOwner.BillTarg   = Mobsub.BillTarg
      MSOwner.Brand      = mobsub.Brand
      Msowner.AgrCust    = Mobsub.AgrCust
      Msowner.InvCust    = Mobsub.InvCust
      MSOwner.IMSI       = MobSub.IMSI
      MSowner.Paytype    = MObsub.Paytype
      MSOwner.clitype    = mobsub.clitype
      MSOWner.clievent   = (IF NOT llIsFixedOnly AND MsRequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE} THEN 
                               "F" 
                            ELSE 
                               "C")
      Msowner.InPortOper = Order.CurrOper.

      IF fGetOrderMandateId(BUFFER Order,
                            OUTPUT lcMandateId,
                            OUTPUT ldaMandateDate)
      THEN ASSIGN
         MsOwner.MandateId   = lcMandateId
         MsOwner.MandateDate = ldaMandateDate.
              
   IF llDoEvent THEN fMakeCreateEvent((BUFFER MsOwner:HANDLE),
                                      "",
                                      Syst.Var:katun,
                                      "").

   IF AVAIL orderfusion THEN
      RUN pCopyPackage(MobSub.CLIType,
                       "TMSService",
                       "",
                       MobSub.MSSeq,
                       MobSub.ActivationDate,
                       TRUE,    /* only those that don't already exist */
                       FALSE,   /* create fees */
                       FALSE,   /* solog (this is used for new mobsubs only) */
                       0,
                       FALSE,
                       OUTPUT liRequest).

   /* default counter limits */
   IF MobSub.PayType = FALSE THEN DO:
      
      fTMRLimit2Subscription(MobSub.MsSeq).

      _fCreateNewInvoiceTarget(
         MobSub.MsSeq,
         MobSub.Custnum,
         MobSub.CLiType,
         OUTPUT lcError).

      IF lcError NE "" THEN 
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "Invoice target creation failed",
                          lcError).

      fSetSpecialTTFLimit(MobSub.Custnum,
                          MobSub.CLIType).
   END.

   IF MsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE} THEN
      RUN Mm/requestaction_exec.p (MsRequest.MsRequest,
                                MobSub.CLIType,
                                Order.OrderID,
                                MobSub.ActivationTS,
                                MobSub.ActivationTS,
                                TRUE,                   /* create fees */
                                {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                                {&REQUEST_ACTIONLIST_ALL}).

   /* Welcome SMS for all tariffs (Excluding Corporate) */
   IF LOOKUP(Customer.category,"20,21,22,40,41,42,43") = 0 THEN DO:

      lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
      
       /* prepais tariffs have their own Welcome SMSes */
       IF LOOKUP(MobSub.CliType, "TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN
          lcSMSText = fGetSMSTxt(MobSub.CliType + "Act",
                                 TODAY,
                                 Customer.Language,
                                 OUTPUT ldeSMSStamp).
       ELSE DO:

         lcSMSText = fGetSMSTxt((IF MsRequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE}
                                 THEN "WelcomeConvergent"
                                 ELSE "WelcomeSubs"),
                                TODAY,
                                Customer.Language,
                                OUTPUT ldeSMSStamp).   

         IF LOOKUP(MobSub.CLIType,lcBundleCLITypes) > 0 THEN DO:
            lcBundleId = fGetDataBundleInOrderAction(Order.OrderId,MobSub.CLIType).
            lcReplacedTxt = fConvBundleToBillItem(lcBundleId).
            lcReplacedTxt = fGetItemName(Syst.Var:gcBrand,
                                         "BillItem",
                                         lcReplacedTxt,
                                         Customer.Language,
                                         TODAY).
         END.
         ELSE 
            lcReplacedTxt = fGetItemName(Syst.Var:gcBrand,
                                         "CLIType",
                                         MobSub.CLIType,
                                         Customer.Language,
                                         TODAY).

         ASSIGN lcSMSText = REPLACE(lcSMSText,"#CLITYPE",lcReplacedTxt)
                lcSMSText = REPLACE(lcSMSText,"#CLI", MobSub.CLI)
                lcSMSText = REPLACE(lcSMSText,"#FIXED_NUMBER",
                              MobSub.FixedNumber) WHEN MobSub.FixedNumber NE ?.
      END.

      IF lcSMSText > "" THEN DO:
         
         IF MsRequest.ReqType = {&REQTYPE_FIXED_LINE_CREATE} THEN DO:
   
            IF Order.OrderType EQ {&ORDER_TYPE_MNP} THEN
               lcMobileNumber = MobSub.CLI.
            ELSE DO:
            
               FIND OrderCustomer OF Order NO-LOCK WHERE
                    OrderCustomer.RowType = 1.

               IF fIsMobileNumber(OrderCustomer.MobileNumber) THEN
                  lcMobileNumber = OrderCustomer.MobileNumber.
               ELSE IF fIsMobileNumber(OrderCustomer.FixedNumber) THEN
                  lcMobileNumber = OrderCustomer.FixedNumber.
            END.
         END.
         ELSE lcMobileNumber = MobSub.CLI.
      
         IF lcMobileNumber > "" THEN
            fMakeSchedSMS2(MobSub.CustNum,
                           lcMobileNumber,
                           {&SMSTYPE_INFO},
                           lcSMSText,
                           ldeSMSStamp,
                           "22622",
                           "").
      END.
   END. /* IF LOOKUP(Customer.category,"20,40,41") = 0 THEN DO: */

   IF MsRequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE} THEN DO:
      fUpdateCustomerInstAddr(Order.OrderID).
      RUN Mm/orderaction_exec.p (MobSub.MsSeq,
                      Order.OrderID,
                      ldeActivationTS,
                      MsRequest.MsRequest,
                      {&REQUEST_SOURCE_SUBSCRIPTION_CREATION}). 
      fReqStatus(2,"").
      IF NOT llIsFixedOnly THEN 
      DO:
         IF liSubscriptionProductId > 0 THEN
            fSetOrderProductStatus(Order.OrderId, liSubscriptionProductId, {&ORDER_STATUS_DELIVERED}). 
         RETURN.
      END.   
   END.

END.
ELSE DO:
   
   FIND FIRST imsi WHERE 
              imsi.icc = lcOrderSubICC
   NO-LOCK NO-ERROR.

   FIND FIRST MSOwner EXCLUSIVE-LOCK WHERE
              MSOwner.MsSeq   = Mobsub.MsSeq
        USE-INDEX MsSeq NO-ERROR.

   Func.Common:mTS2Date(MSOwner.TSBegin, OUTPUT ldaActDate).

   IF ldaActDate NE TODAY THEN DO:

      IF llDoEvent THEN DO:
         RUN StarEventSetOldBuffer (lhMsOwner).
      END.

      CREATE bMsOwner.
      BUFFER-COPY MSOwner EXCEPT TSBegin TSEnd CLIEvent TO bMsOwner.
      ASSIGN
         MsOwner.TSend = Func.Common:mSecOffSet(ldeActivationTS, -1) 
         bMsOwner.TSBegin = ldeActivationTS 
         bMsOwner.TSEnd = 99999999.99999.

      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent (lhMsOwner).
         fMakeCreateEvent((BUFFER bMsOwner:HANDLE),
                           "",Syst.Var:katun, "").
         fCleanEventObjects().
      END.

      FIND MSOwner EXCLUSIVE-LOCK WHERE
         ROWID(MSOwner) = ROWID(bMsOwner).
   END.

   ASSIGN
      MsRequest.Custnum = Customer.Custnum
      MsOwner.imsi      = IMSI.IMSI WHEN AVAIL IMSI
      MsOwner.CLIEvent  = "C"
      Mobsub.MsStatus   = {&MSSTATUS_ACTIVE}
      Mobsub.Icc        = lcOrderSubICC
      Mobsub.imsi       = IMSI.IMSI WHEN AVAIL IMSI.

END.


/* If pending additional line orders are available then release them */  
FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand   = Syst.Var:gcBrand                   AND
           OrderCustomer.OrderId = Order.OrderId                      AND
           OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

IF AVAIL OrderCustomer THEN DO:
   fActionOnAdditionalLines (OrderCustomer.CustIdType,
                             OrderCustomer.CustID,
                             MobSub.CLIType,
                             FALSE,
                             "RELEASE"). 

   IF fCLITypeIsMainLine(MobSub.CLIType) THEN DO:
      
      /* Mainline Associated extraline has to be released when 
         mainline is (Fixed + Mobile line) is delivered */
      fActionOnExtraLineOrders(Order.OrderId,    /* Main line Order Id  */
                               "RELEASE").       /* Action              */

      /* Check for orphan extraline for customer, if available 
         then assign to the mainline */ 
      fCheckAndAssignOrphanExtraline(MobSub.MsSeq,   
                                     MobSub.CustNum,
                                     MobSub.CLIType). 
                               
   END.                            

END.

fSetOrderStatus(Order.OrderId,"6").  

IF liSubscriptionProductId > 0 THEN
   fSetOrderProductStatus(Order.OrderId, liSubscriptionProductId, {&ORDER_STATUS_DELIVERED}).

fMarkOrderStamp(Order.OrderID,
                "Delivery",
                Func.Common:mMakeTS()).

/* default services */
RUN Mm/copysp.p(MobSub.MsSeq,
           TODAY,
           TRUE,   /* new subs */
           TRUE).  /* silent */

/* clitype spesific fees */
IF AVAIL CliType AND CliType.FeeModel1 > "" THEN DO:
   RUN Mc/creasfee.p (MobSub.CustNum,
                 MobSub.MsSeq,
                 Today,
                 "MobSub",
                 CliType.FeeModel1,
                 1,
                 ?,
                 "",    /* memo   */
                 FALSE,           /* no messages to screen */
                 Syst.Var:katun,
                 "SubscriptionCreation",
                 0,
                 "",
                 "",
                 OUTPUT lcCharValue).
END.

/* general fees */
RUN Mc/creasfee.p (MobSub.CustNum,
              MobSub.MsSeq,
              Today,
              "MobSub",
              "MONTHLYFEE",
              1,
              ?,
              "",    /* memo   */
              FALSE,           /* no messages to screen */
              Syst.Var:katun,
              "SubscriptionCreation",
              0,
              "",
              "",
              OUTPUT lcCharValue).

/* add initial fees and additional cost (delivery charge) and cash invoice */
IF Order.InvNum > 0 THEN
   RUN Mc/cashfee.p (Order.OrderID,
             1,                     /* action 1=create fees */
             OUTPUT lcCharValue,
             OUTPUT ldAmount,
             OUTPUT lcError).

/* write possible error to an order memo */
IF lcError BEGINS "Error" THEN DO:
   Func.Common:mWriteMemo("Order",
                    STRING(Order.OrderID),
                    MobSub.CustNum,
                    "CASH INVOICE FAILED",
                    lcError).
END.

/* terminals, do this before periodical contract creation */
fCreateSubsTerminal(BUFFER Order).
 
/* fatime is granted in a campaign */
IF Order.FatAmount NE 0 OR Order.FtGrp > "" THEN DO:

   IF Order.FtGrp > "" AND 
      CAN-FIND(FIRST FatGroup WHERE
                     FatGroup.Brand = Syst.Var:gcBrand AND
                     FatGroup.FtGrp = Order.FtGrp)
   THEN lcFatGroup = Order.FtGrp.
   ELSE lcFatGroup = fCParamC("OrderCampaignFat").
    
   IF lcFatGroup = ? OR lcFatGroup = "" THEN 
      lcError = "FATime group for campaign not defined".
      
   ELSE RUN Mc/creafat.p (MobSub.CustNum,
                     MobSub.MsSeq,
                     lcFatGroup,
                     Order.FatAmount,
                     0,
                     ?,
                     YEAR(TODAY) * 100 + MONTH(TODAY),
                     999999,
                     OUTPUT lcError).

   /* write possible error to an order memo */
   IF lcError > "" THEN DO:
      Func.Common:mWriteMemo("Order",
                       STRING(Order.OrderID),
                       MobSub.CustNum,
                       "FATIME CREATION FAILED",
                       lcError).
   END.
END.

/* Create Default shaper, if there is no bundle with subscription */
/* this must be executed before calling orderaction_exec */
FIND FIRST OrderAction WHERE
           OrderAction.Brand    = Syst.Var:gcBrand AND
           OrderAction.OrderId  = Order.OrderID AND
           OrderAction.ItemType = "BundleItem" AND
           OrderAction.ItemKey NE {&DSS} NO-LOCK NO-ERROR.

IF NOT AVAIL OrderAction AND
   LOOKUP(MobSub.CLIType,"CONT6,TARJRD1,CONT7,CONT8,CONTS,CONTFF,CONTSF,CONT9,CONT10,CONT15,CONT24,CONT23,CONT25,CONT26,CONT27,CONT31,CONT33,CONT34") = 0 AND
   NOT MobSub.CLIType BEGINS "CONTFH" AND
   NOT MobSub.CLITYpe BEGINS "CONTDSL" THEN DO:

   RUN pCopyPackage(MobSub.CLIType,
                    "SHAPER",
                    "",
                    MobSub.MSSeq,
                    TODAY,
                    ?,
                    FALSE,  /* create fees */
                    TRUE,   /* solog (provisioning) */
                    MsRequest.MsRequest,
                    FALSE,   /* mandatory subrequest */
                    OUTPUT liRequest).
   IF liRequest = 0 THEN
      /* write possible error to a memo */
      Func.Common:mWriteMemo("MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.Custnum,
                       "DEFAULT SHAPER ACTIVATION FAILED",
                       "DEFAULT SHAPER ACTIVATION FAILED").
END. /* IF NOT AVAIL OfferItem AND NOT AVAIL OrderAction THEN DO: */

/* initial topup, fatime, per.contracts from offer */
IF Order.Offer > "" THEN 
   RUN Mc/offeritem_exec.p (MobSub.MsSeq,
                       Order.OrderID,
                       ldeActivationTS,
                       MsRequest.MsRequest,
                       {&REQUEST_SOURCE_SUBSCRIPTION_CREATION}).

/* activate periodical contracts, service packages etc. */
RUN Mm/requestaction_exec.p (MsRequest.MsRequest,
                          MobSub.CLIType,
                          Order.OrderID,
                          ldeActivationTS,
                          ldeActivationTS,
                          TRUE,                   /* create fees */
                          {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                          {&REQUEST_ACTIONLIST_ALL}).

/* per.contract and service package created with the order */
RUN Mm/orderaction_exec.p (MobSub.MsSeq,
                           Order.OrderID,
                           ldeActivationTS,
                           MsRequest.MsRequest,
                           {&REQUEST_SOURCE_SUBSCRIPTION_CREATION}).

/* CREATE - DSS group for eligible Postpaid subscriptions 
   ADD    - In case DSS group is already active for eligible Postpaid susbcriptions */
IF NOT MobSub.PayType THEN DO:

   ASSIGN
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
      lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE").
      llgMatrixAvailable    = fCheckActiveExtraLinePair(MobSub.MsSeq,
                                                        MobSub.CLIType,
                                                        OUTPUT lcDSSBundleId).

   lcBundleId = fGetActiveDSSId(INPUT MobSub.CustNum,INPUT Func.Common:mMakeTS()).

   IF (lcBundleId GT ""      AND lcDSSBundleId NE {&DSS4})              OR
      (lcBundleId EQ {&DSS4} AND lcDSSBundleId EQ {&DSS4})              OR
      CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.Brand      EQ Syst.Var:gcBrand      AND
                     MsRequest.ReqType    EQ {&REQTYPE_DSS}        AND
                     MsRequest.Custnum    EQ MobSub.CustNum        AND
                     MsRequest.ReqCParam1 EQ "CREATE"              AND
                     MsRequest.ActStamp   <= Func.Common:mMakeTS() AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"5,6,7,8") > 0) THEN
   DO:
      IF lcBundleId = {&DSS} OR
        (lcBundleId = {&DSS2}                              AND
         LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) GT 0 AND 
         NOT fCLITypeIsMainLine(MobSub.CLIType)            AND 
         NOT fCLITypeIsExtraLine(MobSub.CLIType))          THEN
         fDSSAddRequest(Mobsub.MsSeq,
                        lcBundleId,
                        MsRequest.MsRequest,
                        MsRequest.ReqSource,
                        0).
      ELSE IF llgMatrixAvailable                                       AND 
              ((lcBundleId EQ {&DSS2} AND lcDSSBundleId EQ {&DSS2}) OR  
               (lcBundleId EQ {&DSS4}))                                THEN DO:
         fDSSAddRequest(Mobsub.MsSeq,
                        lcBundleId,
                        MsRequest.MsRequest,
                        MsRequest.ReqSource,
                        0).
              
         /* Extraline Business Functionality                                            */
         /* Rule 1: In case of adding any Mainline subscription to existing DSS group,  */
         /*         then add its associated Extralines to the group.                    */
         /* Rule 2: In case of adding any Extraline subscription to existing DSS group, */
         /*         then its associated mainline DSS add request call has to be         */
         /*         done only once - when its first Extraline subscription is added     */
         IF fCLITypeIsMainLine(MobSub.CLIType) THEN DO:
             FOR EACH lbELMobSub NO-LOCK WHERE
                      lbELMobSub.Brand      EQ Syst.Var:gcBrand AND
                      lbELMobSub.MultiSimId EQ MobSub.MsSeq:
                fDSSAddRequest(lbELMobSub.MsSeq,
                               lcBundleId,
                               MsRequest.MsRequest,
                               MsRequest.ReqSource,
                               0).
             END.
         END.
         ELSE IF fCLITypeIsExtraLine(MobSub.CLIType)             AND 
                 fExtraLineCountForMainLine(MobSub.MultiSimId,
                                            MobSub.CustNum) EQ 1 THEN DO:
            fDSSAddRequest(MobSub.MultiSimId,
                           lcBundleId,
                           MsRequest.MsRequest,
                           MsRequest.ReqSource,
                           0).
         END.

      END.
   END.
   ELSE IF NOT fOngoingDSSAct(MobSub.CustNum) THEN DO:

      IF LOOKUP(MobSub.CLIType,lcAllowedDSS4SubsType) > 0  AND
         lcDSSBundleId EQ {&DSS4}                          AND
         fIsDSSActivationAllowed(MobSub.CustNum,
                                 MobSub.MsSeq,
                                 MobSub.ActivationTS,
                                 {&DSS4},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSId = {&DSS4}.
      ELSE IF LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
         lcDSSBundleId EQ {&DSS2}                              AND
         fIsDSSActivationAllowed(MobSub.CustNum,
                                 MobSub.MsSeq,
                                 MobSub.ActivationTS,
                                 {&DSS2},
                                 OUTPUT liDSSPriMsSeq,
                                 OUTPUT lcResult) THEN
         lcDSSId = {&DSS2}.               
      
      FIND FIRST lbMobSub WHERE
                 lbMobSub.MsSeq = liDSSPriMsSeq NO-LOCK NO-ERROR.
      
      IF AVAIL lbMobSub THEN DO:
         /* Functionality changed to deny DSS2 creation  
            if there is DSS2 termination request. YTS-8140 
            used lbMobSub.Custnum cause of ACC */
         FIND FIRST bTerMsRequest NO-LOCK USE-INDEX CustNum WHERE
                    bTerMsRequest.Brand      EQ Syst.Var:gcBrand AND
                    bTerMsRequest.ReqType    EQ 83               AND
                    bTerMsRequest.Custnum    EQ lbMobSub.Custnum AND
                    bTerMsRequest.ReqCParam3 BEGINS "DSS"        AND
                    bTerMsRequest.ReqCParam1 EQ "DELETE"         AND
                    LOOKUP(STRING(bTerMsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") EQ 0  NO-ERROR.

         IF NOT AVAIL bTerMsRequest THEN 
            liRequest = fDSSCreateRequest(lbMobSub.MsSeq,
                                          lbMobSub.CustNum,
                                          lcDSSId,
                                          {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                                          0,
                                          MobSub.ActivationTS,
                                          "DSS activation failed in Mobsub creation", /* Error Msg */
                                          OUTPUT lcResult).

      END.
   END.
END.

RUN pCreateCommission.

/* Default premium barring postpaid (YTD-33) & prepaid (YDR-157) */
llDefBarring = TRUE.
IF TODAY - Customer.CreDate > 60 THEN DO:

   INVSEARCH_LOOP:
   FOR EACH Invoice WHERE
            Invoice.Brand = Syst.Var:gcBrand AND
            Invoice.Custnum = Customer.Custnum AND
            Invoice.InvType = 1 NO-LOCK:
      IF Invoice.DueDate < TODAY - 30 AND
         Invoice.PaymState NE 2 AND
         Invoice.InvAmt > 0 THEN DO:
         llDefBarring = TRUE. /* unpaid invoice */
         LEAVE INVSEARCH_LOOP.
      END.
      llDefBarring = FALSE. /* invoice(s) found */
   END.
END.

IF llDefBarring THEN lcInitialBarring = "Prod_TotalPremium_Off=1,Y_BPSUB=1".
ELSE lcInitialBarring = "Y_BPSUB=1".

RUN Mm/barrengine.p(MobSub.MsSeq,
                lcInitialBarring,
                "1",                 /* source = subscr. creation  */
                Syst.Var:katun,               /* creator */
                Func.Common:mMakeTS(),           /* activate */
                "",                  /* sms */
                OUTPUT lcResult).

liRequest = 0.
liRequest = INTEGER(lcResult) NO-ERROR. 

IF liRequest = 0 THEN                               
   /* write possible error to a memo */
   Func.Common:mWriteMemo("MobSub",
                    STRING(MobSub.MsSeq),
                    MobSub.Custnum,
                    "INITIAL BARRING FAILED",
                    lcInitialBarring).
         
/* mark mnp process finished */
IF Order.MNPStatus > 0 THEN DO:
   FIND FIRST MNPProcess WHERE
         MNPProcess.OrderId = Order.OrderId AND
         MNPProcess.MNPType = ({&MNP_TYPE_IN}) AND
         MNPProcess.StatusCode = ({&MNP_ST_ACON}) EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL MNPProcess THEN DO:
      ASSIGN
         MNPProcess.UpdateTS = Func.Common:mMakeTS()
         MNPProcess.StatusCode = {&MNP_ST_APOR}.
      FIND CURRENT Order EXCLUSIVE-LOCK.
      Order.MNPStatus = {&MNP_ST_APOR} + 1.
      RELEASE MNPProcess.
   END.
END.

/* Override default national roaming profile */
IF Customer.NWProfile > 0 AND
   Customer.NWProfile NE {&CUSTOMER_NW_PROFILE_YG_OR} THEN DO:

   liRequest = fServiceRequest(
                  MobSub.MsSeq,
                  "NW",
                  Customer.NWProfile,
                  "", /* param */
                  Func.Common:mMakeTS(),
                  "", /* salesman */
                  TRUE,      /* fees */
                  FALSE,      /* sms */
                  "", /* usercode */
                  {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                  msrequest.msrequest, /* father request */
                  false, /* mandatory for father request */
                  OUTPUT lcerror).
   
   IF liRequest = 0 THEN                               
      /* write possible error to a memo */
      Func.Common:mWriteMemo("MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.Custnum,
                       "NW profile change failed",
                       lcError).
END.

IF Order.MultiSimID > 0 THEN DO:

   CASE Order.MultiSimType:
      WHEN {&MULTISIMTYPE_PRIMARY} THEN DO:
         FOR FIRST lbOrder NO-LOCK USE-INDEX MultiSIMId WHERE
                   lbOrder.Brand = Syst.Var:gcBrand AND
                   lbOrder.MultiSimID = Order.MultiSimID AND
                   lbOrder.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
                   lbOrder.StatusCode = {&ORDER_STATUS_PENDING_MAIN_LINE}:
            fSetOrderStatus(lbOrder.OrderId,{&ORDER_STATUS_NEW}).
         END.
      END.
      WHEN {&MULTISIMTYPE_SECONDARY} THEN DO:
         lcSMSText = fGetSMSTxt("MultiSIMSecondaryAct",
                                TODAY,
                                Customer.Language,
                                OUTPUT ldeSMSStamp).
            
         IF lcSMSText > "" THEN 
            fMakeSchedSMS2(mobsub.CustNum,
                           mobsub.CLI,
                           {&SMSTYPE_CONTRACT_ACTIVATION},
                           lcSMSText,
                           ldeSMSStamp,
                           "22622",
                           "").
      END.
   END.
END.

/* release pending secondary line orders, YDR-1089 */
IF CLIType.LineType > 0 AND
   CAN-FIND(FIRST OrderAction NO-LOCK WHERE
            OrderAction.Brand = Syst.Var:gcBrand AND
            OrderAction.OrderId = Order.OrderID AND
            OrderAction.ItemType = "BundleItem" AND
            CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.Brand = Syst.Var:gcBrand AND
                           CLIType.CLIType = OrderAction.ItemKey AND
                           CLIType.LineType = {&CLITYPE_LINETYPE_MAIN})) THEN
   FOR EACH lbOrderCustomer NO-LOCK WHERE   
            lbOrderCustomer.Brand      EQ Syst.Var:gcBrand AND 
            lbOrderCustomer.CustId     EQ Customer.OrgId AND
            lbOrderCustomer.CustIdType EQ Customer.CustIdType AND
            lbOrderCustomer.RowType    EQ 1,
       EACH lbOrder NO-LOCK WHERE
            lbOrder.Brand              EQ Syst.Var:gcBrand AND
            lbOrder.orderid            EQ lbOrderCustomer.Orderid AND
            lbOrder.statuscode         EQ {&ORDER_STATUS_PENDING_MAIN_LINE}:
      RUN Mc/orderinctrl.p(lbOrder.OrderID, 0, TRUE).
   END.


IF CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL} AND 
   NOT fIsMainLineSubActive (Customer.CustIdType,
                             Customer.OrgId)         AND
   NOT fIsMainLineOrderPending (Customer.CustIdType,
                                Customer.OrgId,
                                Order.OrderID)       THEN DO:

   ldLastDate = Func.Common:mLastDayOfMonth(TODAY). 
   
   fTermAdditionalSim(MobSub.MsSeq,
                      MobSub.CLI,
                      MobSub.CustNum,
                     {&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM},
                      ldLastDate,
                     {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                      iiMsRequest,
                      OUTPUT lcError).
   
   IF lcError NE "" THEN
      Func.Common:mWriteMemo("MobSub",
                        STRING(MobSub.MsSeq),
                        MobSub.CustNum,
                       "Additional SIM termination request creation failed",
                        lcError).
    
END.   

/* request handled succesfully */
fReqStatus(2,""). 

RUN Mm/requestaction_sms.p(INPUT MsRequest.MsRequest,
                        INPUT MobSub.CliType,
                        INPUT MsRequest.ReqSource).

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.

PROCEDURE check-order:

   DEF OUTPUT PARAMETER ocError AS CHAR NO-UNDO.

   IF LOCKED(Order) THEN DO:
      ocError = "INFO:Order is in use by another user !".
      RETURN.
   END.
   
   ELSE IF NOT AVAIL Order THEN DO:
      ocError = "Order not found !".
      RETURN.
   END.

   ELSE DO:
      IF order.Statuscode = "7" or 
         order.statuscode = "8" or
         Order.Statuscode = "74" 
      THEN  ocError =  "Order is closed or on hold".
      ELSE IF order.statuscode = "6" THEN
         ocError = "Subscription already created!".
      ELSE IF Order.StatusCode = "5" OR 
             (Order.StatusCode = "3" and Order.CredOk) THEN DO: 
         ocError =  "".
      END.
      ELSE IF Order.StatusCode = "2" THEN DO:
         ocError = "Credit for this order has not "  +
                   "been checked or is not ok !   "  .
      END.
             
      IF lcProductCLI = "" THEN DO:
         ocError = "MSISDN empty".
      END.
             
      DEC(lcProductCLI) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         ocError =  "MSISDN number of order contains " + 
                    "invalid characters! You must "    +
                    "correct it before continuing!".
      END.
           
      IF lcProductCLIType = "" THEN DO:
         ocError =  "CLIType missing !".
      END.

      FIND FIRST clitype WHERE
                 clitype.brand = Syst.Var:gcBrand and
                 clitype.clitype = lcProductCLIType NO-LOCK NO-ERROR.
      IF NOT AVAIL clitype THEN DO:
        ocError =  "Invalid CLIType !".
      END.

      IF ocError ne "" THEN RETURN.
      
   END.
   
END PROCEDURE.

PROCEDURE pCreateCommission:

   DEF VAR ldeSMSStamp AS DEC NO-UNDO. 

   /* If order has a referee then create commission 
       RuleType:
         2 = for referee
         3 = for promoted
   */
   IF Order.Referee NE "" THEN DO:

      /* Referee commission creation */
      FIND bRefMobSub NO-LOCK WHERE
         bRefMobSub.Brand = Syst.Var:gcBrand AND
         bRefMobSub.CLI   = Order.Referee NO-ERROR.
      
      IF AVAIL bRefMobSub THEN DO:
         
         Func.Common:mSplitTS(Order.CrStamp, OUTPUT ldaOrderDate, OUTPUT liOrderTime).

         FIND bRefCust NO-LOCK WHERE 
            bRefCust.Custnum = bRefMobSub.Custnum NO-ERROR.
        
         FIND bRefCLIType NO-LOCK WHERE
            bRefCLIType.Brand = Syst.Var:gcBrand AND
            bRefCLIType.CLIType = bRefMobSub.CLIType.

         liRuleId = 0.
         FOR EACH CORule NO-LOCK WHERE
                  CORule.Brand = Syst.Var:gcBrand AND
                  CORule.RuleType = 2 AND
                  CORule.CoFrom <= ldaOrderDate AND
                  CORule.CoTo   >= ldaOrderDate AND
                  CoRule.PayType = bRefCLIType.PayType
         BY CoRule.Priority:

            IF CORule.CLIType > "" AND
               LOOKUP(lcProductCLIType,CORule.CLIType) = 0 THEN NEXT.

            IF CORule.AllowedDNI > "" AND
               LOOKUP(bRefCust.CustIDType,CORule.AllowedDNI) = 0 THEN NEXT.

            liRuleId = CORule.CoRuleID.
            LEAVE.
         END. 
        
         IF liRuleId > 0 THEN DO:
            
            CREATE COTarg.
            ASSIGN
               COTarg.Brand         = Syst.Var:gcBrand
               CoTarg.CoTargID      = NEXT-VALUE(CommSeq)
               COTarg.CommStatus    = 1
               COTarg.TargType      = "M"
               CoTarg.COTarg        = STRING(bRefMobSub.MsSeq)
               CoTarg.PromotedId    = MobSub.MsSeq
               CoTarg.PromotedCLI   = MobSub.CLI
               CoTarg.OrderId       = Order.OrderId
               CoTarg.CoRuleID      = liRuleId.
               CoTarg.CreatedTS     = Func.Common:mMakeTS().


            IF CoRule.CreationSMS NE "" THEN DO: 
            
               lcSMSText = fGetSMSTxt(
                                   CORule.CreationSMS,
                                   TODAY,
                                   bRefCust.Language,
                                   OUTPUT ldeSMSStamp).
               
               IF lcSMSText > "" THEN DO:
                  lcSMSText = REPLACE(lcSMSText, "#FRIEND", Customer.FirstName).

                  /* Release buffer, otherwise the first parameter is not used.YTS-5279*/
                  RELEASE bReqSub.
                  liRequest = fSMSRequest(bRefMobsub.MsSeq,
                                          9,          /* type=info */
                                          "Free",     /* source of message */
                                          lcSMSText,
                                          ldeSMSStamp,          
                                          "8",
                                          "",
                                          "",
                                          OUTPUT lcResult). 
                        
                  IF liRequest = 0 THEN 
                     Func.Common:mWriteMemo("CoTarg",
                                      STRING(CoTarg.CoTargID),
                                      0,
                                      "COMMISSION",
                                      "Creation SMS failed: " + lcResult).
               END.
            END.
            

         END. 
         ELSE DO:
            
            Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             MobSub.CustNum,
                             "COMMISSION CREATION FAILED",
                             "Commission rule for Referee was not found").
         END.
         
         /* Promoted (=Orderer) commission handling begins */   
         liRuleId = 0.
         FOR EACH CORule NO-LOCK WHERE
                  CORule.Brand = Syst.Var:gcBrand AND
                  CORule.RuleType = 3 AND
                  CORule.CoFrom <= ldaOrderDate AND
                  CORule.CoTo   >= ldaOrderDate AND
                  CoRule.PayType = CLIType.PayType
         BY CoRule.Priority:

            IF CORule.CLIType > "" AND
               LOOKUP(CLIType.CLIType,CORule.CLIType) = 0 THEN NEXT.

            IF CORule.AllowedDNI > "" AND
               LOOKUP(Customer.CustIDType,CORule.AllowedDNI) = 0 THEN NEXT.

            liRuleId = CORule.CoRuleID.
            LEAVE.
         END. 
        
         IF liRuleId > 0 THEN DO:
            
            CREATE COTarg.
            ASSIGN
               COTarg.Brand         = Syst.Var:gcBrand
               CoTarg.CoTargID      = NEXT-VALUE(CommSeq)
               COTarg.CommStatus    = 1
               COTarg.TargType      = "M"
               CoTarg.COTarg        = STRING(MobSub.MsSeq)
               CoTarg.OrderId       = Order.OrderId
               CoTarg.CoRuleID      = liRuleId.
               CoTarg.CreatedTS     = Func.Common:mMakeTS().

         END. 
         ELSE DO:
            
            Func.Common:mWriteMemo("Order",
                           STRING(Order.OrderID),
                           MobSub.CustNum,
                           "COMMISSION CREATION FAILED",
                           "Commission rule for promoted was not found.").
         END.
         /* Promoted commission handling ends */      
      
      END.
      ELSE DO:

         Func.Common:mWriteMemo("Order",
                          STRING(Order.OrderID),
                          MobSub.CustNum,
                          "COMMISSION CREATION FAILED",
                          SUBST("Referee &1 was not found", Order.Referee)).
      END.


   END.

END PROCEDURE. 
