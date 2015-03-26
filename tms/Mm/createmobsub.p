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
  Version ......: yoigo
-------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{msisdn.i}
{forderstamp.i}
{ftaxdata.i}
{ftopup.i}
{fmakemsreq.i}
{msreqfunc.i}
{service.i}
{ftmrlimit.i}
{fsmsreq.i}
{orderfunc.i}
{fsubsterminal.i}
{invoicetarget.i}
{fdss.i}
{orderchk.i}
{fmakesms.i}
{transname.i}
{fbundle.i}
{fbankdata.i}
{create_eventlog.i}

DEF INPUT  PARAMETER iiMSRequest AS INT  NO-UNDO.

DEF VAR lcCharValue    AS CHAR NO-UNDO.
DEF VAR oiCustomer     AS INT  NO-UNDO.

DEF VAR ldAmount       AS DEC  NO-UNDO.
DEF VAR lcError        AS CHAR NO-UNDO. 
DEF VAR liPPRequest    AS INT  NO-UNDO.
DEF VAR lcFatGroup     AS CHAR NO-UNDO. 
DEF VAR lcTaxZone      AS CHAR NO-UNDO.
DEF VAR liReqStatus    AS INT  NO-UNDO.
DEF VAR lcErrorTxt     AS CHAR NO-UNDO.
DEF VAR ldaOrderDate   AS DATE NO-UNDO.
DEF VAR liOrderTime    AS INT  NO-UNDO.
DEF VAR liRuleId       AS INT  NO-UNDO.
DEF VAR liRequest      AS INT  NO-UNDO.
DEF VAR lcResult       AS CHAR NO-UNDO.
DEF VAR llCorporate    AS LOG  NO-UNDO.
DEF VAR llDefBarring   AS LOG  NO-UNDO.
DEF VAR ldeSMSStamp    AS DEC  NO-UNDO.
DEF VAR lcBundleId     AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
DEF VAR liDSSPriMsSeq  AS INT  NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR lcReplacedTxt  AS CHAR NO-UNDO.
DEF VAR lcMandateId AS CHAR NO-UNDO. 
DEF VAR ldaMandateDate AS DATE NO-UNDO. 
DEF VAR ldLastDate     AS DATE NO-UNDO. 

DEF BUFFER bInvCust    FOR Customer.
DEF BUFFER bRefCust    FOR Customer.
DEF BUFFER bRefMobsub  FOR Mobsub.
DEF BUFFER bRefCLIType FOR CLIType.
DEF BUFFER bMsRequest  FOR MsRequest.
DEF BUFFER lbOrder     FOR Order.
DEF BUFFER lbMobSub    FOR MobSub.
DEF BUFFER lbOrderCustomer FOR OrderCustomer.

FIND FIRST MSRequest WHERE 
           MSRequest.MSrequest = iiMSrequest
EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 13 THEN 
   RETURN "ERROR".
   
liReqStatus = MsRequest.ReqStatus.

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
      
FIND FIRST order WHERE
           Order.MSSeq = MSRequest.MSSeq
EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

FIND FIRST Mobsub WHERE 
           Mobsub.MSSEq = MSRequest.Msseq
NO-LOCK NO-ERROR.

IF AVAIL mobsub THEN DO: 
   fReqError("Subscription already exists").
   RETURN. 
END.           

IF AVAIL Order THEN DO:
   
   FIND FIRST Mobsub WHERE 
              Mobsub.CLI = Order.CLI
   NO-LOCK NO-ERROR.

   IF AVAIL mobsub THEN DO: 
      fReqError("Subscription with the same MSISDN already exists").
      RETURN. 
   END.           
END.

FIND FIRST CustTemp NO-LOCK NO-ERROR.

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

IF Order.CLIType EQ "TARJ5" THEN DO:

   RUN air_update_serviceclass.p(Order.CLI,
                                 {&SC_TARJ5_NORMAL},
                                 {&SC_TARJ5_PROMOTIONAL},
                                 TODAY + 31,
                                 OUTPUT lcResult).
   IF lcResult BEGINS "ERROR" THEN DO:
      fReqError("Promotional price activation failed: " + lcResult).
      RETURN.
   END.
END.

RUN createcustomer(INPUT Order.OrderId,1,FALSE,output oiCustomer).

ASSIGN Msrequest.CustNum = oiCustomer.

RUN createcustomer(INPUT Order.OrderId,3,FALSE,output oicustomer).

FIND FIRST CLIType WHERE
           CLIType.CliType = Order.CLIType 
NO-LOCK NO-ERROR.

FIND FIRST imsi WHERE 
           imsi.icc = Order.icc
NO-LOCK NO-ERROR.

CREATE MobSub. 

ASSIGN
   Mobsub.ActivationTS  = fMakeTS()
   MobSub.MsSeq         = Order.MSSeq .

llCorporate = CAN-FIND(OrderCustomer OF Order WHERE
                       OrderCustomer.RowType = 1 AND
                       OrderCustomer.CustIdType = "CIF").

FOR EACH OrderCustomer NO-LOCK WHERE
         OrderCustomer.Brand   = gcBrand   AND
         OrderCustomer.OrderID = Order.OrderID:

   CASE OrderCustomer.RowType:
   WHEN 1 THEN MobSub.AgrCust = OrderCustomer.CustNum.
   WHEN 2 THEN MobSub.InvCust = OrderCustomer.CustNum.
   WHEN 3 THEN MobSub.CustNum = OrderCustomer.CustNum.
   END CASE.
   
   /* Create contact data for corporate customers */
   IF llCorporate AND
      (OrderCustomer.RowType = 1 OR OrderCustomer.Rowtype = 5) THEN DO:
      
      RUN createcustcontact.p(
          Order.OrderId,
          MsRequest.Custnum,
          OrderCustomer.RowType,
          OUTPUT lcError).
   
      /* write possible error to an order memo */
      IF lcError > "" THEN DO:
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "Order",
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

{cr_bscode.i}

   
ASSIGN
   MobSub.CLI              = Order.CLI
   Mobsub.icc              = Order.ICC
   Mobsub.Brand            = Order.Brand 
   Mobsub.MsStatus         = 4
   MobSub.Reseller         = Order.Reseller
   Mobsub.Paytype          = (CLIType.PayType = 2)
   Mobsub.salesman         = Order.salesman 
   Mobsub.MNPChannel       = 2 
   Mobsub.CreationDate     = TODAY
   Mobsub.CLIType          = Order.CliType    
   Mobsub.BillTarget       = Clitype.BillTarget
   Mobsub.SimDelStatus     = 2
   Mobsub.Activationdate   = TODAY
   Mobsub.IDCode           = STRING(RANDOM(0,9999),"9999")
   MobSub.MultiSimID       = Order.MultiSimID
   MobSub.MultiSimType     = Order.MultiSimType
   MobSub.TariffActDate    = TODAY.

fSetOrderStatus(Order.OrderId,"6").  
fMarkOrderStamp(Order.OrderID,
                "Delivery",
                Mobsub.ActivationTS).

IF Avail imsi THEN Mobsub.imsi = IMSI.IMSI.

/* Initial TopUp */
IF MobSub.PayType = TRUE AND Order.Offer = "" THEN 
   RUN topupcamp(MobSub.MsSeq, OUTPUT liPPRequest).

/* additional topup */
lcTaxZone = "".
FOR EACH OrderTopup OF Order NO-LOCK:

   IF lcTaxZone = "" THEN DO:
      FIND bInvCust WHERE bInvCust.CustNum = MobSub.InvCust NO-LOCK.
      lcTaxZone = fRegionTaxZone(bInvCust.Region).
   END.
      
   fCreateTopUpRequest(MobSub.MsSeq,
                       MobSub.CLI,
                       "RefillTRequest",
                       "WEB Order",
                       "RefillTRequest",
                       "994",
                       "Campaign",    /* reference */
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
   Segmentation.SegmentCreation = fMakeTS().

CREATE msowner.
ASSIGN
   mSOwner.CLI       = Mobsub.cli
   MSOwner.CustNum   = Mobsub.CustNum
   MSOwner.MsSeq     = MobSub.MsSeq
   MSOwner.TsBegin   = YEAR (MobSub.ActivationDate) * 10000 +  /* yyyymmdd */
                       month(MobSub.ActivationDate) * 100   +
                       DAY  (MobSub.ActivationDate) +

                      (IF MobSub.ActivationDate > TODAY THEN 0 
                          /* no decimals, i.e we assume that the
                                usage begun on activation DAY AT 00.00.00 */

                       ELSE 
                           /* immediate activation */
                             TIME / 100000)  

   msowner.tsend      = 99999999.99999
   MSOwner.BillTarg   = Mobsub.BillTarg
   MSOwner.Brand      = mobsub.Brand
   Msowner.AgrCust    = Mobsub.AgrCust
   Msowner.InvCust    = Mobsub.InvCust
   MSOwner.IMSI       = MobSub.IMSI
   MSowner.Paytype    = MObsub.Paytype
   MSOwner.clitype    = mobsub.clitype
   MSOWner.clievent   = "C" 
   Msowner.InPortOper = Order.CurrOper .

   IF fGetOrderMandateId(BUFFER Order,
                         OUTPUT lcMandateId,
                         OUTPUT ldaMandateDate)
   THEN ASSIGN
      MsOwner.MandateId   = lcMandateId
      MsOwner.MandateDate = ldaMandateDate.
           
IF llDoEvent THEN fMakeCreateEvent((BUFFER MsOwner:HANDLE),
                                   "",
                                   katun,
                                   "").

fSetOrderStatus(Order.OrderId,"6").

/* default services */
RUN copysp(MobSub.MsSeq,
           MobSub.ActivationDate,
           TRUE,   /* new subs */
           TRUE).  /* silent */

/* clitype spesific fees */
IF AVAIL CliType AND CliType.FeeModel1 > "" THEN DO:
   RUN creasfee (MobSub.CustNum,
                 MobSub.MsSeq,
                 Today,
                 "MobSub",
                 CliType.FeeModel1,
                 1,
                 ?,
                 "",    /* memo   */
                 FALSE,           /* no messages to screen */
                 katun,
                 "SubscriptionCreation",
                 0,
                 "",
                 "",
                 OUTPUT lcCharValue).
END.

/* general fees */
RUN creasfee (MobSub.CustNum,
              MobSub.MsSeq,
              Today,
              "MobSub",
              "MONTHLYFEE",
              1,
              ?,
              "",    /* memo   */
              FALSE,           /* no messages to screen */
              katun,
              "SubscriptionCreation",
              0,
              "",
              "",
              OUTPUT lcCharValue).

/* add initial fees and additional cost (delivery charge) and cash invoice */
IF Order.InvNum > 0 THEN
   RUN cashfee (Order.OrderID,
             1,                     /* action 1=create fees */
             OUTPUT lcCharValue,
             OUTPUT ldAmount,
             OUTPUT lcError).

/* write possible error to an order memo */
IF lcError BEGINS "Error" THEN DO:
   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "Order",
                    STRING(Order.OrderID),
                    MobSub.CustNum,
                    "CASH INVOICE FAILED",
                    lcError).
END.

/* default counter limits */
IF MobSub.PayType = FALSE THEN DO:
   
   fTMRLimit2Subscription(MobSub.MsSeq).

   _fCreateNewInvoiceTarget(
      MobSub.MsSeq,
      MobSub.Custnum,
      MobSub.CLiType,
      OUTPUT lcError).

   IF lcError NE "" THEN 
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.CustNum,
                       "Invoice target creation failed",
                       lcError).
END.

/* terminals, do this before periodical contract creation */
fCreateSubsTerminal(BUFFER Order).
 
/* fatime is granted in a campaign */
IF Order.FatAmount NE 0 OR Order.FtGrp > "" THEN DO:

   IF Order.FtGrp > "" AND 
      CAN-FIND(FIRST FatGroup WHERE
                     FatGroup.Brand = gcBrand AND
                     FatGroup.FtGrp = Order.FtGrp)
   THEN lcFatGroup = Order.FtGrp.
   ELSE lcFatGroup = fCParamC("OrderCampaignFat").
    
   IF lcFatGroup = ? OR lcFatGroup = "" THEN 
      lcError = "FATime group for campaign not defined".
      
   ELSE RUN creafat (MobSub.CustNum,
                     MobSub.MsSeq,
                     lcFatGroup,
                     Order.FatAmount,
                     0,
                     ?,
                     YEAR(MobSub.ActivationDate) * 100 + 
                     MONTH(MobSub.ActivationDate),
                     999999,
                     OUTPUT lcError).

   /* write possible error to an order memo */
   IF lcError > "" THEN DO:
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "Order",
                       STRING(Order.OrderID),
                       MobSub.CustNum,
                       "FATIME CREATION FAILED",
                       lcError).
   END.
END.

/* Create Default shaper, if there is no bundle with subscription */
/* this must be executed before calling orderaction_exec */
FIND FIRST OrderAction WHERE
           OrderAction.Brand    = gcBrand AND
           OrderAction.OrderId  = Order.OrderID AND
           OrderAction.ItemType = "BundleItem" AND
           OrderAction.ItemKey NE {&DSS} NO-LOCK NO-ERROR.
IF NOT AVAIL OrderAction AND
   LOOKUP(MobSub.CLIType,"CONT6,TARJRD1,CONT7,CONT8,CONTS,CONTFF,CONTSF,CONT9,CONT15,CONT24") = 0
THEN DO:
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
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.Custnum,
                       "DEFAULT SHAPER ACTIVATION FAILED",
                       "DEFAULT SHAPER ACTIVATION FAILED").
END. /* IF NOT AVAIL OfferItem AND NOT AVAIL OrderAction THEN DO: */

/* initial topup, fatime, per.contracts from offer */
IF Order.Offer > "" THEN 
   RUN offeritem_exec.p (MobSub.MsSeq,
                       Order.OrderID,
                       MsRequest.MsRequest,
                       "1").

/* activate periodical contracts, service packages etc. */
RUN requestaction_exec.p (MsRequest.MsRequest,
                          MobSub.CLIType,
                          Order.OrderID,
                          MobSub.ActivationTS,
                          MobSub.ActivationTS,
                          TRUE,                   /* create fees */
                          "1",                    /* req.source */
                          {&REQUEST_ACTIONLIST_ALL}).

/* per.contract and service package created with the order */
RUN orderaction_exec (MobSub.MsSeq,
                      Order.OrderID,
                      MsRequest.MsRequest,
                      "1").

/* Add postpaid subs. to DSS group if DSS group is active or ongoing DSS */
IF NOT MobSub.PayType THEN DO:

   ASSIGN
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   lcBundleId = fGetActiveDSSId(INPUT MobSub.CustNum,INPUT fMakeTS()).

   IF lcBundleId > "" OR
      CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.Brand = gcBrand          AND
                     MsRequest.ReqType = {&REQTYPE_DSS} AND
                     MsRequest.Custnum = MobSub.CustNum AND
                     MsRequest.ReqCParam1 = "CREATE"    AND
                     MsRequest.ActStamp <= fMakeTS()    AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"5,6,7,8") > 0)
   THEN DO:
      IF lcBundleId = {&DSS} OR (lcBundleId = "DSS2" AND
         LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN
         RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                            INPUT Mobsub.CLI,
                            INPUT MobSub.CustNum,
                            INPUT "ADD",
                            INPUT "",           /* Optional param list */
                            INPUT MsRequest.MsRequest,
                            INPUT fSecOffSet(fMakeTS(),180), /* 3 mins delay */
                            INPUT MsRequest.ReqSource,
                            INPUT lcBundleId).
   END.
   ELSE IF Order.MultiSimId > 0 AND
           Order.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO: 
      FOR FIRST lbOrder NO-LOCK WHERE
                lbOrder.Brand = gcBrand AND
                lbOrder.MultiSimID = Order.MultiSimId AND
                lbOrder.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                lbOrder.StatusCode = {&ORDER_STATUS_DELIVERED},
          FIRST lbMobSub NO-LOCK WHERE
                lbMobSub.MsSeq = lbOrder.Msseq AND
                lbMobSub.Custnum = lbOrder.Custnum AND
                lbMobSub.PayType = FALSE:
         
         liRequest = fDSSRequest(lbMobSub.MsSeq,
                                 lbMobSub.CustNum,
                                 "CREATE",
                                 "",
                                 "DSS",
                                 lbMobSub.ActivationTS,
                                 {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                                 "",
                                 TRUE, /* create fees */
                                 0,
                                 FALSE,
                                 OUTPUT lcResult).
         IF liRequest = 0 THEN
            /* write possible error to a memo */
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "MobSub",
                             STRING(lbMobSub.MsSeq),
                             lbMobSub.Custnum,
                             "Multi SIM DSS activation failed",
                             lcResult).
      END.
   END.
   ELSE IF LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
      NOT fOngoingDSSAct(MobSub.CustNum) AND
      fIsDSS2Allowed(MobSub.CustNum,MobSub.MsSeq,MobSub.ActivationTS,
                     OUTPUT liDSSPriMsSeq,OUTPUT lcResult) THEN DO:

      FIND FIRST lbMobSub WHERE
                 lbMobSub.MsSeq = liDSSPriMsSeq NO-LOCK NO-ERROR.
      IF AVAIL lbMobSub THEN DO:
         liRequest = fDSSRequest(lbMobSub.MsSeq,
                              lbMobSub.CustNum,
                              "CREATE",
                              "",
                              "DSS2",
                              fSecOffSet(MobSub.ActivationTS,180),
                              {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                              "",
                              TRUE, /* create fees */
                              0,
                              FALSE,
                              OUTPUT lcResult).
         IF liRequest = 0 THEN
            /* write possible error to a memo */
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.Custnum,
                          "DSS2 activation failed",
                          lcResult).
      END.
   END.
END.

RUN pCreateCommission.

/* Default premium barring postpaid (YTD-33) & prepaid (YDR-157) */
llDefBarring = TRUE.
IF TODAY - Customer.CreDate > 60 THEN DO:

   INVSEARCH_LOOP:
   FOR EACH Invoice WHERE
            Invoice.Brand = gcBrand AND
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
      
IF llDefBarring THEN DO:

   RUN barrengine.p(MobSub.MsSeq,
                   "Y_HURP",
                   "1",                 /* source = subscr. creation  */
                   katun,               /* creator */
                   fMakeTS(),           /* activate */
                   "",                  /* sms */
                   OUTPUT lcResult).
   
   liRequest = 0.
   liRequest = INTEGER(lcResult) NO-ERROR. 
   
   IF liRequest = 0 THEN                               
      /* write possible error to a memo */
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.Custnum,
                       "DEFAULT PREMIUM BARRING FAILED",
                       "Y_HURP").
END.
         
/* mark mnp process finished */
IF Order.MNPStatus > 0 THEN DO:
   FIND FIRST MNPProcess WHERE
         MNPProcess.OrderId = Order.OrderId AND
         MNPProcess.MNPType = ({&MNP_TYPE_IN}) AND
         MNPProcess.StatusCode = ({&MNP_ST_ACON}) EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL MNPProcess THEN DO:
      ASSIGN
         MNPProcess.UpdateTS = fMakeTS()
         MNPProcess.StatusCode = {&MNP_ST_APOR}.
      FIND CURRENT Order EXCLUSIVE-LOCK.
      Order.MNPStatus = {&MNP_ST_APOR} + 1.
      RELEASE MNPProcess.
   END.
END.

/* Welcome SMS for all tariffs (Excluding Corporate) */
IF LOOKUP(Customer.category,"20,40,41") = 0 THEN DO:

   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

   lcSMSText = fGetSMSTxt("WelcomeSubs",
                          TODAY,
                          Customer.Language,
                          OUTPUT ldeSMSStamp).   

   IF LOOKUP(Order.CLIType,lcBundleCLITypes) > 0 THEN DO:
      lcBundleId = fGetDataBundleInOrderAction(Order.OrderId,Order.CLIType).
      lcReplacedTxt = fConvBundleToBillItem(lcBundleId).
      lcReplacedTxt = fGetItemName(gcBrand,
                                   "BillItem",
                                   lcReplacedTxt,
                                   Customer.Language,
                                   TODAY).
   END.
   ELSE 
      lcReplacedTxt = fGetItemName(gcBrand,
                                   "CLIType",
                                   Order.CLIType,
                                   Customer.Language,
                                   TODAY).
   
   ASSIGN lcSMSText = REPLACE(lcSMSText,"#CLITYPE",lcReplacedTxt)
          lcSMSText = REPLACE(lcSMSText,"#CLI",Order.CLI).

   IF lcSMSText > "" THEN
      fMakeSchedSMS2(MobSub.CustNum,
                     MobSub.CLI,
                     {&SMSTYPE_INFO},
                     lcSMSText,
                     ldeSMSStamp,
                     "22622",
                     "").
END. /* IF LOOKUP(Customer.category,"20,40,41") = 0 THEN DO: */

IF Order.MultiSimID > 0 THEN DO:

   CASE Order.MultiSimType:
      WHEN {&MULTISIMTYPE_PRIMARY} THEN DO:
         FOR FIRST lbOrder NO-LOCK USE-INDEX MultiSIMId WHERE
                   lbOrder.Brand = gcBrand AND
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
            OrderAction.Brand = gcBrand AND
            OrderAction.OrderId = Order.OrderID AND
            OrderAction.ItemType = "BundleItem" AND
            CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.Brand = gcBrand AND
                           CLIType.CLIType = OrderAction.ItemKey AND
                           CLIType.LineType = {&CLITYPE_LINETYPE_MAIN})) THEN
   FOR EACH lbOrderCustomer NO-LOCK WHERE   
            lbOrderCustomer.Brand      EQ gcBrand AND 
            lbOrderCustomer.CustId     EQ Customer.OrgId AND
            lbOrderCustomer.CustIdType EQ Customer.CustIdType AND
            lbOrderCustomer.RowType    EQ 1,
       EACH lbOrder NO-LOCK WHERE
            lbOrder.Brand              EQ gcBrand AND
            lbOrder.orderid            EQ lbOrderCustomer.Orderid AND
            lbOrder.statuscode         EQ {&ORDER_STATUS_PENDING_MAIN_LINE}:
      RUN orderinctrl.p(lbOrder.OrderID, 0, TRUE).
   END.


IF CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL} AND 
   NOT fIsMainLineSubActive (Customer.CustIdType,
                             Customer.OrgId)         AND
   NOT fIsMainLineOrderPending (Customer.CustIdType,
                                Customer.OrgId,
                                Order.OrderID)       THEN DO:

   ldLastDate = fLastDayOfMonth(TODAY). 
   
   fTermAdditionalSim(MobSub.MsSeq,
                      MobSub.CLI,
                      MobSub.CustNum,
                     {&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM},
                      ldLastDate,
                     {&REQUEST_SOURCE_SUBSCRIPTION_CREATION},
                      iiMsRequest,
                      OUTPUT lcError).
   
   IF lcError NE "" THEN
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                        STRING(MobSub.MsSeq),
                        MobSub.CustNum,
                       "Additional SIM termination request creation failed",
                        lcError).
    
END.   

/* request handled succesfully */
fReqStatus(2,""). 

RUN requestaction_sms.p(INPUT MsRequest.MsRequest,
                        INPUT MobSub.CliType,
                        INPUT MsRequest.ReqSource).

PROCEDURE check-order:

   DEF OUTPUT PARAMETER ocError AS CHAR NO-UNDO.

   IF LOCKED(Order) THEN DO:
      ocError = "INFO:Order is in use by another user !".
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
             
      IF order.CLI    = "" THEN DO:
         ocError = "MSISDN empty".
      END.
             
      DEC(order.cli) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         ocError =  "MSISDN number of order contains " + 
                    "invalid characters! You must "    +
                    "correct it before continuing!".
      END.
           
      IF order.clitype = "" THEN DO:
         ocError =  "CLIType missing !".
      END.

      FIND FIRST clitype WHERE
                 clitype.brand = gcBrand and
                 clitype.clitype = order.clitype NO-LOCK NO-ERROR.
      IF NOT AVAIL clitype THEN DO:
        ocError =  "Invalid CLIType !".
      END.

      FIND FIRST mobsub NO-LOCK WHERE
                 mobsub.cli   = order.cli AND 
                 Mobsub.MSSeq = Order.MSSeq NO-ERROR.

      IF AVAIL mobsub THEN 
      ocError =  "Subscription already exists with CLI " + order.cli .

      IF ocError ne "" THEN RETURN.
      
      FIND FIRST MSISDNNumber WHERE
                 MSISDNNumber.CLI   = Order.CLI
      EXCLUSIVE-LOCK NO-ERROR.
               
      IF NOT AVAIL msisdnNumber THEN DO:
         CREATE MSISDNNumber.
         ASSIGN MSISDNNumber.CLI = Order.CLI.
      END.
      
      FIND FIRST MSISDN WHERE 
                 MSISDN.Brand    = gcBrand    AND 
                 MSISDN.CLI      = Order.CLI  AND 
                 MSISDN.ValidTo >= fMakeTS() 
      EXCLUSIVE-LOCK NO-ERROR.
      
      IF NOT AVAIL MSISDN THEN DO:

         CREATE MSISDN.
         ASSIGN
            MSISDN.CLI         = order.CLI
            MSISDN.Brand       = gcBrand
            MSISDN.Stat        = 3.

         ASSIGN 
            MSISDN.ValidFrom   = fMakeTS()
            MSISDN.ActionDate  = Today.
      END.

      ELSE DO:
         
         fMakeMsidnHistory(INPUT RECID(MSISDN)).
  
         MSISDN.Stat        = 3.
      END.
   
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
         bRefMobSub.Brand = gcBrand AND
         bRefMobSub.CLI   = Order.Referee NO-ERROR.
      
      IF AVAIL bRefMobSub THEN DO:
         
         fSplitTS(Order.CrStamp, OUTPUT ldaOrderDate, OUTPUT liOrderTime).

         FIND bRefCust NO-LOCK WHERE 
            bRefCust.Custnum = bRefMobSub.Custnum NO-ERROR.
        
         FIND bRefCLIType NO-LOCK WHERE
            bRefCLIType.Brand = gcBrand AND
            bRefCLIType.CLIType = bRefMobSub.CLIType.

         liRuleId = 0.
         FOR EACH CORule NO-LOCK WHERE
                  CORule.Brand = gcBrand AND
                  CORule.RuleType = 2 AND
                  CORule.CoFrom <= ldaOrderDate AND
                  CORule.CoTo   >= ldaOrderDate AND
                  CoRule.PayType = bRefCLIType.PayType
         BY CoRule.Priority:

            IF CORule.CLIType > "" AND
               LOOKUP(CLIType.CLIType,CORule.CLIType) = 0 THEN NEXT.

            IF CORule.AllowedDNI > "" AND
               LOOKUP(bRefCust.CustIDType,CORule.AllowedDNI) = 0 THEN NEXT.

            liRuleId = CORule.CoRuleID.
            LEAVE.
         END. 
        
         IF liRuleId > 0 THEN DO:
            
            CREATE COTarg.
            ASSIGN
               COTarg.Brand         = gcBrand
               CoTarg.CoTargID      = NEXT-VALUE(CommSeq)
               COTarg.CommStatus    = 1
               COTarg.TargType      = "M"
               CoTarg.COTarg        = STRING(bRefMobSub.MsSeq)
               CoTarg.PromotedId    = MobSub.MsSeq
               CoTarg.PromotedCLI   = MobSub.CLI
               CoTarg.OrderId       = Order.OrderId
               CoTarg.CoRuleID      = liRuleId.
               CoTarg.CreatedTS     = fMakeTS().


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
                     DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                      "CoTarg",
                                      STRING(CoTarg.CoTargID),
                                      0,
                                      "COMMISSION",
                                      "Creation SMS failed: " + lcResult).
               END.
            END.
            

         END. 
         ELSE DO:
            
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "Order",
                             STRING(Order.OrderID),
                             MobSub.CustNum,
                             "COMMISSION CREATION FAILED",
                             "Commission rule for Referee was not found").
         END.
         
         /* Promoted (=Orderer) commission handling begins */   
         liRuleId = 0.
         FOR EACH CORule NO-LOCK WHERE
                  CORule.Brand = gcBrand AND
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
               COTarg.Brand         = gcBrand
               CoTarg.CoTargID      = NEXT-VALUE(CommSeq)
               COTarg.CommStatus    = 1
               COTarg.TargType      = "M"
               CoTarg.COTarg        = STRING(MobSub.MsSeq)
               CoTarg.OrderId       = Order.OrderId
               CoTarg.CoRuleID      = liRuleId.
               CoTarg.CreatedTS     = fMakeTS().

         END. 
         ELSE DO:
            
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                           "Order",
                           STRING(Order.OrderID),
                           MobSub.CustNum,
                           "COMMISSION CREATION FAILED",
                           "Commission rule for promoted was not found.").
         END.
         /* Promoted commission handling ends */      
      
      END.
      ELSE DO:

         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "Order",
                          STRING(Order.OrderID),
                          MobSub.CustNum,
                          "COMMISSION CREATION FAILED",
                          SUBST("Referee &1 was not found", Order.Referee)).
      END.


   END.

END PROCEDURE. 
